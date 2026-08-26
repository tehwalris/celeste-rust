// GENERATED from a TRACED frame (shape 7). Do not edit.
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
pub const SHAPE: u64 = 7233084974395260882;

/// (path, kind) - resolved against a block at bind time.
pub const UNI_SLOTS: &[(&str, &str)] = &[
    ("objects[0].hitbox.h", "num"),
    ("objects[0].hitbox.w", "num"),
    ("objects[0].hitbox.x", "num"),
    ("objects[0].hitbox.y", "num"),
    ("objects[1].hitbox.h", "num"),
    ("objects[1].hitbox.w", "num"),
    ("objects[1].hitbox.x", "num"),
    ("objects[1].hitbox.y", "num"),
];

/// Block-uniform inputs, in `UNI_SLOTS` order.
pub struct Uni {
    pub c294: P8,
    pub c295: P8,
    pub c296: P8,
    pub c297: P8,
    pub c304: P8,
    pub c305: P8,
    pub c306: P8,
    pub c307: P8,
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
    ("objects[0].flip.x", "bool"),
    ("objects[0].flip.y", "bool"),
    ("objects[0].rem.x", "num"),
    ("objects[0].rem.y", "num"),
    ("objects[0].solids", "bool"),
    ("objects[0].spd.x", "num"),
    ("objects[0].spd.y", "num"),
    ("objects[0].spr", "num"),
    ("objects[0].x", "num"),
    ("objects[0].y", "num"),
    ("objects[1].collideable", "bool"),
    ("objects[1].delay", "num"),
    ("objects[1].flip.x", "bool"),
    ("objects[1].flip.y", "bool"),
    ("objects[1].rem.x", "num"),
    ("objects[1].rem.y", "num"),
    ("objects[1].solids", "bool"),
    ("objects[1].spd.x", "num"),
    ("objects[1].spd.y", "num"),
    ("objects[1].spr", "num"),
    ("objects[1].state", "num"),
    ("objects[1].target.x", "num"),
    ("objects[1].target.y", "num"),
    ("objects[1].x", "num"),
    ("objects[1].y", "num"),
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
    pub c234: u16,
    pub c292: u16,
    pub c293: u16,
    pub c298: ZN,
    pub c299: ZN,
    pub c243: u16,
    pub c300: ZN,
    pub c301: ZN,
    pub c245: ZN,
    pub c247: ZN,
    pub c248: ZN,
    pub c251: u16,
    pub c252: ZN,
    pub c302: u16,
    pub c303: u16,
    pub c308: ZN,
    pub c309: ZN,
    pub c261: u16,
    pub c310: ZN,
    pub c311: ZN,
    pub c263: ZN,
    pub c264: ZN,
    pub c312: ZN,
    pub c313: ZN,
    pub c267: ZN,
    pub c268: ZN,
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
    pub c234: u32,
    pub c292: u32,
    pub c293: u32,
    pub c298: u32,
    pub c299: u32,
    pub c243: u32,
    pub c300: u32,
    pub c301: u32,
    pub c245: u32,
    pub c247: u32,
    pub c248: u32,
    pub c251: u32,
    pub c252: u32,
    pub c302: u32,
    pub c303: u32,
    pub c308: u32,
    pub c309: u32,
    pub c261: u32,
    pub c310: u32,
    pub c311: u32,
    pub c263: u32,
    pub c264: u32,
    pub c312: u32,
    pub c313: u32,
    pub c267: u32,
    pub c268: u32,
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
        c294: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c295: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c296: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c297: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c304: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c305: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c306: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c307: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
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
        c234: cell("objects[0].collideable")?,
        c292: cell("objects[0].flip.x")?,
        c293: cell("objects[0].flip.y")?,
        c298: cell("objects[0].rem.x")?,
        c299: cell("objects[0].rem.y")?,
        c243: cell("objects[0].solids")?,
        c300: cell("objects[0].spd.x")?,
        c301: cell("objects[0].spd.y")?,
        c245: cell("objects[0].spr")?,
        c247: cell("objects[0].x")?,
        c248: cell("objects[0].y")?,
        c251: cell("objects[1].collideable")?,
        c252: cell("objects[1].delay")?,
        c302: cell("objects[1].flip.x")?,
        c303: cell("objects[1].flip.y")?,
        c308: cell("objects[1].rem.x")?,
        c309: cell("objects[1].rem.y")?,
        c261: cell("objects[1].solids")?,
        c310: cell("objects[1].spd.x")?,
        c311: cell("objects[1].spd.y")?,
        c263: cell("objects[1].spr")?,
        c264: cell("objects[1].state")?,
        c312: cell("objects[1].target.x")?,
        c313: cell("objects[1].target.y")?,
        c267: cell("objects[1].x")?,
        c268: cell("objects[1].y")?,
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
        c292: match &b.cols[s.c292 as usize] {
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
        c293: match &b.cols[s.c293 as usize] {
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
        c298: match &b.cols[s.c298 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c299: match &b.cols[s.c299 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c243: match &b.cols[s.c243 as usize] {
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
        c300: match &b.cols[s.c300 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c301: match &b.cols[s.c301 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c245: match &b.cols[s.c245 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c247: match &b.cols[s.c247 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c248: match &b.cols[s.c248 as usize] {
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
        c252: match &b.cols[s.c252 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c302: match &b.cols[s.c302 as usize] {
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
        c303: match &b.cols[s.c303 as usize] {
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
        c308: match &b.cols[s.c308 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c309: match &b.cols[s.c309 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c261: match &b.cols[s.c261 as usize] {
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
        c310: match &b.cols[s.c310 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c311: match &b.cols[s.c311 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c263: match &b.cols[s.c263 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c264: match &b.cols[s.c264 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c312: match &b.cols[s.c312 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c313: match &b.cols[s.c313 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c267: match &b.cols[s.c267 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c268: match &b.cols[s.c268 as usize] {
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
    (175, "balloon.tile"),
    (202, "big_chest.tile"),
    (194, "chest.if_not_fruit"),
    (196, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (188, "fake_wall.if_not_fruit"),
    (189, "fake_wall.tile"),
    (178, "fall_floor.tile"),
    (184, "fly_fruit.if_not_fruit"),
    (186, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (180, "fruit.if_not_fruit"),
    (182, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (191, "key.if_not_fruit"),
    (192, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (234, "objects[0].collideable"),
    (298, "objects[0].flip.x"),
    (299, "objects[0].flip.y"),
    (300, "objects[0].hitbox.h"),
    (301, "objects[0].hitbox.w"),
    (302, "objects[0].hitbox.x"),
    (303, "objects[0].hitbox.y"),
    (304, "objects[0].rem.x"),
    (305, "objects[0].rem.y"),
    (243, "objects[0].solids"),
    (306, "objects[0].spd.x"),
    (307, "objects[0].spd.y"),
    (245, "objects[0].spr"),
    (247, "objects[0].x"),
    (248, "objects[0].y"),
    (251, "objects[1].collideable"),
    (308, "objects[1].dash_accel.x"),
    (309, "objects[1].dash_accel.y"),
    (253, "objects[1].dash_effect_time"),
    (310, "objects[1].dash_target.x"),
    (311, "objects[1].dash_target.y"),
    (255, "objects[1].dash_time"),
    (256, "objects[1].djump"),
    (312, "objects[1].flip.x"),
    (313, "objects[1].flip.y"),
    (258, "objects[1].grace"),
    (314, "objects[1].hitbox.h"),
    (315, "objects[1].hitbox.w"),
    (316, "objects[1].hitbox.x"),
    (317, "objects[1].hitbox.y"),
    (265, "objects[1].p_dash"),
    (266, "objects[1].p_jump"),
    (318, "objects[1].rem.x"),
    (319, "objects[1].rem.y"),
    (268, "objects[1].solids"),
    (320, "objects[1].spd.x"),
    (321, "objects[1].spd.y"),
    (272, "objects[1].x"),
    (273, "objects[1].y"),
    (43, "pause_player"),
    (157, "player_spawn.tile"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (172, "spring.tile"),
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
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 171), (8, 172), (6, 173)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 174), (8, 175), (6, 176)]),
    SCell::Obj(&[(5, 177), (8, 178), (6, 179)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 180), (5, 181), (8, 182), (6, 183)]),
    SCell::Obj(&[(9, 184), (5, 185), (8, 186), (6, 187)]),
    SCell::Obj(&[(9, 188), (8, 189), (6, 190)]),
    SCell::Obj(&[(9, 191), (8, 192), (6, 193)]),
    SCell::Obj(&[(9, 194), (5, 195), (8, 196), (6, 197)]),
    SCell::Obj(&[(5, 198), (6, 199)]),
    SCell::Obj(&[(7, 200), (5, 201), (8, 202)]),
    SCell::Obj(&[(7, 203), (5, 204)]),
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
    SCell::Obj(&[(21, 232), (20, 233), (11, 234), (14, 235), (15, 236), (19, 237), (18, 238), (22, 239), (23, 240), (24, 241), (4, 242), (12, 243), (3, 244), (13, 245), (0, 246), (1, 247), (2, 248)]),
    SCell::Obj(&[(21, 249), (20, 250), (11, 251), (28, 252), (33, 253), (27, 254), (26, 255), (30, 256), (14, 257), (29, 258), (15, 259), (19, 260), (18, 261), (22, 262), (23, 263), (24, 264), (32, 265), (31, 266), (4, 267), (12, 268), (3, 269), (13, 270), (0, 271), (1, 272), (2, 273)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 298), (2, 299)]),
    SCell::Obj(&[(17, 300), (16, 301), (1, 302), (2, 303)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 304), (2, 305)]),
    SCell::Obj(&[(1, 306), (2, 307)]),
    SCell::Clo(26, &[206]),
    SCell::Clo(25, &[206]),
    SCell::Obj(&[(1, 308), (2, 309)]),
    SCell::Obj(&[(1, 310), (2, 311)]),
    SCell::Obj(&[(1, 312), (2, 313)]),
    SCell::Obj(&[(17, 314), (16, 315), (1, 316), (2, 317)]),
    SCell::Clo(24, &[206]),
    SCell::Clo(23, &[206]),
    SCell::Clo(27, &[206]),
    SCell::Clo(28, &[206]),
    SCell::Clo(29, &[206]),
    SCell::Obj(&[(1, 318), (2, 319)]),
    SCell::Obj(&[(1, 320), (2, 321)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 205),
    (152, 206),
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
    (171, 212),
    (173, 213),
    (174, 214),
    (176, 215),
    (177, 216),
    (179, 217),
    (181, 218),
    (183, 219),
    (185, 220),
    (187, 221),
    (190, 222),
    (193, 223),
    (195, 224),
    (197, 225),
    (198, 226),
    (199, 227),
    (200, 228),
    (201, 229),
    (203, 230),
    (204, 231),
    (232, 274),
    (233, 275),
    (235, 276),
    (236, 277),
    (237, 278),
    (238, 279),
    (239, 280),
    (240, 281),
    (241, 282),
    (242, 283),
    (244, 284),
    (246, 123),
    (249, 285),
    (250, 286),
    (252, 287),
    (254, 288),
    (257, 289),
    (259, 290),
    (260, 291),
    (261, 292),
    (262, 293),
    (263, 294),
    (264, 295),
    (267, 296),
    (269, 297),
    (271, 93),
];

/// Outcome 0's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared0 {
    pub c39: ZN,
    pub c273: ZN,
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
    pub c308: ZN,
    pub c309: ZN,
    pub c253: ZN,
    pub c310: ZN,
    pub c311: ZN,
    pub c255: ZN,
    pub c256: ZN,
    pub c312: ZB,
    pub c258: ZN,
    pub c265: ZB,
    pub c266: ZB,
    pub c320: ZN,
    pub c321: ZN,
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
    (175, "balloon.tile"),
    (202, "big_chest.tile"),
    (194, "chest.if_not_fruit"),
    (196, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (188, "fake_wall.if_not_fruit"),
    (189, "fake_wall.tile"),
    (178, "fall_floor.tile"),
    (184, "fly_fruit.if_not_fruit"),
    (186, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (180, "fruit.if_not_fruit"),
    (182, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (191, "key.if_not_fruit"),
    (192, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (233, "objects[0].collideable"),
    (259, "objects[0].flip.x"),
    (260, "objects[0].flip.y"),
    (261, "objects[0].hitbox.h"),
    (262, "objects[0].hitbox.w"),
    (263, "objects[0].hitbox.x"),
    (264, "objects[0].hitbox.y"),
    (265, "objects[0].rem.x"),
    (266, "objects[0].rem.y"),
    (242, "objects[0].solids"),
    (267, "objects[0].spd.x"),
    (268, "objects[0].spd.y"),
    (244, "objects[0].spr"),
    (246, "objects[0].x"),
    (247, "objects[0].y"),
    (43, "pause_player"),
    (157, "player_spawn.tile"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (172, "spring.tile"),
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
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 171), (8, 172), (6, 173)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 174), (8, 175), (6, 176)]),
    SCell::Obj(&[(5, 177), (8, 178), (6, 179)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 180), (5, 181), (8, 182), (6, 183)]),
    SCell::Obj(&[(9, 184), (5, 185), (8, 186), (6, 187)]),
    SCell::Obj(&[(9, 188), (8, 189), (6, 190)]),
    SCell::Obj(&[(9, 191), (8, 192), (6, 193)]),
    SCell::Obj(&[(9, 194), (5, 195), (8, 196), (6, 197)]),
    SCell::Obj(&[(5, 198), (6, 199)]),
    SCell::Obj(&[(7, 200), (5, 201), (8, 202)]),
    SCell::Obj(&[(7, 203), (5, 204)]),
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
    SCell::Obj(&[(21, 231), (20, 232), (11, 233), (14, 234), (15, 235), (19, 236), (18, 237), (22, 238), (23, 239), (24, 240), (4, 241), (12, 242), (3, 243), (13, 244), (0, 245), (1, 246), (2, 247)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 259), (2, 260)]),
    SCell::Obj(&[(17, 261), (16, 262), (1, 263), (2, 264)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 265), (2, 266)]),
    SCell::Obj(&[(1, 267), (2, 268)]),
    SCell::Val,
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
    (151, 205),
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
    (171, 211),
    (173, 212),
    (174, 213),
    (176, 214),
    (177, 215),
    (179, 216),
    (181, 217),
    (183, 218),
    (185, 219),
    (187, 220),
    (190, 221),
    (193, 222),
    (195, 223),
    (197, 224),
    (198, 225),
    (199, 226),
    (200, 227),
    (201, 228),
    (203, 229),
    (204, 230),
    (231, 248),
    (232, 249),
    (234, 250),
    (235, 251),
    (236, 252),
    (237, 253),
    (238, 254),
    (239, 255),
    (240, 256),
    (241, 257),
    (243, 258),
    (245, 123),
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
    (175, "balloon.tile"),
    (202, "big_chest.tile"),
    (194, "chest.if_not_fruit"),
    (196, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (188, "fake_wall.if_not_fruit"),
    (189, "fake_wall.tile"),
    (178, "fall_floor.tile"),
    (184, "fly_fruit.if_not_fruit"),
    (186, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (180, "fruit.if_not_fruit"),
    (182, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (191, "key.if_not_fruit"),
    (192, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (233, "objects[0].collideable"),
    (234, "objects[0].delay"),
    (263, "objects[0].flip.x"),
    (264, "objects[0].flip.y"),
    (265, "objects[0].hitbox.h"),
    (266, "objects[0].hitbox.w"),
    (267, "objects[0].hitbox.x"),
    (268, "objects[0].hitbox.y"),
    (269, "objects[0].rem.x"),
    (270, "objects[0].rem.y"),
    (243, "objects[0].solids"),
    (271, "objects[0].spd.x"),
    (272, "objects[0].spd.y"),
    (245, "objects[0].spr"),
    (246, "objects[0].state"),
    (273, "objects[0].target.x"),
    (274, "objects[0].target.y"),
    (157, "objects[0].type.tile"),
    (249, "objects[0].x"),
    (250, "objects[0].y"),
    (43, "pause_player"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (172, "spring.tile"),
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
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 171), (8, 172), (6, 173)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 174), (8, 175), (6, 176)]),
    SCell::Obj(&[(5, 177), (8, 178), (6, 179)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 180), (5, 181), (8, 182), (6, 183)]),
    SCell::Obj(&[(9, 184), (5, 185), (8, 186), (6, 187)]),
    SCell::Obj(&[(9, 188), (8, 189), (6, 190)]),
    SCell::Obj(&[(9, 191), (8, 192), (6, 193)]),
    SCell::Obj(&[(9, 194), (5, 195), (8, 196), (6, 197)]),
    SCell::Obj(&[(5, 198), (6, 199)]),
    SCell::Obj(&[(7, 200), (5, 201), (8, 202)]),
    SCell::Obj(&[(7, 203), (5, 204)]),
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
    SCell::Obj(&[(21, 231), (20, 232), (11, 233), (35, 234), (14, 235), (15, 236), (19, 237), (18, 238), (22, 239), (23, 240), (24, 241), (4, 242), (12, 243), (3, 244), (13, 245), (25, 246), (34, 247), (0, 248), (1, 249), (2, 250)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 263), (2, 264)]),
    SCell::Obj(&[(17, 265), (16, 266), (1, 267), (2, 268)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 269), (2, 270)]),
    SCell::Obj(&[(1, 271), (2, 272)]),
    SCell::Obj(&[(1, 273), (2, 274)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 205),
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
    (171, 211),
    (173, 212),
    (174, 213),
    (176, 214),
    (177, 215),
    (179, 216),
    (181, 217),
    (183, 218),
    (185, 219),
    (187, 220),
    (190, 221),
    (193, 222),
    (195, 223),
    (197, 224),
    (198, 225),
    (199, 226),
    (200, 227),
    (201, 228),
    (203, 229),
    (204, 230),
    (231, 251),
    (232, 252),
    (235, 253),
    (236, 254),
    (237, 255),
    (238, 256),
    (239, 257),
    (240, 258),
    (241, 259),
    (242, 260),
    (244, 261),
    (247, 262),
    (248, 94),
];

/// Outcome 2's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared2 {
    pub c39: ZN,
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
    (175, "balloon.tile"),
    (202, "big_chest.tile"),
    (194, "chest.if_not_fruit"),
    (196, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (188, "fake_wall.if_not_fruit"),
    (189, "fake_wall.tile"),
    (178, "fall_floor.tile"),
    (184, "fly_fruit.if_not_fruit"),
    (186, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (180, "fruit.if_not_fruit"),
    (182, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (191, "key.if_not_fruit"),
    (192, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (234, "objects[0].collideable"),
    (292, "objects[0].flip.x"),
    (293, "objects[0].flip.y"),
    (294, "objects[0].hitbox.h"),
    (295, "objects[0].hitbox.w"),
    (296, "objects[0].hitbox.x"),
    (297, "objects[0].hitbox.y"),
    (298, "objects[0].rem.x"),
    (299, "objects[0].rem.y"),
    (243, "objects[0].solids"),
    (300, "objects[0].spd.x"),
    (301, "objects[0].spd.y"),
    (245, "objects[0].spr"),
    (247, "objects[0].x"),
    (248, "objects[0].y"),
    (251, "objects[1].collideable"),
    (252, "objects[1].delay"),
    (302, "objects[1].flip.x"),
    (303, "objects[1].flip.y"),
    (304, "objects[1].hitbox.h"),
    (305, "objects[1].hitbox.w"),
    (306, "objects[1].hitbox.x"),
    (307, "objects[1].hitbox.y"),
    (308, "objects[1].rem.x"),
    (309, "objects[1].rem.y"),
    (261, "objects[1].solids"),
    (310, "objects[1].spd.x"),
    (311, "objects[1].spd.y"),
    (263, "objects[1].spr"),
    (264, "objects[1].state"),
    (312, "objects[1].target.x"),
    (313, "objects[1].target.y"),
    (157, "objects[1].type.tile"),
    (267, "objects[1].x"),
    (268, "objects[1].y"),
    (43, "pause_player"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (172, "spring.tile"),
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
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 171), (8, 172), (6, 173)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 174), (8, 175), (6, 176)]),
    SCell::Obj(&[(5, 177), (8, 178), (6, 179)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 180), (5, 181), (8, 182), (6, 183)]),
    SCell::Obj(&[(9, 184), (5, 185), (8, 186), (6, 187)]),
    SCell::Obj(&[(9, 188), (8, 189), (6, 190)]),
    SCell::Obj(&[(9, 191), (8, 192), (6, 193)]),
    SCell::Obj(&[(9, 194), (5, 195), (8, 196), (6, 197)]),
    SCell::Obj(&[(5, 198), (6, 199)]),
    SCell::Obj(&[(7, 200), (5, 201), (8, 202)]),
    SCell::Obj(&[(7, 203), (5, 204)]),
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
    SCell::Obj(&[(21, 232), (20, 233), (11, 234), (14, 235), (15, 236), (19, 237), (18, 238), (22, 239), (23, 240), (24, 241), (4, 242), (12, 243), (3, 244), (13, 245), (0, 246), (1, 247), (2, 248)]),
    SCell::Obj(&[(21, 249), (20, 250), (11, 251), (35, 252), (14, 253), (15, 254), (19, 255), (18, 256), (22, 257), (23, 258), (24, 259), (4, 260), (12, 261), (3, 262), (13, 263), (25, 264), (34, 265), (0, 266), (1, 267), (2, 268)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 292), (2, 293)]),
    SCell::Obj(&[(17, 294), (16, 295), (1, 296), (2, 297)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 298), (2, 299)]),
    SCell::Obj(&[(1, 300), (2, 301)]),
    SCell::Clo(26, &[206]),
    SCell::Clo(25, &[206]),
    SCell::Obj(&[(1, 302), (2, 303)]),
    SCell::Obj(&[(17, 304), (16, 305), (1, 306), (2, 307)]),
    SCell::Clo(24, &[206]),
    SCell::Clo(23, &[206]),
    SCell::Clo(27, &[206]),
    SCell::Clo(28, &[206]),
    SCell::Clo(29, &[206]),
    SCell::Obj(&[(1, 308), (2, 309)]),
    SCell::Obj(&[(1, 310), (2, 311)]),
    SCell::Obj(&[(1, 312), (2, 313)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 205),
    (152, 206),
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
    (171, 212),
    (173, 213),
    (174, 214),
    (176, 215),
    (177, 216),
    (179, 217),
    (181, 218),
    (183, 219),
    (185, 220),
    (187, 221),
    (190, 222),
    (193, 223),
    (195, 224),
    (197, 225),
    (198, 226),
    (199, 227),
    (200, 228),
    (201, 229),
    (203, 230),
    (204, 231),
    (232, 269),
    (233, 270),
    (235, 271),
    (236, 272),
    (237, 273),
    (238, 274),
    (239, 275),
    (240, 276),
    (241, 277),
    (242, 278),
    (244, 279),
    (246, 123),
    (249, 280),
    (250, 281),
    (253, 282),
    (254, 283),
    (255, 284),
    (256, 285),
    (257, 286),
    (258, 287),
    (259, 288),
    (260, 289),
    (262, 290),
    (265, 291),
    (266, 94),
];

/// Outcome 3's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared3 {
    pub c39: ZN,
    pub c20: ZN,
    pub c294: ZN,
    pub c295: ZN,
    pub c252: ZN,
    pub c309: ZN,
    pub c311: ZN,
    pub c263: ZN,
    pub c264: ZN,
    pub c268: ZN,
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
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[20] = Col::N(Vec::new());
    b.cols[180] = Col::U(AV::Bool(true));
    b.cols[182] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[234] = Col::U(AV::Bool(true));
    b.cols[298] = Col::U(AV::Bool(false));
    b.cols[299] = Col::U(AV::Bool(false));
    b.cols[300] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[301] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[302] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[303] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[304] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[305] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Bool(true));
    b.cols[306] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[307] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[245] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[247] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[248] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[308] = Col::N(Vec::new());
    b.cols[309] = Col::N(Vec::new());
    b.cols[253] = Col::N(Vec::new());
    b.cols[310] = Col::N(Vec::new());
    b.cols[311] = Col::N(Vec::new());
    b.cols[255] = Col::N(Vec::new());
    b.cols[256] = Col::N(Vec::new());
    b.cols[312] = Col::V(Vec::new());
    b.cols[313] = Col::U(AV::Bool(false));
    b.cols[258] = Col::N(Vec::new());
    b.cols[314] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[315] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[316] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[317] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[265] = Col::V(Vec::new());
    b.cols[266] = Col::V(Vec::new());
    b.cols[318] = Col::U(AV::Ival(P8::from_raw(-32768i32), P8::from_raw(32767i32)));
    b.cols[319] = Col::U(AV::Ival(P8::from_raw(-32768i32), P8::from_raw(32767i32)));
    b.cols[268] = Col::U(AV::Bool(true));
    b.cols[320] = Col::N(Vec::new());
    b.cols[321] = Col::N(Vec::new());
    b.cols[272] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[273] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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
        if let Col::N(v) = &mut acc.cols[308] { v.push(kv.c308.lane(i)); }
        if let Col::N(v) = &mut acc.cols[309] { v.push(kv.c309.lane(i)); }
        if let Col::N(v) = &mut acc.cols[253] { v.push(kv.c253.lane(i)); }
        if let Col::N(v) = &mut acc.cols[310] { v.push(kv.c310.lane(i)); }
        if let Col::N(v) = &mut acc.cols[311] { v.push(kv.c311.lane(i)); }
        if let Col::N(v) = &mut acc.cols[255] { v.push(kv.c255.lane(i)); }
        if let Col::N(v) = &mut acc.cols[256] { v.push(kv.c256.lane(i)); }
        if let Col::V(v) = &mut acc.cols[312] {
            v.push(if kv.c312.known & (1 << i) != 0 {
                AV::Bool(kv.c312.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[258] { v.push(kv.c258.lane(i)); }
        if let Col::V(v) = &mut acc.cols[265] {
            v.push(if kv.c265.known & (1 << i) != 0 {
                AV::Bool(kv.c265.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[266] {
            v.push(if kv.c266.known & (1 << i) != 0 {
                AV::Bool(kv.c266.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[320] { v.push(kv.c320.lane(i)); }
        if let Col::N(v) = &mut acc.cols[321] { v.push(kv.c321.lane(i)); }
        if let Col::N(v) = &mut acc.cols[273] { v.push(sh.c273.lane(i)); }
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
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[20] = Col::N(Vec::new());
    b.cols[180] = Col::U(AV::Bool(true));
    b.cols[182] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[233] = Col::U(AV::Bool(true));
    b.cols[259] = Col::U(AV::Bool(false));
    b.cols[260] = Col::U(AV::Bool(false));
    b.cols[261] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[262] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[263] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[264] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[265] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[266] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[242] = Col::U(AV::Bool(true));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[244] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[246] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[247] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[20] = Col::N(Vec::new());
    b.cols[180] = Col::U(AV::Bool(true));
    b.cols[182] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[233] = Col::U(AV::Bool(true));
    b.cols[234] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[263] = Col::U(AV::Bool(false));
    b.cols[264] = Col::U(AV::Bool(false));
    b.cols[265] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[266] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Bool(false));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(-262144i32)));
    b.cols[245] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[246] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[249] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(8388608i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[20] = Col::N(Vec::new());
    b.cols[180] = Col::U(AV::Bool(true));
    b.cols[182] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[234] = Col::U(AV::Bool(true));
    b.cols[292] = Col::U(AV::Bool(false));
    b.cols[293] = Col::U(AV::Bool(false));
    b.cols[294] = Col::N(Vec::new());
    b.cols[295] = Col::N(Vec::new());
    b.cols[296] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[297] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[298] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[299] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Bool(true));
    b.cols[300] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[301] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[245] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[247] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[248] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[252] = Col::N(Vec::new());
    b.cols[302] = Col::U(AV::Bool(false));
    b.cols[303] = Col::U(AV::Bool(false));
    b.cols[304] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[305] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[306] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[307] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[308] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[309] = Col::N(Vec::new());
    b.cols[261] = Col::U(AV::Bool(false));
    b.cols[310] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[311] = Col::N(Vec::new());
    b.cols[263] = Col::N(Vec::new());
    b.cols[264] = Col::N(Vec::new());
    b.cols[312] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[313] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[268] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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
        if let Col::N(v) = &mut acc.cols[294] { v.push(sh.c294.lane(i)); }
        if let Col::N(v) = &mut acc.cols[295] { v.push(sh.c295.lane(i)); }
        if let Col::N(v) = &mut acc.cols[252] { v.push(sh.c252.lane(i)); }
        if let Col::N(v) = &mut acc.cols[309] { v.push(sh.c309.lane(i)); }
        if let Col::N(v) = &mut acc.cols[311] { v.push(sh.c311.lane(i)); }
        if let Col::N(v) = &mut acc.cols[263] { v.push(sh.c263.lane(i)); }
        if let Col::N(v) = &mut acc.cols[264] { v.push(sh.c264.lane(i)); }
        if let Col::N(v) = &mut acc.cols[268] { v.push(sh.c268.lane(i)); }
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
    let r_c234: ZB = ZB { val: rin.c234, known: ALL };
    let r_c243: ZB = ZB { val: rin.c243, known: ALL };
    let r_c245: ZN = rin.c245;
    let r_c247: ZN = rin.c247;
    let r_c248: ZN = rin.c248;
    let r_c251: ZB = ZB { val: rin.c251, known: ALL };
    let r_c252: ZN = rin.c252;
    let r_c261: ZB = ZB { val: rin.c261, known: ALL };
    let r_c263: ZN = rin.c263;
    let r_c264: ZN = rin.c264;
    let r_c267: ZN = rin.c267;
    let r_c268: ZN = rin.c268;
    let r_c292: ZB = ZB { val: rin.c292, known: ALL };
    let r_c293: ZB = ZB { val: rin.c293, known: ALL };
    let r_c298: ZN = rin.c298;
    let r_c299: ZN = rin.c299;
    let r_c300: ZN = rin.c300;
    let r_c301: ZN = rin.c301;
    let r_c302: ZB = ZB { val: rin.c302, known: ALL };
    let r_c303: ZB = ZB { val: rin.c303, known: ALL };
    let r_c308: ZN = rin.c308;
    let r_c309: ZN = rin.c309;
    let r_c310: ZN = rin.c310;
    let r_c311: ZN = rin.c311;
    let r_c312: ZN = rin.c312;
    let r_c313: ZN = rin.c313;
    let n69: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c87);
    let n70: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c84);
    let n71: ZB = zb_not(r_c41);
    let n72: ZB = zb_not(r_c42);
    let n73: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n74: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c86);
    let n75: ZB = zb_not(r_c292);
    let n76: ZB = zb_not(r_c293);
    let n77: bool = P8::from_raw(0i32) == u.c296;
    let n78: bool = P8::from_raw(0i32) == u.c297;
    let n79: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c298);
    let n80: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c299);
    let n81: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c300);
    let n82: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c301);
    let n83: ZB = zn_eq(zn_splat(P8::from_raw(4194304i32)), r_c245);
    let n84: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c247);
    let n85: ZB = zn_eq(zn_splat(P8::from_raw(2097152i32)), r_c248);
    let n86: ZB = zb_not(r_c302);
    let n87: ZB = zb_not(r_c303);
    let n88: bool = P8::from_raw(524288i32) == u.c304;
    let n89: bool = P8::from_raw(524288i32) == u.c305;
    let n90: bool = P8::from_raw(0i32) == u.c306;
    let n91: bool = P8::from_raw(0i32) == u.c307;
    let n92: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c308);
    let n93: ZB = zb_not(r_c261);
    let n94: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c310);
    let n95: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c264);
    let n96: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c312);
    let n97: ZB = zn_eq(zn_splat(P8::from_raw(6291456i32)), r_c313);
    let n98: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c267);
    let n99: ZB = zb_not(r_c43);
    let n100: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c85);
    let n101: ZB = zb_not(r_c38);
    let n104: ZB = zb_not(n95);
    let n105: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c264);
    let n106: ZB = zb_not(n105);
    let n107: ZB = zn_eq(zn_splat(P8::from_raw(131072i32)), r_c264);
    let n108: ZN = zn_sub(r_c252, zn_splat(P8::from_raw(65536i32)));
    let n109: ZB = zn_lt(n108, zn_splat(P8::from_raw(0i32)));
    let n122: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n124: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n150: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c311);
    let n151: ZB = zb_not(n150);
    let n152: ZB = zb_and(n104, n122);
    let n153: ZB = zb_and(n106, n152);
    let n154: ZB = zb_and(n107, n153);
    let n155: ZB = zb_and(n109, n154);
    let n164: ZN = zn_add(r_c309, r_c311);
    let n165: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n164);
    let n166: ZN = zn_flr(n165);
    let n167: ZN = zn_add(r_c268, n166);
    let n168: ZN = zsel_n(n151, n167, r_c268);
    let n169: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n168);
    let n170: ZN = zn_div(n169, zn_splat(P8::from_raw(524288i32)));
    let n171: ZN = zn_flr(n170);
    let n172: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n171);
    let n173: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n169);
    let n174: ZN = zn_sub(n173, zn_splat(P8::from_raw(65536i32)));
    let n175: ZN = zn_div(n174, zn_splat(P8::from_raw(524288i32)));
    let n176: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n175);
    let n177: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n172);
    let n178: ZB = zn_le(n177, n176);
    let n179: ZB = zn_gt(n177, n176);
    let n180: ZB = zb_and(n155, n179);
    let n181: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n177);
    let n182: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n181);
    let n183: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n182);
    let n184: ZN = zn_rem(n174, zn_splat(P8::from_raw(524288i32)));
    let n185: ZB = zn_ge(n184, zn_splat(P8::from_raw(393216i32)));
    let n186: ZN = zn_mul(n177, zn_splat(P8::from_raw(524288i32)));
    let n187: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n186);
    let n188: ZB = zn_eq(n173, n187);
    let n189: ZB = zb_and(n104, n106);
    let n190: ZB = zb_and(n107, n189);
    let n191: ZB = zb_and(n109, n190);
    let n192: ZB = zb_and(n122, n191);
    let n193: ZB = zb_and(n178, n192);
    let n194: ZB = zb_or(n185, n188);
    let n195: ZB = zb_and(n183, n194);
    let n196: ZB = zb_not(n195);
    let n197: ZB = zb_and(n193, n195);
    let n198: ZB = zb_and(n193, n196);
    let n199: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n182);
    let n200: ZN = zn_rem(n169, zn_splat(P8::from_raw(524288i32)));
    let n201: ZB = zn_le(n200, zn_splat(P8::from_raw(131072i32)));
    let n202: ZB = zb_and(n199, n201);
    let n203: ZB = zb_not(n202);
    let n204: ZB = zb_and(n198, n202);
    let n205: ZB = zb_and(n198, n203);
    let n206: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n182);
    let n207: ZB = zb_not(n206);
    let n208: ZB = zb_and(n205, n206);
    let n209: ZB = zb_and(n205, n207);
    let n210: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n182);
    let n211: ZB = zb_not(n210);
    let n212: ZB = zb_and(n209, n210);
    let n213: ZB = zb_and(n209, n211);
    let n214: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n172);
    let n215: ZB = zn_le(n214, n176);
    let n216: ZB = zn_gt(n214, n176);
    let n217: ZB = zb_and(n213, n215);
    let n218: ZB = zb_and(n213, n216);
    let n219: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n214);
    let n220: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n219);
    let n221: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n220);
    let n222: ZN = zn_mul(n214, zn_splat(P8::from_raw(524288i32)));
    let n223: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n222);
    let n224: ZB = zn_eq(n173, n223);
    let n225: ZB = zb_or(n185, n224);
    let n226: ZB = zb_and(n221, n225);
    let n227: ZB = zb_not(n226);
    let n228: ZB = zb_and(n217, n226);
    let n229: ZB = zb_and(n217, n227);
    let n230: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n220);
    let n231: ZB = zb_and(n201, n230);
    let n232: ZB = zb_not(n231);
    let n233: ZB = zb_and(n229, n231);
    let n234: ZB = zb_and(n229, n232);
    let n235: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n220);
    let n236: ZB = zb_not(n235);
    let n237: ZB = zb_and(n234, n235);
    let n238: ZB = zb_and(n234, n236);
    let n239: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n220);
    let n240: ZB = zb_not(n239);
    let n241: ZB = zb_and(n238, n239);
    let n242: ZB = zb_and(n238, n240);
    let n243: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n172);
    let n244: ZB = zn_le(n243, n176);
    let n245: ZB = zn_gt(n243, n176);
    let n246: ZB = zb_and(n242, n244);
    let n247: ZB = zb_and(n242, n245);
    let n248: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n243);
    let n249: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n248);
    let n250: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n249);
    let n251: ZN = zn_mul(n243, zn_splat(P8::from_raw(524288i32)));
    let n252: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n251);
    let n253: ZB = zn_eq(n173, n252);
    let n254: ZB = zb_or(n185, n253);
    let n255: ZB = zb_and(n250, n254);
    let n256: ZB = zb_not(n255);
    let n257: ZB = zb_and(n246, n255);
    let n258: ZB = zb_and(n246, n256);
    let n259: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n249);
    let n260: ZB = zb_and(n201, n259);
    let n261: ZB = zb_not(n260);
    let n262: ZB = zb_and(n258, n260);
    let n263: ZB = zb_and(n258, n261);
    let n264: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n249);
    let n265: ZB = zb_not(n264);
    let n266: ZB = zb_and(n263, n264);
    let n267: ZB = zb_and(n263, n265);
    let n268: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n249);
    let n269: ZB = zb_not(n268);
    let n270: ZB = zb_and(n267, n268);
    let n271: ZB = zb_and(n267, n269);
    let n272: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n172);
    let n273: ZB = zn_gt(n272, n176);
    let n274: ZB = zb_or(n247, n271);
    let n275: ZB = zb_or(n245, n273);
    let n276: ZB = zb_or(n218, n274);
    let n277: ZB = zb_or(n216, n275);
    let n278: ZB = zb_or(n180, n276);
    let n279: ZB = zb_or(n179, n277);
    let n280: ZB = zn_le(n168, zn_splat(P8::from_raw(8388608i32)));
    let n281: ZB = zb_and(n278, n280);
    let n282: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n169);
    let n283: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(589824i32)), n282, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n284: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n282);
    let n285: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n173);
    let n286: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n285);
    let n287: ZB = zb_and(n284, n286);
    let n288: ZB = zb_or(n283, n287);
    let n289: ZB = zb_not(n288);
    let n290: ZN = zsel_n(n288, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n291: ZN = zsel_n(n289, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n292: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n291);
    let n293: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n291);
    let n294: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n169);
    let n295: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n294);
    let n296: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n173);
    let n297: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n296);
    let n298: ZB = zb_and(n295, n297);
    let n299: ZN = zsel_n(n289, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n300: ZB = zn_gt(n290, zn_splat(P8::from_raw(0i32)));
    let n301: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(393216i32)), n294, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n302: ZB = zb_or(n298, n301);
    let n303: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(786432i32)), n294, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n304: ZB = zb_or(n298, n303);
    let n305: ZN = zsel_n(n304, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n306: ZN = zsel_n(n302, zn_splat(P8::from_raw(-65536i32)), n305);
    let n307: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n306);
    let n308: ZB = zb_not(n307);
    let n309: ZN = zn_neg(n306);
    let n310: ZN = zn_mul(n309, zn_splat(P8::from_raw(131072i32)));
    let n311: ZN = zsel_n(n308, n310, zn_splat(P8::from_raw(0i32)));
    let n312: ZN = zsel_n(n308, zn_splat(P8::from_raw(-131072i32)), n299);
    let n313: ZN = zsel_n(n300, zn_splat(P8::from_raw(0i32)), n290);
    let n314: ZN = zsel_n(n300, zn_splat(P8::from_raw(0i32)), n311);
    let n315: ZN = zsel_n(n300, zn_splat(P8::from_raw(-131072i32)), n312);
    let n316: ZB = zn_lt(n168, zn_splat(P8::from_raw(-262144i32)));
    let n317: ZB = zn_ge(n168, zn_splat(P8::from_raw(-262144i32)));
    let n318: ZB = zb_and(n281, n316);
    let n323: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n292);
    let n324: ZN = zsel_n(n308, n310, n323);
    let n325: ZN = zsel_n(n300, n323, n324);
    let n326: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n293);
    let n327: ZN = zsel_n(n308, n310, n326);
    let n328: ZN = zsel_n(n300, n326, n327);
    let n330: ZB = zb_or(n208, n212);
    let n331: ZB = zb_or(n204, n330);
    let n332: ZB = zb_or(n197, n331);
    let n333: ZB = zb_or(n237, n241);
    let n334: ZB = zb_or(n233, n333);
    let n335: ZB = zb_or(n228, n334);
    let n336: ZB = zb_or(n266, n270);
    let n337: ZB = zb_or(n262, n336);
    let n338: ZB = zb_or(n257, n337);
    let n339: ZB = zb_or(n335, n338);
    let n340: ZB = zb_or(n332, n339);
    let n341: ZB = zn_gt(n168, zn_splat(P8::from_raw(8388608i32)));
    let n342: ZB = zb_and(n278, n341);
    let n343: ZB = zb_or(n340, n342);
    let n344: ZB = zb_or(n279, n340);
    let n345: ZB = zb_and(n316, n343);
    let n349: ZN = zsel_n(n318, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n350: ZB = zb_not(n318);
    let n351: ZB = zb_or(n318, n345);
    let n352: ZB = zsel_b(n318, n279, n344);
    let n357: ZB = zb_not(n107);
    let n358: ZB = zn_ge(n108, zn_splat(P8::from_raw(0i32)));
    let n359: ZN = zsel_n(n107, n108, r_c252);
    let n360: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n361: ZN = zsel_n(n124, n360, r_c20);
    let n362: ZB = zb_and(n95, n122);
    let n363: ZN = zn_add(r_c311, zn_splat(P8::from_raw(32768i32)));
    let n364: ZB = zn_gt(n363, zn_splat(P8::from_raw(0i32)));
    let n365: ZB = zn_gt(r_c252, zn_splat(P8::from_raw(0i32)));
    let n366: ZB = zb_and(n104, n105);
    let n367: ZB = zb_and(n122, n366);
    let n368: ZB = zb_and(n364, n365);
    let n369: ZN = zsel_n(n368, n108, r_c252);
    let n370: ZN = zsel_n(n368, zn_splat(P8::from_raw(0i32)), n363);
    let n371: ZB = zn_gt(n370, zn_splat(P8::from_raw(0i32)));
    let n372: ZB = zb_and(n153, n357);
    let n373: ZB = zb_and(n154, n358);
    let n374: ZN = zsel_n(n107, zn_splat(P8::from_raw(393216i32)), r_c263);
    let n375: ZB = zb_or(n372, n373);
    let n376: ZN = zsel_n(n105, r_c263, n374);
    let n377: ZN = zsel_n(n95, r_c263, n376);
    let n378: ZN = zsel_n(n124, r_c263, n377);
    let n379: ZN = zsel_n(n124, zn_splat(u.c294), zn_splat(P8::from_raw(1048576i32)));
    let n380: ZN = zsel_n(n124, zn_splat(u.c295), zn_splat(P8::from_raw(1048576i32)));
    let n381: ZN = zn_sub(n165, zn_splat(P8::from_raw(32768i32)));
    let n382: ZN = zn_sub(n381, n166);
    let n383: ZN = zsel_n(n151, n382, r_c309);
    let n384: ZB = zn_lt(n168, zn_splat(P8::from_raw(7340032i32)));
    let n385: ZN = zsel_n(n384, zn_splat(P8::from_raw(196608i32)), r_c252);
    let n386: ZN = zsel_n(n384, zn_splat(P8::from_raw(65536i32)), r_c264);
    let n387: ZB = zn_gt(n168, zn_splat(P8::from_raw(6291456i32)));
    let n388: ZB = zb_and(n371, n387);
    let n389: ZN = zsel_n(n388, zn_splat(P8::from_raw(327680i32)), n369);
    let n390: ZN = zsel_n(n388, zn_splat(P8::from_raw(131072i32)), r_c264);
    let n391: ZN = zsel_n(n388, zn_splat(P8::from_raw(6291456i32)), n168);
    let n392: ZN = zsel_n(n388, zn_splat(P8::from_raw(0i32)), n370);
    let n393: ZN = zsel_n(n105, n389, n359);
    let n394: ZN = zsel_n(n105, n390, r_c264);
    let n395: ZN = zsel_n(n105, n391, n168);
    let n396: ZN = zsel_n(n105, n392, r_c311);
    let n397: ZB = zb_or(n367, n375);
    let n398: ZN = zsel_n(n95, n385, n393);
    let n399: ZN = zsel_n(n95, n386, n394);
    let n400: ZN = zsel_n(n95, n168, n395);
    let n401: ZN = zsel_n(n95, r_c311, n396);
    let n402: ZB = zb_or(n362, n397);
    let n403: ZN = zsel_n(n124, r_c252, n398);
    let n404: ZN = zsel_n(n124, r_c264, n399);
    let n405: ZN = zsel_n(n124, r_c268, n400);
    let n406: ZN = zsel_n(n124, r_c309, n383);
    let n407: ZN = zsel_n(n124, r_c311, n401);
    let n408: ZB = zb_or(n124, n402);
    let n411: ZW = zw_bits_n(r_c39);
    let n412: ZW = zw_mix1(zw_splat(11400714819323198485u64), n411, 39u64);
    let n413: ZW = zw_mix2(zw_splat(11562461410679940143u64), n411, 39u64);
    let n414: ZW = zw_bits_n(n168);
    let n415: ZW = zw_mix1(n412, n414, 273u64);
    let n416: ZW = zw_mix2(n413, n414, 273u64);
    let n417: ZW = zw_bits_n(r_c20);
    let n418: ZW = zw_mix1(n415, n417, 20u64);
    let n419: ZW = zw_mix2(n416, n417, 20u64);
    let n420: u64 = false as u64;
    let n421: ZW = zw_mix1(n418, zw_splat(n420), 41u64);
    let n422: ZW = zw_mix2(n419, zw_splat(n420), 41u64);
    let n423: u64 = P8::from_raw(0i32).as_raw_u32() as u64;
    let n424: ZW = zw_mix1(n421, zw_splat(n423), 253u64);
    let n425: ZW = zw_mix2(n422, zw_splat(n423), 253u64);
    let n426: ZW = zw_mix1(n424, zw_splat(n423), 255u64);
    let n427: ZW = zw_mix2(n425, zw_splat(n423), 255u64);
    let n428: u64 = P8::from_raw(65536i32).as_raw_u32() as u64;
    let n429: ZW = zw_mix1(n426, zw_splat(n428), 256u64);
    let n430: ZW = zw_mix2(n427, zw_splat(n428), 256u64);
    let n431: ZW = zw_bits_n(n290);
    let n432: ZW = zw_mix1(n429, n431, 258u64);
    let n433: ZW = zw_mix2(n430, n431, 258u64);
    let n434: ZW = zw_mix1(n432, zw_splat(n420), 265u64);
    let n435: ZW = zw_mix2(n433, zw_splat(n420), 265u64);
    let n436: ZW = zw_mix1(n434, zw_splat(n420), 266u64);
    let n437: ZW = zw_mix2(n435, zw_splat(n420), 266u64);
    let n438: ZW = zw_mix1(n436, zw_splat(n423), 308u64);
    let n439: ZW = zw_mix2(n437, zw_splat(n423), 308u64);
    let n440: ZW = zw_mix1(n438, zw_splat(n423), 309u64);
    let n441: ZW = zw_mix2(n439, zw_splat(n423), 309u64);
    let n442: ZW = zw_mix1(n440, zw_splat(n423), 310u64);
    let n443: ZW = zw_mix2(n441, zw_splat(n423), 310u64);
    let n444: ZW = zw_mix1(n442, zw_splat(n423), 311u64);
    let n445: ZW = zw_mix2(n443, zw_splat(n423), 311u64);
    let n446: ZW = zw_mix1(n444, zw_splat(n420), 312u64);
    let n447: ZW = zw_mix2(n445, zw_splat(n420), 312u64);
    let n448: ZW = zw_mix1(n446, zw_splat(n423), 320u64);
    let n449: ZW = zw_mix2(n447, zw_splat(n423), 320u64);
    let n450: ZW = zw_bits_n(n299);
    let n451: ZW = zw_mix1(n448, n450, 321u64);
    let n452: ZW = zw_mix2(n449, n450, 321u64);
    let n453: u64 = true as u64;
    let n454: ZW = zw_mix1(n444, zw_splat(n453), 312u64);
    let n455: ZW = zw_mix2(n445, zw_splat(n453), 312u64);
    let n456: ZW = zw_bits_n(n323);
    let n457: ZW = zw_mix1(n454, n456, 320u64);
    let n458: ZW = zw_mix2(n455, n456, 320u64);
    let n459: ZW = zw_mix1(n457, n450, 321u64);
    let n460: ZW = zw_mix2(n458, n450, 321u64);
    let n461: ZW = zw_bits_n(n326);
    let n462: ZW = zw_mix1(n446, n461, 320u64);
    let n463: ZW = zw_mix2(n447, n461, 320u64);
    let n464: ZW = zw_mix1(n462, n450, 321u64);
    let n465: ZW = zw_mix2(n463, n450, 321u64);
    let n466: ZW = zw_bits_n(n313);
    let n467: ZW = zw_mix1(n429, n466, 258u64);
    let n468: ZW = zw_mix2(n430, n466, 258u64);
    let n469: ZW = zw_mix1(n467, zw_splat(n420), 265u64);
    let n470: ZW = zw_mix2(n468, zw_splat(n420), 265u64);
    let n471: ZW = zw_mix1(n469, zw_splat(n453), 266u64);
    let n472: ZW = zw_mix2(n470, zw_splat(n453), 266u64);
    let n473: ZW = zw_mix1(n471, zw_splat(n423), 308u64);
    let n474: ZW = zw_mix2(n472, zw_splat(n423), 308u64);
    let n475: ZW = zw_mix1(n473, zw_splat(n423), 309u64);
    let n476: ZW = zw_mix2(n474, zw_splat(n423), 309u64);
    let n477: ZW = zw_mix1(n475, zw_splat(n423), 310u64);
    let n478: ZW = zw_mix2(n476, zw_splat(n423), 310u64);
    let n479: ZW = zw_mix1(n477, zw_splat(n423), 311u64);
    let n480: ZW = zw_mix2(n478, zw_splat(n423), 311u64);
    let n481: ZW = zw_mix1(n479, zw_splat(n420), 312u64);
    let n482: ZW = zw_mix2(n480, zw_splat(n420), 312u64);
    let n483: ZW = zw_bits_n(n314);
    let n484: ZW = zw_mix1(n481, n483, 320u64);
    let n485: ZW = zw_mix2(n482, n483, 320u64);
    let n486: ZW = zw_bits_n(n315);
    let n487: ZW = zw_mix1(n484, n486, 321u64);
    let n488: ZW = zw_mix2(n485, n486, 321u64);
    let n489: ZW = zw_mix1(n479, zw_splat(n453), 312u64);
    let n490: ZW = zw_mix2(n480, zw_splat(n453), 312u64);
    let n491: ZW = zw_bits_n(n325);
    let n492: ZW = zw_mix1(n489, n491, 320u64);
    let n493: ZW = zw_mix2(n490, n491, 320u64);
    let n494: ZW = zw_mix1(n492, n486, 321u64);
    let n495: ZW = zw_mix2(n493, n486, 321u64);
    let n496: ZW = zw_bits_n(n328);
    let n497: ZW = zw_mix1(n481, n496, 320u64);
    let n498: ZW = zw_mix2(n482, n496, 320u64);
    let n499: ZW = zw_mix1(n497, n486, 321u64);
    let n500: ZW = zw_mix2(n498, n486, 321u64);
    let n501: u64 = P8::from_raw(131072i32).as_raw_u32() as u64;
    let n502: ZW = zw_mix1(n415, zw_splat(n501), 20u64);
    let n503: ZW = zw_mix2(n416, zw_splat(n501), 20u64);
    let n504: ZW = zw_mix1(n502, zw_splat(n453), 41u64);
    let n505: ZW = zw_mix2(n503, zw_splat(n453), 41u64);
    let n506: u64 = P8::from_raw(655360i32).as_raw_u32() as u64;
    let n507: ZW = zw_mix1(n504, zw_splat(n506), 253u64);
    let n508: ZW = zw_mix2(n505, zw_splat(n506), 253u64);
    let n509: u64 = P8::from_raw(262144i32).as_raw_u32() as u64;
    let n510: ZW = zw_mix1(n507, zw_splat(n509), 255u64);
    let n511: ZW = zw_mix2(n508, zw_splat(n509), 255u64);
    let n512: ZW = zw_mix1(n510, zw_splat(n423), 256u64);
    let n513: ZW = zw_mix2(n511, zw_splat(n423), 256u64);
    let n514: ZW = zw_mix1(n512, n431, 258u64);
    let n515: ZW = zw_mix2(n513, n431, 258u64);
    let n516: ZW = zw_mix1(n514, zw_splat(n453), 265u64);
    let n517: ZW = zw_mix2(n515, zw_splat(n453), 265u64);
    let n518: ZW = zw_mix1(n516, zw_splat(n420), 266u64);
    let n519: ZW = zw_mix2(n517, zw_splat(n420), 266u64);
    let n520: u64 = P8::from_raw(98304i32).as_raw_u32() as u64;
    let n521: ZW = zw_mix1(n518, zw_splat(n520), 308u64);
    let n522: ZW = zw_mix2(n519, zw_splat(n520), 308u64);
    let n523: u64 = P8::from_raw(69510i32).as_raw_u32() as u64;
    let n524: ZW = zw_mix1(n521, zw_splat(n523), 309u64);
    let n525: ZW = zw_mix2(n522, zw_splat(n523), 309u64);
    let n526: ZW = zw_mix1(n524, zw_splat(n501), 310u64);
    let n527: ZW = zw_mix2(n525, zw_splat(n501), 310u64);
    let n528: ZW = zw_mix1(n526, zw_splat(n423), 311u64);
    let n529: ZW = zw_mix2(n527, zw_splat(n423), 311u64);
    let n530: ZW = zw_mix1(n528, zw_splat(n420), 312u64);
    let n531: ZW = zw_mix2(n529, zw_splat(n420), 312u64);
    let n532: ZW = zw_mix1(n530, zw_splat(n428), 320u64);
    let n533: ZW = zw_mix2(n531, zw_splat(n428), 320u64);
    let n534: ZW = zw_mix1(n532, zw_splat(n423), 321u64);
    let n535: ZW = zw_mix2(n533, zw_splat(n423), 321u64);
    let n536: u64 = P8::from_raw(-131072i32).as_raw_u32() as u64;
    let n537: ZW = zw_mix1(n524, zw_splat(n536), 310u64);
    let n538: ZW = zw_mix2(n525, zw_splat(n536), 310u64);
    let n539: ZW = zw_mix1(n537, zw_splat(n423), 311u64);
    let n540: ZW = zw_mix2(n538, zw_splat(n423), 311u64);
    let n541: ZW = zw_mix1(n539, zw_splat(n453), 312u64);
    let n542: ZW = zw_mix2(n540, zw_splat(n453), 312u64);
    let n543: u64 = P8::from_raw(-327680i32).as_raw_u32() as u64;
    let n544: ZW = zw_mix1(n541, zw_splat(n543), 320u64);
    let n545: ZW = zw_mix2(n542, zw_splat(n543), 320u64);
    let n546: ZW = zw_mix1(n544, zw_splat(n423), 321u64);
    let n547: ZW = zw_mix2(n545, zw_splat(n423), 321u64);
    let n548: u64 = P8::from_raw(327680i32).as_raw_u32() as u64;
    let n549: ZW = zw_mix1(n530, zw_splat(n548), 320u64);
    let n550: ZW = zw_mix2(n531, zw_splat(n548), 320u64);
    let n551: ZW = zw_mix1(n549, zw_splat(n423), 321u64);
    let n552: ZW = zw_mix2(n550, zw_splat(n423), 321u64);
    let n553: ZW = zw_mix1(n518, zw_splat(n523), 308u64);
    let n554: ZW = zw_mix2(n519, zw_splat(n523), 308u64);
    let n555: ZW = zw_mix1(n553, zw_splat(n520), 309u64);
    let n556: ZW = zw_mix2(n554, zw_splat(n520), 309u64);
    let n557: ZW = zw_mix1(n555, zw_splat(n423), 310u64);
    let n558: ZW = zw_mix2(n556, zw_splat(n423), 310u64);
    let n559: u64 = P8::from_raw(-98304i32).as_raw_u32() as u64;
    let n560: ZW = zw_mix1(n557, zw_splat(n559), 311u64);
    let n561: ZW = zw_mix2(n558, zw_splat(n559), 311u64);
    let n562: ZW = zw_mix1(n560, zw_splat(n420), 312u64);
    let n563: ZW = zw_mix2(n561, zw_splat(n420), 312u64);
    let n564: ZW = zw_mix1(n562, zw_splat(n423), 320u64);
    let n565: ZW = zw_mix2(n563, zw_splat(n423), 320u64);
    let n566: ZW = zw_mix1(n564, zw_splat(n543), 321u64);
    let n567: ZW = zw_mix2(n565, zw_splat(n543), 321u64);
    let n568: ZW = zw_mix1(n553, zw_splat(n523), 309u64);
    let n569: ZW = zw_mix2(n554, zw_splat(n523), 309u64);
    let n570: ZW = zw_mix1(n568, zw_splat(n536), 310u64);
    let n571: ZW = zw_mix2(n569, zw_splat(n536), 310u64);
    let n572: ZW = zw_mix1(n570, zw_splat(n559), 311u64);
    let n573: ZW = zw_mix2(n571, zw_splat(n559), 311u64);
    let n574: ZW = zw_mix1(n572, zw_splat(n453), 312u64);
    let n575: ZW = zw_mix2(n573, zw_splat(n453), 312u64);
    let n576: u64 = P8::from_raw(-231700i32).as_raw_u32() as u64;
    let n577: ZW = zw_mix1(n574, zw_splat(n576), 320u64);
    let n578: ZW = zw_mix2(n575, zw_splat(n576), 320u64);
    let n579: ZW = zw_mix1(n577, zw_splat(n576), 321u64);
    let n580: ZW = zw_mix2(n578, zw_splat(n576), 321u64);
    let n581: ZW = zw_mix1(n568, zw_splat(n501), 310u64);
    let n582: ZW = zw_mix2(n569, zw_splat(n501), 310u64);
    let n583: ZW = zw_mix1(n581, zw_splat(n559), 311u64);
    let n584: ZW = zw_mix2(n582, zw_splat(n559), 311u64);
    let n585: ZW = zw_mix1(n583, zw_splat(n420), 312u64);
    let n586: ZW = zw_mix2(n584, zw_splat(n420), 312u64);
    let n587: u64 = P8::from_raw(231700i32).as_raw_u32() as u64;
    let n588: ZW = zw_mix1(n585, zw_splat(n587), 320u64);
    let n589: ZW = zw_mix2(n586, zw_splat(n587), 320u64);
    let n590: ZW = zw_mix1(n588, zw_splat(n576), 321u64);
    let n591: ZW = zw_mix2(n589, zw_splat(n576), 321u64);
    let n592: ZW = zw_mix1(n557, zw_splat(n501), 311u64);
    let n593: ZW = zw_mix2(n558, zw_splat(n501), 311u64);
    let n594: ZW = zw_mix1(n592, zw_splat(n420), 312u64);
    let n595: ZW = zw_mix2(n593, zw_splat(n420), 312u64);
    let n596: ZW = zw_mix1(n594, zw_splat(n423), 320u64);
    let n597: ZW = zw_mix2(n595, zw_splat(n423), 320u64);
    let n598: ZW = zw_mix1(n596, zw_splat(n548), 321u64);
    let n599: ZW = zw_mix2(n597, zw_splat(n548), 321u64);
    let n600: ZW = zw_mix1(n570, zw_splat(n501), 311u64);
    let n601: ZW = zw_mix2(n571, zw_splat(n501), 311u64);
    let n602: ZW = zw_mix1(n600, zw_splat(n453), 312u64);
    let n603: ZW = zw_mix2(n601, zw_splat(n453), 312u64);
    let n604: ZW = zw_mix1(n602, zw_splat(n576), 320u64);
    let n605: ZW = zw_mix2(n603, zw_splat(n576), 320u64);
    let n606: ZW = zw_mix1(n604, zw_splat(n587), 321u64);
    let n607: ZW = zw_mix2(n605, zw_splat(n587), 321u64);
    let n608: ZW = zw_mix1(n581, zw_splat(n501), 311u64);
    let n609: ZW = zw_mix2(n582, zw_splat(n501), 311u64);
    let n610: ZW = zw_mix1(n608, zw_splat(n420), 312u64);
    let n611: ZW = zw_mix2(n609, zw_splat(n420), 312u64);
    let n612: ZW = zw_mix1(n610, zw_splat(n587), 320u64);
    let n613: ZW = zw_mix2(n611, zw_splat(n587), 320u64);
    let n614: ZW = zw_mix1(n612, zw_splat(n587), 321u64);
    let n615: ZW = zw_mix2(n613, zw_splat(n587), 321u64);
    let n616: ZW = zw_mix1(n512, n466, 258u64);
    let n617: ZW = zw_mix2(n513, n466, 258u64);
    let n618: ZW = zw_mix1(n616, zw_splat(n453), 265u64);
    let n619: ZW = zw_mix2(n617, zw_splat(n453), 265u64);
    let n620: ZW = zw_mix1(n618, zw_splat(n453), 266u64);
    let n621: ZW = zw_mix2(n619, zw_splat(n453), 266u64);
    let n622: ZW = zw_mix1(n620, zw_splat(n520), 308u64);
    let n623: ZW = zw_mix2(n621, zw_splat(n520), 308u64);
    let n624: ZW = zw_mix1(n622, zw_splat(n523), 309u64);
    let n625: ZW = zw_mix2(n623, zw_splat(n523), 309u64);
    let n626: ZW = zw_mix1(n624, zw_splat(n501), 310u64);
    let n627: ZW = zw_mix2(n625, zw_splat(n501), 310u64);
    let n628: ZW = zw_mix1(n626, zw_splat(n423), 311u64);
    let n629: ZW = zw_mix2(n627, zw_splat(n423), 311u64);
    let n630: ZW = zw_mix1(n628, zw_splat(n420), 312u64);
    let n631: ZW = zw_mix2(n629, zw_splat(n420), 312u64);
    let n632: ZW = zw_mix1(n630, zw_splat(n428), 320u64);
    let n633: ZW = zw_mix2(n631, zw_splat(n428), 320u64);
    let n634: ZW = zw_mix1(n632, zw_splat(n423), 321u64);
    let n635: ZW = zw_mix2(n633, zw_splat(n423), 321u64);
    let n636: ZW = zw_mix1(n624, zw_splat(n536), 310u64);
    let n637: ZW = zw_mix2(n625, zw_splat(n536), 310u64);
    let n638: ZW = zw_mix1(n636, zw_splat(n423), 311u64);
    let n639: ZW = zw_mix2(n637, zw_splat(n423), 311u64);
    let n640: ZW = zw_mix1(n638, zw_splat(n453), 312u64);
    let n641: ZW = zw_mix2(n639, zw_splat(n453), 312u64);
    let n642: ZW = zw_mix1(n640, zw_splat(n543), 320u64);
    let n643: ZW = zw_mix2(n641, zw_splat(n543), 320u64);
    let n644: ZW = zw_mix1(n642, zw_splat(n423), 321u64);
    let n645: ZW = zw_mix2(n643, zw_splat(n423), 321u64);
    let n646: ZW = zw_mix1(n630, zw_splat(n548), 320u64);
    let n647: ZW = zw_mix2(n631, zw_splat(n548), 320u64);
    let n648: ZW = zw_mix1(n646, zw_splat(n423), 321u64);
    let n649: ZW = zw_mix2(n647, zw_splat(n423), 321u64);
    let n650: ZW = zw_mix1(n620, zw_splat(n523), 308u64);
    let n651: ZW = zw_mix2(n621, zw_splat(n523), 308u64);
    let n652: ZW = zw_mix1(n650, zw_splat(n520), 309u64);
    let n653: ZW = zw_mix2(n651, zw_splat(n520), 309u64);
    let n654: ZW = zw_mix1(n652, zw_splat(n423), 310u64);
    let n655: ZW = zw_mix2(n653, zw_splat(n423), 310u64);
    let n656: ZW = zw_mix1(n654, zw_splat(n559), 311u64);
    let n657: ZW = zw_mix2(n655, zw_splat(n559), 311u64);
    let n658: ZW = zw_mix1(n656, zw_splat(n420), 312u64);
    let n659: ZW = zw_mix2(n657, zw_splat(n420), 312u64);
    let n660: ZW = zw_mix1(n658, zw_splat(n423), 320u64);
    let n661: ZW = zw_mix2(n659, zw_splat(n423), 320u64);
    let n662: ZW = zw_mix1(n660, zw_splat(n543), 321u64);
    let n663: ZW = zw_mix2(n661, zw_splat(n543), 321u64);
    let n664: ZW = zw_mix1(n650, zw_splat(n523), 309u64);
    let n665: ZW = zw_mix2(n651, zw_splat(n523), 309u64);
    let n666: ZW = zw_mix1(n664, zw_splat(n536), 310u64);
    let n667: ZW = zw_mix2(n665, zw_splat(n536), 310u64);
    let n668: ZW = zw_mix1(n666, zw_splat(n559), 311u64);
    let n669: ZW = zw_mix2(n667, zw_splat(n559), 311u64);
    let n670: ZW = zw_mix1(n668, zw_splat(n453), 312u64);
    let n671: ZW = zw_mix2(n669, zw_splat(n453), 312u64);
    let n672: ZW = zw_mix1(n670, zw_splat(n576), 320u64);
    let n673: ZW = zw_mix2(n671, zw_splat(n576), 320u64);
    let n674: ZW = zw_mix1(n672, zw_splat(n576), 321u64);
    let n675: ZW = zw_mix2(n673, zw_splat(n576), 321u64);
    let n676: ZW = zw_mix1(n664, zw_splat(n501), 310u64);
    let n677: ZW = zw_mix2(n665, zw_splat(n501), 310u64);
    let n678: ZW = zw_mix1(n676, zw_splat(n559), 311u64);
    let n679: ZW = zw_mix2(n677, zw_splat(n559), 311u64);
    let n680: ZW = zw_mix1(n678, zw_splat(n420), 312u64);
    let n681: ZW = zw_mix2(n679, zw_splat(n420), 312u64);
    let n682: ZW = zw_mix1(n680, zw_splat(n587), 320u64);
    let n683: ZW = zw_mix2(n681, zw_splat(n587), 320u64);
    let n684: ZW = zw_mix1(n682, zw_splat(n576), 321u64);
    let n685: ZW = zw_mix2(n683, zw_splat(n576), 321u64);
    let n686: ZW = zw_mix1(n654, zw_splat(n501), 311u64);
    let n687: ZW = zw_mix2(n655, zw_splat(n501), 311u64);
    let n688: ZW = zw_mix1(n686, zw_splat(n420), 312u64);
    let n689: ZW = zw_mix2(n687, zw_splat(n420), 312u64);
    let n690: ZW = zw_mix1(n688, zw_splat(n423), 320u64);
    let n691: ZW = zw_mix2(n689, zw_splat(n423), 320u64);
    let n692: ZW = zw_mix1(n690, zw_splat(n548), 321u64);
    let n693: ZW = zw_mix2(n691, zw_splat(n548), 321u64);
    let n694: ZW = zw_mix1(n666, zw_splat(n501), 311u64);
    let n695: ZW = zw_mix2(n667, zw_splat(n501), 311u64);
    let n696: ZW = zw_mix1(n694, zw_splat(n453), 312u64);
    let n697: ZW = zw_mix2(n695, zw_splat(n453), 312u64);
    let n698: ZW = zw_mix1(n696, zw_splat(n576), 320u64);
    let n699: ZW = zw_mix2(n697, zw_splat(n576), 320u64);
    let n700: ZW = zw_mix1(n698, zw_splat(n587), 321u64);
    let n701: ZW = zw_mix2(n699, zw_splat(n587), 321u64);
    let n702: ZW = zw_mix1(n676, zw_splat(n501), 311u64);
    let n703: ZW = zw_mix2(n677, zw_splat(n501), 311u64);
    let n704: ZW = zw_mix1(n702, zw_splat(n420), 312u64);
    let n705: ZW = zw_mix2(n703, zw_splat(n420), 312u64);
    let n706: ZW = zw_mix1(n704, zw_splat(n587), 320u64);
    let n707: ZW = zw_mix2(n705, zw_splat(n587), 320u64);
    let n708: ZW = zw_mix1(n706, zw_splat(n587), 321u64);
    let n709: ZW = zw_mix2(n707, zw_splat(n587), 321u64);
    let n710: ZW = zw_mix1(zw_splat(11400714819323198485u64), n417, 20u64);
    let n711: ZW = zw_mix2(zw_splat(11562461410679940143u64), n417, 20u64);
    let n712: ZW = zw_mix1(n710, zw_splat(n420), 41u64);
    let n713: ZW = zw_mix2(n711, zw_splat(n420), 41u64);
    let n714: u64 = mix64(11400714819323198485u64 ^ mix64(n501 ^ 20u64));
    let n715: u64 = 11562461410679940143u64.wrapping_add(mix64(n501.wrapping_mul((20u64 << 1) | 1)));
    let n716: u64 = mix64(n714 ^ mix64(n453 ^ 41u64));
    let n717: u64 = n715.wrapping_add(mix64(n453.wrapping_mul((41u64 << 1) | 1)));
    let n718: ZW = zw_bits_b(n350);
    let n719: ZW = zw_mix1(zw_splat(11400714819323198485u64), n718, 38u64);
    let n720: ZW = zw_mix2(zw_splat(11562461410679940143u64), n718, 38u64);
    let n721: ZW = zw_bits_n(n349);
    let n722: ZW = zw_mix1(n719, n721, 39u64);
    let n723: ZW = zw_mix2(n720, n721, 39u64);
    let n724: ZW = zw_mix1(n722, n417, 20u64);
    let n725: ZW = zw_mix2(n723, n417, 20u64);
    let n726: ZW = zw_mix1(n722, zw_splat(n501), 20u64);
    let n727: ZW = zw_mix2(n723, zw_splat(n501), 20u64);
    let n728: ZW = zw_bits_n(n361);
    let n729: ZW = zw_mix1(zw_splat(11400714819323198485u64), n728, 20u64);
    let n730: ZW = zw_mix2(zw_splat(11562461410679940143u64), n728, 20u64);
    let n731: ZW = zw_mix1(n729, n411, 39u64);
    let n732: ZW = zw_mix2(n730, n411, 39u64);
    let n733: ZW = zw_bits_n(n403);
    let n734: ZW = zw_mix1(n731, n733, 252u64);
    let n735: ZW = zw_mix2(n732, n733, 252u64);
    let n736: ZW = zw_bits_n(n378);
    let n737: ZW = zw_mix1(n734, n736, 263u64);
    let n738: ZW = zw_mix2(n735, n736, 263u64);
    let n739: ZW = zw_bits_n(n404);
    let n740: ZW = zw_mix1(n737, n739, 264u64);
    let n741: ZW = zw_mix2(n738, n739, 264u64);
    let n742: ZW = zw_bits_n(n405);
    let n743: ZW = zw_mix1(n740, n742, 268u64);
    let n744: ZW = zw_mix2(n741, n742, 268u64);
    let n745: ZW = zw_bits_n(n379);
    let n746: ZW = zw_mix1(n743, n745, 294u64);
    let n747: ZW = zw_mix2(n744, n745, 294u64);
    let n748: ZW = zw_bits_n(n380);
    let n749: ZW = zw_mix1(n746, n748, 295u64);
    let n750: ZW = zw_mix2(n747, n748, 295u64);
    let n751: ZW = zw_bits_n(n406);
    let n752: ZW = zw_mix1(n749, n751, 309u64);
    let n753: ZW = zw_mix2(n750, n751, 309u64);
    let n754: ZW = zw_bits_n(n407);
    let n755: ZW = zw_mix1(n752, n754, 311u64);
    let n756: ZW = zw_mix2(n753, n754, 311u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v0_b0: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v0_b0: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v1_b1: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v1_b1: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v1_b1: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v2_b2: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v2_b2: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v2_b2: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v16_b3: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v16_b3: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v16_b3: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v17_b4: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v17_b4: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v17_b4: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v18_b5: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v18_b5: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v18_b5: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v32_b6: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v32_b6: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v32_b6: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v33_b7: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v33_b7: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v33_b7: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v34_b8: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v34_b8: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v34_b8: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v36_b9: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v36_b9: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v36_b9: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v37_b10: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v37_b10: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v37_b10: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v38_b11: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v38_b11: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v38_b11: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v40_b12: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v40_b12: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v40_b12: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v41_b13: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v41_b13: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v41_b13: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v42_b14: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v42_b14: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v42_b14: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v48_b15: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v48_b15: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v48_b15: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v49_b16: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v49_b16: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v49_b16: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v50_b17: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v50_b17: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v50_b17: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v52_b18: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v52_b18: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v52_b18: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v53_b19: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v53_b19: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v53_b19: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v54_b20: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v54_b20: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v54_b20: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v56_b21: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v56_b21: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v56_b21: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v57_b22: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v57_b22: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v57_b22: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v58_b23: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n279);
    let bd_v58_b23: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v58_b23: u16 = ALL & zb_holds(n278) & zb_holds(n280) & zb_holds(n317);
    let ok_v0_b24: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n344);
    let bd_v0_b24: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v0_b24: u16 = ALL & zb_holds(n317) & zb_holds(n343);
    let ok_v32_b25: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n344);
    let bd_v32_b25: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v32_b25: u16 = ALL & zb_holds(n317) & zb_holds(n343);
    let ok_v0_b26: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n352);
    let bd_v0_b26: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v0_b26: u16 = ALL & zb_holds(n351);
    let ok_v32_b27: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n352);
    let bd_v32_b27: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v32_b27: u16 = ALL & zb_holds(n351);
    let ok_v0_b28: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70);
    let bd_v0_b28: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v0_b28: u16 = ALL & zb_holds(n408);
    let sh0 = KShared0 {
        c39: r_c39,
        c273: n168,
    };
    let sh1 = KShared1 {
    };
    let sh2 = KShared2 {
        c39: n349,
        c38: n350,
    };
    let sh3 = KShared3 {
        c39: r_c39,
        c20: n361,
        c294: n379,
        c295: n380,
        c252: n403,
        c309: n406,
        c311: n407,
        c263: n378,
        c264: n404,
        c268: n405,
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
        c308: zn_splat(P8::from_raw(0i32)),
        c309: zn_splat(P8::from_raw(0i32)),
        c253: zn_splat(P8::from_raw(0i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(false),
        c258: n290,
        c265: zb_splat(false),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: n299,
        h1: n451, h2: n452,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c308: zn_splat(P8::from_raw(0i32)),
        c309: zn_splat(P8::from_raw(0i32)),
        c253: zn_splat(P8::from_raw(0i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(true),
        c258: n290,
        c265: zb_splat(false),
        c266: zb_splat(false),
        c320: n323,
        c321: n299,
        h1: n459, h2: n460,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c308: zn_splat(P8::from_raw(0i32)),
        c309: zn_splat(P8::from_raw(0i32)),
        c253: zn_splat(P8::from_raw(0i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(false),
        c258: n290,
        c265: zb_splat(false),
        c266: zb_splat(false),
        c320: n326,
        c321: n299,
        h1: n464, h2: n465,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c308: zn_splat(P8::from_raw(0i32)),
        c309: zn_splat(P8::from_raw(0i32)),
        c253: zn_splat(P8::from_raw(0i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(false),
        c258: n313,
        c265: zb_splat(false),
        c266: zb_splat(true),
        c320: n314,
        c321: n315,
        h1: n487, h2: n488,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c308: zn_splat(P8::from_raw(0i32)),
        c309: zn_splat(P8::from_raw(0i32)),
        c253: zn_splat(P8::from_raw(0i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(true),
        c258: n313,
        c265: zb_splat(false),
        c266: zb_splat(true),
        c320: n325,
        c321: n315,
        h1: n494, h2: n495,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c308: zn_splat(P8::from_raw(0i32)),
        c309: zn_splat(P8::from_raw(0i32)),
        c253: zn_splat(P8::from_raw(0i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(false),
        c258: n313,
        c265: zb_splat(false),
        c266: zb_splat(true),
        c320: n328,
        c321: n315,
        h1: n499, h2: n500,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(98304i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n290,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(65536i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n534, h2: n535,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(98304i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(-131072i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(true),
        c258: n290,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(-327680i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n546, h2: n547,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(98304i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n290,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(327680i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n551, h2: n552,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(98304i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(-98304i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n290,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: zn_splat(P8::from_raw(-327680i32)),
        h1: n566, h2: n567,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(-131072i32)),
        c311: zn_splat(P8::from_raw(-98304i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(true),
        c258: n290,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(-231700i32)),
        c321: zn_splat(P8::from_raw(-231700i32)),
        h1: n579, h2: n580,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(-98304i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n290,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(231700i32)),
        c321: zn_splat(P8::from_raw(-231700i32)),
        h1: n590, h2: n591,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(98304i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(131072i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n290,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: zn_splat(P8::from_raw(327680i32)),
        h1: n598, h2: n599,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(-131072i32)),
        c311: zn_splat(P8::from_raw(131072i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(true),
        c258: n290,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(-231700i32)),
        c321: zn_splat(P8::from_raw(231700i32)),
        h1: n606, h2: n607,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(131072i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n290,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(231700i32)),
        c321: zn_splat(P8::from_raw(231700i32)),
        h1: n614, h2: n615,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(98304i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n313,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(65536i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n634, h2: n635,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(98304i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(-131072i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(true),
        c258: n313,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(-327680i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n644, h2: n645,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(98304i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n313,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(327680i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n648, h2: n649,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(98304i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(-98304i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n313,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: zn_splat(P8::from_raw(-327680i32)),
        h1: n662, h2: n663,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(-131072i32)),
        c311: zn_splat(P8::from_raw(-98304i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(true),
        c258: n313,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(-231700i32)),
        c321: zn_splat(P8::from_raw(-231700i32)),
        h1: n674, h2: n675,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(-98304i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n313,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(231700i32)),
        c321: zn_splat(P8::from_raw(-231700i32)),
        h1: n684, h2: n685,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(98304i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(131072i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n313,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: zn_splat(P8::from_raw(327680i32)),
        h1: n692, h2: n693,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(-131072i32)),
        c311: zn_splat(P8::from_raw(131072i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(true),
        c258: n313,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(-231700i32)),
        c321: zn_splat(P8::from_raw(231700i32)),
        h1: n700, h2: n701,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(131072i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n313,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(231700i32)),
        c321: zn_splat(P8::from_raw(231700i32)),
        h1: n708, h2: n709,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: zb_splat(false),
        h1: n712, h2: n713,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v32_b25 & (if bd_v32_b25 { ALL } else { !ok_v32_b25 });
    take_1_1 |= live_v32_b25 & ok_v32_b25 & (if bd_v32_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        h1: zw_splat(n716), h2: zw_splat(n717),
    };
    // body 25: buttons 0x20, forks 0x0
    sink.o1(32, take_1_1, &sh1, &o1);
    declined |= live_v0_b26 & (if bd_v0_b26 { ALL } else { !ok_v0_b26 });
    take_2_0 |= live_v0_b26 & ok_v0_b26 & (if bd_v0_b26 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        h1: n724, h2: n725,
    };
    // body 26: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v32_b27 & (if bd_v32_b27 { ALL } else { !ok_v32_b27 });
    take_2_1 |= live_v32_b27 & ok_v32_b27 & (if bd_v32_b27 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: zn_splat(P8::from_raw(131072i32)),
        h1: n726, h2: n727,
    };
    // body 27: buttons 0x20, forks 0x0
    sink.o2(32, take_2_1, &sh2, &o2);
    declined |= live_v0_b28 & (if bd_v0_b28 { ALL } else { !ok_v0_b28 });
    take_3_0 |= live_v0_b28 & ok_v0_b28 & (if bd_v0_b28 { 0 } else { ALL });
    let o3 = KOut3 {
        h1: n755, h2: n756,
    };
    // body 28: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined
}
