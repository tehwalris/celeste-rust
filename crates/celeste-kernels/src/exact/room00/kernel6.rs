// GENERATED from a TRACED frame (shape 6). Do not edit.
//
// One input shape, 6 output shapes, 114 distinct button
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
pub const SHAPE: u64 = 5354716000392713856;

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
    pub c300: P8,
    pub c301: P8,
    pub c302: P8,
    pub c303: P8,
    pub c314: P8,
    pub c315: P8,
    pub c316: P8,
    pub c317: P8,
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
    ("objects[1].dash_accel.x", "num"),
    ("objects[1].dash_accel.y", "num"),
    ("objects[1].dash_effect_time", "num"),
    ("objects[1].dash_target.x", "num"),
    ("objects[1].dash_target.y", "num"),
    ("objects[1].dash_time", "num"),
    ("objects[1].djump", "num"),
    ("objects[1].flip.x", "bool"),
    ("objects[1].flip.y", "bool"),
    ("objects[1].grace", "num"),
    ("objects[1].p_dash", "bool"),
    ("objects[1].p_jump", "bool"),
    ("objects[1].rem.x", "num"),
    ("objects[1].rem.y", "num"),
    ("objects[1].solids", "bool"),
    ("objects[1].spd.x", "num"),
    ("objects[1].spd.y", "num"),
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
    pub c298: u16,
    pub c299: u16,
    pub c304: ZN,
    pub c305: ZN,
    pub c243: u16,
    pub c306: ZN,
    pub c307: ZN,
    pub c245: ZN,
    pub c247: ZN,
    pub c248: ZN,
    pub c251: u16,
    pub c308: ZN,
    pub c309: ZN,
    pub c253: ZN,
    pub c310: ZN,
    pub c311: ZN,
    pub c255: ZN,
    pub c256: ZN,
    pub c312: u16,
    pub c313: u16,
    pub c258: ZN,
    pub c265: u16,
    pub c266: u16,
    pub c318: ZN,
    pub c319: ZN,
    pub c268: u16,
    pub c320: ZN,
    pub c321: ZN,
    pub c272: ZN,
    pub c273: ZN,
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
    pub c298: u32,
    pub c299: u32,
    pub c304: u32,
    pub c305: u32,
    pub c243: u32,
    pub c306: u32,
    pub c307: u32,
    pub c245: u32,
    pub c247: u32,
    pub c248: u32,
    pub c251: u32,
    pub c308: u32,
    pub c309: u32,
    pub c253: u32,
    pub c310: u32,
    pub c311: u32,
    pub c255: u32,
    pub c256: u32,
    pub c312: u32,
    pub c313: u32,
    pub c258: u32,
    pub c265: u32,
    pub c266: u32,
    pub c318: u32,
    pub c319: u32,
    pub c268: u32,
    pub c320: u32,
    pub c321: u32,
    pub c272: u32,
    pub c273: u32,
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
        c300: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c301: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c302: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c303: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c314: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c315: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c316: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c317: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
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
        c298: cell("objects[0].flip.x")?,
        c299: cell("objects[0].flip.y")?,
        c304: cell("objects[0].rem.x")?,
        c305: cell("objects[0].rem.y")?,
        c243: cell("objects[0].solids")?,
        c306: cell("objects[0].spd.x")?,
        c307: cell("objects[0].spd.y")?,
        c245: cell("objects[0].spr")?,
        c247: cell("objects[0].x")?,
        c248: cell("objects[0].y")?,
        c251: cell("objects[1].collideable")?,
        c308: cell("objects[1].dash_accel.x")?,
        c309: cell("objects[1].dash_accel.y")?,
        c253: cell("objects[1].dash_effect_time")?,
        c310: cell("objects[1].dash_target.x")?,
        c311: cell("objects[1].dash_target.y")?,
        c255: cell("objects[1].dash_time")?,
        c256: cell("objects[1].djump")?,
        c312: cell("objects[1].flip.x")?,
        c313: cell("objects[1].flip.y")?,
        c258: cell("objects[1].grace")?,
        c265: cell("objects[1].p_dash")?,
        c266: cell("objects[1].p_jump")?,
        c318: cell("objects[1].rem.x")?,
        c319: cell("objects[1].rem.y")?,
        c268: cell("objects[1].solids")?,
        c320: cell("objects[1].spd.x")?,
        c321: cell("objects[1].spd.y")?,
        c272: cell("objects[1].x")?,
        c273: cell("objects[1].y")?,
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
        c298: match &b.cols[s.c298 as usize] {
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
        c299: match &b.cols[s.c299 as usize] {
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
        c304: match &b.cols[s.c304 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c305: match &b.cols[s.c305 as usize] {
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
        c306: match &b.cols[s.c306 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c307: match &b.cols[s.c307 as usize] {
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
        c253: match &b.cols[s.c253 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c312: match &b.cols[s.c312 as usize] {
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
        c313: match &b.cols[s.c313 as usize] {
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
        c258: match &b.cols[s.c258 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c266: match &b.cols[s.c266 as usize] {
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
        c318: match &b.cols[s.c318 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c319: match &b.cols[s.c319 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c268: match &b.cols[s.c268 as usize] {
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
        c320: match &b.cols[s.c320 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c321: match &b.cols[s.c321 as usize] {
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
    pub c280: ZN,
    pub c281: ZN,
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
    (234, "objects[0].collideable"),
    (300, "objects[0].dash_accel.x"),
    (301, "objects[0].dash_accel.y"),
    (236, "objects[0].dash_effect_time"),
    (302, "objects[0].dash_target.x"),
    (303, "objects[0].dash_target.y"),
    (238, "objects[0].dash_time"),
    (239, "objects[0].djump"),
    (304, "objects[0].flip.x"),
    (305, "objects[0].flip.y"),
    (241, "objects[0].grace"),
    (306, "objects[0].hitbox.h"),
    (307, "objects[0].hitbox.w"),
    (308, "objects[0].hitbox.x"),
    (309, "objects[0].hitbox.y"),
    (248, "objects[0].p_dash"),
    (249, "objects[0].p_jump"),
    (310, "objects[0].rem.x"),
    (311, "objects[0].rem.y"),
    (251, "objects[0].solids"),
    (312, "objects[0].spd.x"),
    (313, "objects[0].spd.y"),
    (255, "objects[0].x"),
    (256, "objects[0].y"),
    (259, "objects[1].collideable"),
    (314, "objects[1].flip.x"),
    (315, "objects[1].flip.y"),
    (316, "objects[1].hitbox.h"),
    (317, "objects[1].hitbox.w"),
    (318, "objects[1].hitbox.x"),
    (319, "objects[1].hitbox.y"),
    (267, "objects[1].off"),
    (320, "objects[1].rem.x"),
    (321, "objects[1].rem.y"),
    (269, "objects[1].solids"),
    (322, "objects[1].spd.x"),
    (323, "objects[1].spd.y"),
    (271, "objects[1].spr"),
    (272, "objects[1].start"),
    (274, "objects[1].x"),
    (275, "objects[1].y"),
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
    SCell::Obj(&[(21, 232), (20, 233), (11, 234), (28, 235), (33, 236), (27, 237), (26, 238), (30, 239), (14, 240), (29, 241), (15, 242), (19, 243), (18, 244), (22, 245), (23, 246), (24, 247), (32, 248), (31, 249), (4, 250), (12, 251), (3, 252), (13, 253), (0, 254), (1, 255), (2, 256)]),
    SCell::Obj(&[(21, 257), (20, 258), (11, 259), (14, 260), (15, 261), (19, 262), (18, 263), (22, 264), (23, 265), (24, 266), (41, 267), (4, 268), (12, 269), (3, 270), (13, 271), (38, 272), (0, 273), (1, 274), (2, 275)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 300), (2, 301)]),
    SCell::Obj(&[(1, 302), (2, 303)]),
    SCell::Obj(&[(1, 304), (2, 305)]),
    SCell::Obj(&[(17, 306), (16, 307), (1, 308), (2, 309)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 310), (2, 311)]),
    SCell::Obj(&[(1, 312), (2, 313)]),
    SCell::Clo(26, &[206]),
    SCell::Clo(25, &[206]),
    SCell::Obj(&[(1, 314), (2, 315)]),
    SCell::Obj(&[(17, 316), (16, 317), (1, 318), (2, 319)]),
    SCell::Clo(24, &[206]),
    SCell::Clo(23, &[206]),
    SCell::Clo(27, &[206]),
    SCell::Clo(28, &[206]),
    SCell::Clo(29, &[206]),
    SCell::Obj(&[(1, 320), (2, 321)]),
    SCell::Obj(&[(1, 322), (2, 323)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (232, 276),
    (233, 277),
    (235, 278),
    (237, 279),
    (240, 280),
    (242, 281),
    (243, 282),
    (244, 283),
    (245, 284),
    (246, 285),
    (247, 286),
    (250, 287),
    (252, 288),
    (254, 93),
    (257, 289),
    (258, 290),
    (260, 291),
    (261, 292),
    (262, 293),
    (263, 294),
    (264, 295),
    (265, 296),
    (266, 297),
    (268, 298),
    (270, 299),
    (273, 121),
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
    pub c310: ZN,
    pub c311: ZN,
    pub c256: ZN,
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
    pub c300: ZN,
    pub c301: ZN,
    pub c236: ZN,
    pub c302: ZN,
    pub c303: ZN,
    pub c238: ZN,
    pub c239: ZN,
    pub c304: ZB,
    pub c241: ZN,
    pub c248: ZB,
    pub c249: ZB,
    pub c312: ZN,
    pub c313: ZN,
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
    (261, "objects[0].flip.x"),
    (262, "objects[0].flip.y"),
    (263, "objects[0].hitbox.h"),
    (264, "objects[0].hitbox.w"),
    (265, "objects[0].hitbox.x"),
    (266, "objects[0].hitbox.y"),
    (241, "objects[0].off"),
    (267, "objects[0].rem.x"),
    (268, "objects[0].rem.y"),
    (243, "objects[0].solids"),
    (269, "objects[0].spd.x"),
    (270, "objects[0].spd.y"),
    (245, "objects[0].spr"),
    (246, "objects[0].start"),
    (248, "objects[0].x"),
    (249, "objects[0].y"),
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
    SCell::Obj(&[(21, 231), (20, 232), (11, 233), (14, 234), (15, 235), (19, 236), (18, 237), (22, 238), (23, 239), (24, 240), (41, 241), (4, 242), (12, 243), (3, 244), (13, 245), (38, 246), (0, 247), (1, 248), (2, 249)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 261), (2, 262)]),
    SCell::Obj(&[(17, 263), (16, 264), (1, 265), (2, 266)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 267), (2, 268)]),
    SCell::Obj(&[(1, 269), (2, 270)]),
    SCell::Val,
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
    (231, 250),
    (232, 251),
    (234, 252),
    (235, 253),
    (236, 254),
    (237, 255),
    (238, 256),
    (239, 257),
    (240, 258),
    (242, 259),
    (244, 260),
    (247, 121),
];

/// Outcome 2's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared2 {
    pub c87: ZN,
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

/// Outcome 3's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared3 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
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

// ---------------- outcome 4 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_4: &[(u32, &str)] = &[
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
pub const OUT_UBOOL_4: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_4: &[SCell] = &[
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

pub const OUT_GLOBALS_4: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_4: &[(u32, u32)] = &[
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

/// Outcome 4's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared4 {
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
}

/// Outcome 4's per-assignment values and lane masks.
/// The cells of outcome 4 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared4`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut4 {
    pub c87: ZN,
    pub c39: ZN,
    pub c20: ZN,
    pub c234: ZN,
    pub c246: ZN,
    pub c250: ZN,
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

// ---------------- outcome 5 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_5: &[(u32, &str)] = &[
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
pub const OUT_UBOOL_5: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_5: &[SCell] = &[
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

pub const OUT_GLOBALS_5: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_5: &[(u32, u32)] = &[
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

/// Outcome 5's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared5 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c318: ZN,
    pub c319: ZN,
    pub c273: ZN,
    pub c85: ZN,
}

/// Outcome 5's per-assignment values and lane masks.
/// The cells of outcome 5 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared5`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut5 {
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
    pub c272: ZN,
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
    b.cols[239] = Col::U(AV::Num(P8::from_raw(65536i32)));
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

/// An EMPTY accumulator with outcome 1's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append1`.
pub fn acc1(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_1, OUT_GLOBALS_1, OUT_PTRS_1, 0, cart, cache);
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
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
    b.cols[86] = Col::N(Vec::new());
    b.cols[234] = Col::U(AV::Bool(true));
    b.cols[300] = Col::N(Vec::new());
    b.cols[301] = Col::N(Vec::new());
    b.cols[236] = Col::N(Vec::new());
    b.cols[302] = Col::N(Vec::new());
    b.cols[303] = Col::N(Vec::new());
    b.cols[238] = Col::N(Vec::new());
    b.cols[239] = Col::N(Vec::new());
    b.cols[304] = Col::V(Vec::new());
    b.cols[305] = Col::U(AV::Bool(false));
    b.cols[241] = Col::N(Vec::new());
    b.cols[306] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[307] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[308] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[309] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[248] = Col::V(Vec::new());
    b.cols[249] = Col::V(Vec::new());
    b.cols[310] = Col::N(Vec::new());
    b.cols[311] = Col::N(Vec::new());
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[312] = Col::N(Vec::new());
    b.cols[313] = Col::N(Vec::new());
    b.cols[255] = Col::N(Vec::new());
    b.cols[256] = Col::N(Vec::new());
    b.cols[259] = Col::U(AV::Bool(true));
    b.cols[314] = Col::U(AV::Bool(false));
    b.cols[315] = Col::U(AV::Bool(false));
    b.cols[316] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[317] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[318] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[319] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[320] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[321] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Bool(true));
    b.cols[322] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[323] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(2359296i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(786432i32)));
    b.cols[275] = Col::U(AV::Num(P8::from_raw(2333641i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(false));
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
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[300] { v.push(kv.c300.lane(i)); }
        if let Col::N(v) = &mut acc.cols[301] { v.push(kv.c301.lane(i)); }
        if let Col::N(v) = &mut acc.cols[236] { v.push(kv.c236.lane(i)); }
        if let Col::N(v) = &mut acc.cols[302] { v.push(kv.c302.lane(i)); }
        if let Col::N(v) = &mut acc.cols[303] { v.push(kv.c303.lane(i)); }
        if let Col::N(v) = &mut acc.cols[238] { v.push(kv.c238.lane(i)); }
        if let Col::N(v) = &mut acc.cols[239] { v.push(kv.c239.lane(i)); }
        if let Col::V(v) = &mut acc.cols[304] {
            v.push(if kv.c304.known & (1 << i) != 0 {
                AV::Bool(kv.c304.val & (1 << i) != 0)
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
        if let Col::N(v) = &mut acc.cols[310] { v.push(sh.c310.lane(i)); }
        if let Col::N(v) = &mut acc.cols[311] { v.push(sh.c311.lane(i)); }
        if let Col::N(v) = &mut acc.cols[312] { v.push(kv.c312.lane(i)); }
        if let Col::N(v) = &mut acc.cols[313] { v.push(kv.c313.lane(i)); }
        if let Col::N(v) = &mut acc.cols[255] { v.push(kv.c255.lane(i)); }
        if let Col::N(v) = &mut acc.cols[256] { v.push(sh.c256.lane(i)); }
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
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
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
    b.cols[86] = Col::N(Vec::new());
    b.cols[233] = Col::U(AV::Bool(true));
    b.cols[261] = Col::U(AV::Bool(false));
    b.cols[262] = Col::U(AV::Bool(false));
    b.cols[263] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[264] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[265] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[266] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[241] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Bool(true));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[245] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[246] = Col::U(AV::Num(P8::from_raw(2359296i32)));
    b.cols[248] = Col::U(AV::Num(P8::from_raw(786432i32)));
    b.cols[249] = Col::U(AV::Num(P8::from_raw(2333641i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(true));
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

/// An EMPTY accumulator with outcome 3's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append3`.
pub fn acc3(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_3, OUT_GLOBALS_3, OUT_PTRS_3, 0, cart, cache);
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
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
    b.cols[86] = Col::N(Vec::new());
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
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(true));
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

/// An EMPTY accumulator with outcome 4's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append4`.
pub fn acc4(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_4, OUT_GLOBALS_4, OUT_PTRS_4, 0, cart, cache);
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
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
    b.cols[86] = Col::N(Vec::new());
    b.cols[233] = Col::U(AV::Bool(true));
    b.cols[234] = Col::N(Vec::new());
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
    b.cols[246] = Col::N(Vec::new());
    b.cols[273] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[249] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[250] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::V(Vec::new());
    for (cell, _) in OUT_UBOOL_4 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// Append this assignment's lanes that TAKE outcome 4 and
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
pub fn append4(
    acc: &mut Rt2, sh: &KShared4, kv: &KOut4, take: u16,
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
        if let Col::N(v) = &mut acc.cols[234] { v.push(kv.c234.lane(i)); }
        if let Col::N(v) = &mut acc.cols[246] { v.push(kv.c246.lane(i)); }
        if let Col::N(v) = &mut acc.cols[250] { v.push(kv.c250.lane(i)); }
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

/// An EMPTY accumulator with outcome 5's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append5`.
pub fn acc5(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_5, OUT_GLOBALS_5, OUT_PTRS_5, 0, cart, cache);
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
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
    b.cols[86] = Col::N(Vec::new());
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
    b.cols[318] = Col::N(Vec::new());
    b.cols[319] = Col::N(Vec::new());
    b.cols[268] = Col::U(AV::Bool(true));
    b.cols[320] = Col::N(Vec::new());
    b.cols[321] = Col::N(Vec::new());
    b.cols[272] = Col::N(Vec::new());
    b.cols[273] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(false));
    for (cell, _) in OUT_UBOOL_5 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// Append this assignment's lanes that TAKE outcome 5 and
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
pub fn append5(
    acc: &mut Rt2, sh: &KShared5, kv: &KOut5, take: u16,
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
        if let Col::N(v) = &mut acc.cols[318] { v.push(sh.c318.lane(i)); }
        if let Col::N(v) = &mut acc.cols[319] { v.push(sh.c319.lane(i)); }
        if let Col::N(v) = &mut acc.cols[320] { v.push(kv.c320.lane(i)); }
        if let Col::N(v) = &mut acc.cols[321] { v.push(kv.c321.lane(i)); }
        if let Col::N(v) = &mut acc.cols[272] { v.push(kv.c272.lane(i)); }
        if let Col::N(v) = &mut acc.cols[273] { v.push(sh.c273.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

pub const OUTCOMES: usize = 6;

pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    match i {
        0 => acc0(cart, cache),
        1 => acc1(cart, cache),
        2 => acc2(cart, cache),
        3 => acc3(cart, cache),
        4 => acc4(cart, cache),
        5 => acc5(cart, cache),
        _ => panic!("outcome {} of 6", i),
    }
}

pub fn out_slots(i: usize) -> &'static [(u32, &'static str)] {
    match i {
        0 => OUT_SLOTS_0,
        1 => OUT_SLOTS_1,
        2 => OUT_SLOTS_2,
        3 => OUT_SLOTS_3,
        4 => OUT_SLOTS_4,
        5 => OUT_SLOTS_5,
        _ => panic!("outcome {} of 6", i),
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
    fn o4(&mut self, _mask: u8, take: u16, sh: &KShared4, v: &KOut4) {
        append4(&mut self.accs[4], sh, v, take, self.n, &mut self.seen[4], self.org);
    }
    fn o5(&mut self, _mask: u8, take: u16, sh: &KShared5, v: &KOut5) {
        append5(&mut self.accs[5], sh, v, take, self.n, &mut self.seen[5], self.org);
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
    fn o4(&mut self, mask: u8, take: u16, sh: &KShared4, v: &KOut4);
    fn o5(&mut self, mask: u8, take: u16, sh: &KShared5, v: &KOut5);
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
    let r_c253: ZN = rin.c253;
    let r_c255: ZN = rin.c255;
    let r_c256: ZN = rin.c256;
    let r_c258: ZN = rin.c258;
    let r_c265: ZB = ZB { val: rin.c265, known: ALL };
    let r_c266: ZB = ZB { val: rin.c266, known: ALL };
    let r_c268: ZB = ZB { val: rin.c268, known: ALL };
    let r_c272: ZN = rin.c272;
    let r_c273: ZN = rin.c273;
    let r_c298: ZB = ZB { val: rin.c298, known: ALL };
    let r_c299: ZB = ZB { val: rin.c299, known: ALL };
    let r_c304: ZN = rin.c304;
    let r_c305: ZN = rin.c305;
    let r_c306: ZN = rin.c306;
    let r_c307: ZN = rin.c307;
    let r_c308: ZN = rin.c308;
    let r_c309: ZN = rin.c309;
    let r_c310: ZN = rin.c310;
    let r_c311: ZN = rin.c311;
    let r_c312: ZB = ZB { val: rin.c312, known: ALL };
    let r_c313: ZB = ZB { val: rin.c313, known: ALL };
    let r_c318: ZN = rin.c318;
    let r_c319: ZN = rin.c319;
    let r_c320: ZN = rin.c320;
    let r_c321: ZN = rin.c321;
    let n72: ZB = zb_not(r_c42);
    let n73: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n74: ZB = zb_not(r_c298);
    let n75: ZB = zb_not(r_c299);
    let n76: bool = P8::from_raw(0i32) == u.c302;
    let n77: bool = P8::from_raw(0i32) == u.c303;
    let n78: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c304);
    let n79: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c305);
    let n80: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c306);
    let n81: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c307);
    let n82: ZB = zn_eq(zn_splat(P8::from_raw(4194304i32)), r_c245);
    let n83: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c247);
    let n84: ZB = zn_eq(zn_splat(P8::from_raw(2097152i32)), r_c248);
    let n89: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n90: ZN = zn_rem(n89, zn_splat(P8::from_raw(1966080i32)));
    let n91: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n90);
    let n108: bool = P8::from_raw(1048576i32) == u.c300;
    let n109: bool = P8::from_raw(1048576i32) == u.c301;
    let n110: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n111: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n133: ZB = zb_not(r_c313);
    let n134: bool = P8::from_raw(327680i32) == u.c314;
    let n135: bool = P8::from_raw(393216i32) == u.c315;
    let n136: bool = P8::from_raw(65536i32) == u.c316;
    let n137: bool = P8::from_raw(196608i32) == u.c317;
    let n138: ZB = zb_not(r_c43);
    let n139: ZB = zb_not(r_c38);
    let n140: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n141: ZN = zn_rem(n140, zn_splat(P8::from_raw(3932160i32)));
    let n142: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n141);
    let n143: ZN = zsel_n(n142, n110, r_c86);
    let n144: ZN = zsel_n(n91, n143, r_c86);
    let n145: ZN = zsel_n(n91, n141, r_c85);
    let n146: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c273);
    let n147: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n146);
    let n148: ZB = zn_gt(n147, zn_splat(P8::from_raw(2031616i32)));
    let n149: ZB = zn_lt(n146, zn_splat(P8::from_raw(3211264i32)));
    let n150: ZB = zn_gt(r_c253, zn_splat(P8::from_raw(0i32)));
    let n151: ZB = zn_gt(r_c320, zn_splat(P8::from_raw(0i32)));
    let n152: ZB = zn_le(r_c320, zn_splat(P8::from_raw(0i32)));
    let n153: ZB = zn_lt(r_c320, zn_splat(P8::from_raw(0i32)));
    let n154: ZB = zn_ge(r_c320, zn_splat(P8::from_raw(0i32)));
    let n156: ZN = zsel_n(n153, zn_splat(P8::from_raw(98304i32)), zn_splat(P8::from_raw(0i32)));
    let n157: ZN = zsel_n(n151, zn_splat(P8::from_raw(-98304i32)), n156);
    let n158: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n157);
    let n159: ZB = zb_not(n158);
    let n160: ZN = zn_add(r_c318, n157);
    let n161: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n160);
    let n162: ZN = zn_flr(n161);
    let n163: ZN = zn_sub(n161, zn_splat(P8::from_raw(32768i32)));
    let n164: ZN = zn_sub(n163, n162);
    let n165: ZB = zn_gt(n162, zn_splat(P8::from_raw(0i32)));
    let n166: ZB = zn_le(n162, zn_splat(P8::from_raw(0i32)));
    let n167: ZB = zn_lt(n162, zn_splat(P8::from_raw(0i32)));
    let n168: ZB = zn_ge(n162, zn_splat(P8::from_raw(0i32)));
    let n169: ZN = zsel_n(n167, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n170: ZN = zsel_n(n165, zn_splat(P8::from_raw(65536i32)), n169);
    let n171: ZN = zn_abs(n162);
    let n172: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n146);
    let n173: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n171);
    let n174: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n171);
    let n175: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n171);
    let n176: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n171);
    let n177: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n171);
    let n178: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n171);
    let n179: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n171);
    let n180: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n171);
    let n181: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n171);
    let n182: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n171);
    let n183: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n171);
    let n184: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n171);
    let n185: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n171);
    let n186: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n171);
    let n187: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n171);
    let n188: ZN = zn_add(r_c319, zn_splat(P8::from_raw(-98304i32)));
    let n189: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n188);
    let n190: ZN = zn_flr(n189);
    let n191: ZN = zn_sub(n189, zn_splat(P8::from_raw(32768i32)));
    let n192: ZN = zn_sub(n191, n190);
    let n193: ZB = zn_gt(n190, zn_splat(P8::from_raw(0i32)));
    let n194: ZB = zn_le(n190, zn_splat(P8::from_raw(0i32)));
    let n195: ZB = zn_lt(n190, zn_splat(P8::from_raw(0i32)));
    let n196: ZB = zn_ge(n190, zn_splat(P8::from_raw(0i32)));
    let n197: ZN = zsel_n(n195, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n198: ZN = zsel_n(n193, zn_splat(P8::from_raw(65536i32)), n197);
    let n199: ZN = zn_abs(n190);
    let n200: ZB = zn_gt(n198, zn_splat(P8::from_raw(0i32)));
    let n201: ZB = zn_le(n198, zn_splat(P8::from_raw(0i32)));
    let n202: ZN = zn_add(n146, n198);
    let n203: ZN = zn_add(r_c273, n198);
    let n204: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n199);
    let n205: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n199);
    let n206: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n203);
    let n207: ZN = zn_add(n198, n206);
    let n208: ZN = zn_add(n198, n203);
    let n209: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n199);
    let n210: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n199);
    let n211: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n208);
    let n212: ZN = zn_add(n198, n211);
    let n213: ZN = zn_add(n198, n208);
    let n214: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n199);
    let n215: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n199);
    let n216: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n213);
    let n217: ZN = zn_add(n198, n216);
    let n218: ZN = zn_add(n198, n213);
    let n219: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n199);
    let n220: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n199);
    let n221: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n218);
    let n222: ZN = zn_add(n198, n221);
    let n223: ZN = zn_add(n198, n218);
    let n224: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n199);
    let n225: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n199);
    let n226: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n223);
    let n227: ZN = zn_add(n198, n226);
    let n228: ZN = zn_add(n198, n223);
    let n229: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n199);
    let n230: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n199);
    let n231: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n228);
    let n232: ZN = zn_add(n198, n231);
    let n233: ZN = zn_add(n198, n228);
    let n234: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n199);
    let n235: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n199);
    let n236: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n233);
    let n237: ZN = zn_add(n198, n236);
    let n238: ZN = zn_add(n198, n233);
    let n239: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n199);
    let n240: ZB = zb_not(r_c266);
    let n241: ZB = zb_not(r_c265);
    let n242: ZB = zn_lt(r_c256, zn_splat(P8::from_raw(65536i32)));
    let n243: ZB = zn_ge(r_c256, zn_splat(P8::from_raw(65536i32)));
    let n244: ZN = zsel_n(n242, zn_splat(P8::from_raw(65536i32)), r_c256);
    let n245: ZB = zn_gt(r_c258, zn_splat(P8::from_raw(0i32)));
    let n246: ZB = zn_le(r_c258, zn_splat(P8::from_raw(0i32)));
    let n247: ZN = zn_sub(r_c258, zn_splat(P8::from_raw(65536i32)));
    let n248: ZN = zsel_n(n245, n247, r_c258);
    let n249: ZN = zn_sub(r_c253, zn_splat(P8::from_raw(65536i32)));
    let n251: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c272);
    let n252: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n251);
    let n253: ZB = zn_gt(n252, zn_splat(P8::from_raw(458752i32)));
    let n254: ZB = zb_and(n148, n253);
    let n255: ZB = zn_lt(n251, zn_splat(P8::from_raw(1638400i32)));
    let n256: ZB = zb_and(n254, n255);
    let n257: ZB = zb_and(n149, n256);
    let n258: ZB = zb_and(n111, n257);
    let n259: ZB = zb_and(n150, n258);
    let n260: ZB = zb_and(n151, n259);
    let n261: ZB = zb_and(n152, n259);
    let n262: ZB = zb_and(n153, n261);
    let n263: ZB = zb_and(n154, n261);
    let n264: ZB = zb_or(n262, n263);
    let n265: ZB = zb_or(n260, n264);
    let n266: ZB = zb_and(n158, n265);
    let n267: ZB = zb_and(n159, n265);
    let n268: ZB = zb_or(n266, n267);
    let n269: ZB = zb_and(n165, n268);
    let n270: ZB = zb_and(n166, n268);
    let n271: ZB = zb_and(n167, n270);
    let n272: ZB = zb_and(n168, n270);
    let n273: ZB = zb_or(n271, n272);
    let n274: ZB = zb_or(n269, n273);
    let n275: ZN = zn_add(n170, n251);
    let n276: ZB = zn_tile_flag_at(g.cache, g.cart, n275, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n277: ZB = zb_not(n276);
    let n278: ZB = zb_and(n274, n277);
    let n279: ZB = zb_and(n274, n276);
    let n280: ZB = zb_or(n278, n279);
    let n281: ZB = zb_and(n277, n280);
    let n282: ZB = zb_and(n276, n280);
    let n283: ZB = zb_or(n281, n282);
    let n284: ZB = zb_and(n277, n283);
    let n285: ZB = zb_and(n276, n283);
    let n286: ZN = zn_add(r_c272, n170);
    let n287: ZB = zb_and(n173, n284);
    let n288: ZB = zb_and(n174, n284);
    let n289: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n286);
    let n290: ZN = zn_add(n170, n289);
    let n291: ZB = zn_tile_flag_at(g.cache, g.cart, n290, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n292: ZB = zb_not(n291);
    let n293: ZB = zb_and(n287, n292);
    let n294: ZB = zb_and(n287, n291);
    let n295: ZB = zb_or(n293, n294);
    let n296: ZB = zb_and(n292, n295);
    let n297: ZB = zb_and(n291, n295);
    let n298: ZB = zb_or(n296, n297);
    let n299: ZB = zb_and(n292, n298);
    let n300: ZB = zb_and(n291, n298);
    let n301: ZN = zn_add(n170, n286);
    let n302: ZB = zb_and(n175, n299);
    let n303: ZB = zb_and(n176, n299);
    let n304: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n301);
    let n305: ZN = zn_add(n170, n304);
    let n306: ZB = zn_tile_flag_at(g.cache, g.cart, n305, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n307: ZB = zb_not(n306);
    let n308: ZB = zb_and(n302, n307);
    let n309: ZB = zb_and(n302, n306);
    let n310: ZB = zb_or(n308, n309);
    let n311: ZB = zb_and(n307, n310);
    let n312: ZB = zb_and(n306, n310);
    let n313: ZB = zb_or(n311, n312);
    let n314: ZB = zb_and(n307, n313);
    let n315: ZB = zb_and(n306, n313);
    let n316: ZN = zn_add(n170, n301);
    let n317: ZB = zb_and(n177, n314);
    let n318: ZB = zb_and(n178, n314);
    let n319: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n316);
    let n320: ZN = zn_add(n170, n319);
    let n321: ZB = zn_tile_flag_at(g.cache, g.cart, n320, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n322: ZB = zb_not(n321);
    let n323: ZB = zb_and(n317, n322);
    let n324: ZB = zb_and(n317, n321);
    let n325: ZB = zb_or(n323, n324);
    let n326: ZB = zb_and(n322, n325);
    let n327: ZB = zb_and(n321, n325);
    let n328: ZB = zb_or(n326, n327);
    let n329: ZB = zb_and(n322, n328);
    let n330: ZB = zb_and(n321, n328);
    let n331: ZN = zn_add(n170, n316);
    let n332: ZB = zb_and(n179, n329);
    let n333: ZB = zb_and(n180, n329);
    let n334: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n331);
    let n335: ZN = zn_add(n170, n334);
    let n336: ZB = zn_tile_flag_at(g.cache, g.cart, n335, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n337: ZB = zb_not(n336);
    let n338: ZB = zb_and(n332, n337);
    let n339: ZB = zb_and(n332, n336);
    let n340: ZB = zb_or(n338, n339);
    let n341: ZB = zb_and(n337, n340);
    let n342: ZB = zb_and(n336, n340);
    let n343: ZB = zb_or(n341, n342);
    let n344: ZB = zb_and(n337, n343);
    let n345: ZB = zb_and(n336, n343);
    let n346: ZN = zn_add(n170, n331);
    let n347: ZB = zb_and(n181, n344);
    let n348: ZB = zb_and(n182, n344);
    let n349: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n346);
    let n350: ZN = zn_add(n170, n349);
    let n351: ZB = zn_tile_flag_at(g.cache, g.cart, n350, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n352: ZB = zb_not(n351);
    let n353: ZB = zb_and(n347, n352);
    let n354: ZB = zb_and(n347, n351);
    let n355: ZB = zb_or(n353, n354);
    let n356: ZB = zb_and(n352, n355);
    let n357: ZB = zb_and(n351, n355);
    let n358: ZB = zb_or(n356, n357);
    let n359: ZB = zb_and(n352, n358);
    let n360: ZB = zb_and(n351, n358);
    let n361: ZN = zn_add(n170, n346);
    let n362: ZB = zb_and(n183, n359);
    let n363: ZB = zb_and(n184, n359);
    let n364: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n361);
    let n365: ZN = zn_add(n170, n364);
    let n366: ZB = zn_tile_flag_at(g.cache, g.cart, n365, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n367: ZB = zb_not(n366);
    let n368: ZB = zb_and(n362, n367);
    let n369: ZB = zb_and(n362, n366);
    let n370: ZB = zb_or(n368, n369);
    let n371: ZB = zb_and(n367, n370);
    let n372: ZB = zb_and(n366, n370);
    let n373: ZB = zb_or(n371, n372);
    let n374: ZB = zb_and(n367, n373);
    let n375: ZB = zb_and(n366, n373);
    let n376: ZN = zn_add(n170, n361);
    let n377: ZB = zb_and(n185, n374);
    let n378: ZB = zb_and(n186, n374);
    let n379: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n376);
    let n380: ZN = zn_add(n170, n379);
    let n381: ZB = zn_tile_flag_at(g.cache, g.cart, n380, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n382: ZB = zb_not(n381);
    let n383: ZB = zb_and(n377, n382);
    let n384: ZB = zb_and(n377, n381);
    let n385: ZB = zb_or(n383, n384);
    let n386: ZB = zb_and(n382, n385);
    let n387: ZB = zb_and(n381, n385);
    let n388: ZB = zb_or(n386, n387);
    let n389: ZB = zb_and(n382, n388);
    let n390: ZB = zb_and(n381, n388);
    let n391: ZN = zn_add(n170, n376);
    let n392: ZN = zsel_n(n381, n376, n391);
    let n393: ZN = zsel_n(n381, zn_splat(P8::from_raw(0i32)), n164);
    let n394: ZN = zsel_n(n381, zn_splat(P8::from_raw(0i32)), n157);
    let n395: ZB = zb_or(n389, n390);
    let n396: ZB = zb_or(n187, n381);
    let n397: ZN = zsel_n(n186, n376, n392);
    let n398: ZN = zsel_n(n186, n164, n393);
    let n399: ZN = zsel_n(n186, n157, n394);
    let n400: ZB = zb_or(n378, n395);
    let n401: ZB = zb_or(n186, n396);
    let n402: ZN = zsel_n(n366, n361, n397);
    let n403: ZN = zsel_n(n366, zn_splat(P8::from_raw(0i32)), n398);
    let n404: ZN = zsel_n(n366, zn_splat(P8::from_raw(0i32)), n399);
    let n405: ZB = zb_or(n375, n400);
    let n406: ZB = zb_or(n366, n401);
    let n407: ZN = zsel_n(n184, n361, n402);
    let n408: ZN = zsel_n(n184, n164, n403);
    let n409: ZN = zsel_n(n184, n157, n404);
    let n410: ZB = zb_or(n363, n405);
    let n411: ZB = zb_or(n184, n406);
    let n412: ZN = zsel_n(n351, n346, n407);
    let n413: ZN = zsel_n(n351, zn_splat(P8::from_raw(0i32)), n408);
    let n414: ZN = zsel_n(n351, zn_splat(P8::from_raw(0i32)), n409);
    let n415: ZB = zb_or(n360, n410);
    let n416: ZB = zb_or(n351, n411);
    let n417: ZN = zsel_n(n182, n346, n412);
    let n418: ZN = zsel_n(n182, n164, n413);
    let n419: ZN = zsel_n(n182, n157, n414);
    let n420: ZB = zb_or(n348, n415);
    let n421: ZB = zb_or(n182, n416);
    let n422: ZN = zsel_n(n336, n331, n417);
    let n423: ZN = zsel_n(n336, zn_splat(P8::from_raw(0i32)), n418);
    let n424: ZN = zsel_n(n336, zn_splat(P8::from_raw(0i32)), n419);
    let n425: ZB = zb_or(n345, n420);
    let n426: ZB = zb_or(n336, n421);
    let n427: ZN = zsel_n(n180, n331, n422);
    let n428: ZN = zsel_n(n180, n164, n423);
    let n429: ZN = zsel_n(n180, n157, n424);
    let n430: ZB = zb_or(n333, n425);
    let n431: ZB = zb_or(n180, n426);
    let n432: ZN = zsel_n(n321, n316, n427);
    let n433: ZN = zsel_n(n321, zn_splat(P8::from_raw(0i32)), n428);
    let n434: ZN = zsel_n(n321, zn_splat(P8::from_raw(0i32)), n429);
    let n435: ZB = zb_or(n330, n430);
    let n436: ZB = zb_or(n321, n431);
    let n437: ZN = zsel_n(n178, n316, n432);
    let n438: ZN = zsel_n(n178, n164, n433);
    let n439: ZN = zsel_n(n178, n157, n434);
    let n440: ZB = zb_or(n318, n435);
    let n441: ZB = zb_or(n178, n436);
    let n442: ZN = zsel_n(n306, n301, n437);
    let n443: ZN = zsel_n(n306, zn_splat(P8::from_raw(0i32)), n438);
    let n444: ZN = zsel_n(n306, zn_splat(P8::from_raw(0i32)), n439);
    let n445: ZB = zb_or(n315, n440);
    let n446: ZB = zb_or(n306, n441);
    let n447: ZN = zsel_n(n176, n301, n442);
    let n448: ZN = zsel_n(n176, n164, n443);
    let n449: ZN = zsel_n(n176, n157, n444);
    let n450: ZB = zb_or(n303, n445);
    let n451: ZB = zb_or(n176, n446);
    let n452: ZN = zsel_n(n291, n286, n447);
    let n453: ZN = zsel_n(n291, zn_splat(P8::from_raw(0i32)), n448);
    let n454: ZN = zsel_n(n291, zn_splat(P8::from_raw(0i32)), n449);
    let n455: ZB = zb_or(n300, n450);
    let n456: ZB = zb_or(n291, n451);
    let n457: ZN = zsel_n(n174, n286, n452);
    let n458: ZN = zsel_n(n174, n164, n453);
    let n459: ZN = zsel_n(n174, n157, n454);
    let n460: ZB = zb_or(n288, n455);
    let n461: ZB = zb_or(n174, n456);
    let n462: ZN = zsel_n(n276, r_c272, n457);
    let n463: ZN = zsel_n(n276, zn_splat(P8::from_raw(0i32)), n458);
    let n464: ZN = zsel_n(n276, zn_splat(P8::from_raw(0i32)), n459);
    let n465: ZB = zb_or(n285, n460);
    let n466: ZB = zb_or(n276, n461);
    let n467: ZB = zb_and(n193, n465);
    let n468: ZB = zb_and(n194, n465);
    let n469: ZB = zb_and(n195, n468);
    let n470: ZB = zb_and(n196, n468);
    let n471: ZB = zb_or(n469, n470);
    let n472: ZB = zb_or(n467, n471);
    let n473: ZB = zb_and(n200, n472);
    let n474: ZB = zb_and(n201, n472);
    let n475: ZB = zb_or(n473, n474);
    let n476: ZB = zb_and(n200, n475);
    let n477: ZB = zb_and(n201, n475);
    let n478: ZB = zb_or(n476, n477);
    let n479: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n462);
    let n480: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n479);
    let n481: ZB = zn_tile_flag_at(g.cache, g.cart, n480, n202, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n482: ZB = zb_not(n481);
    let n483: ZB = zb_and(n478, n482);
    let n484: ZB = zb_and(n478, n481);
    let n485: ZB = zb_or(n483, n484);
    let n486: ZB = zb_and(n482, n485);
    let n487: ZB = zb_and(n481, n485);
    let n488: ZB = zb_or(n486, n487);
    let n489: ZB = zb_and(n482, n488);
    let n490: ZB = zb_and(n481, n488);
    let n491: ZB = zb_and(n204, n489);
    let n492: ZB = zb_and(n205, n489);
    let n493: ZB = zb_and(n200, n491);
    let n494: ZB = zb_and(n201, n491);
    let n495: ZB = zb_or(n493, n494);
    let n496: ZB = zb_and(n200, n495);
    let n497: ZB = zb_and(n201, n495);
    let n498: ZB = zb_or(n496, n497);
    let n499: ZB = zn_tile_flag_at(g.cache, g.cart, n480, n207, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n500: ZB = zb_not(n499);
    let n501: ZB = zb_and(n498, n500);
    let n502: ZB = zb_and(n498, n499);
    let n503: ZB = zb_or(n501, n502);
    let n504: ZB = zb_and(n500, n503);
    let n505: ZB = zb_and(n499, n503);
    let n506: ZB = zb_or(n504, n505);
    let n507: ZB = zb_and(n500, n506);
    let n508: ZB = zb_and(n499, n506);
    let n509: ZB = zb_and(n209, n507);
    let n510: ZB = zb_and(n210, n507);
    let n511: ZB = zb_and(n200, n509);
    let n512: ZB = zb_and(n201, n509);
    let n513: ZB = zb_or(n511, n512);
    let n514: ZB = zb_and(n200, n513);
    let n515: ZB = zb_and(n201, n513);
    let n516: ZB = zb_or(n514, n515);
    let n517: ZB = zn_tile_flag_at(g.cache, g.cart, n480, n212, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n518: ZB = zb_not(n517);
    let n519: ZB = zb_and(n516, n518);
    let n520: ZB = zb_and(n516, n517);
    let n521: ZB = zb_or(n519, n520);
    let n522: ZB = zb_and(n518, n521);
    let n523: ZB = zb_and(n517, n521);
    let n524: ZB = zb_or(n522, n523);
    let n525: ZB = zb_and(n518, n524);
    let n526: ZB = zb_and(n517, n524);
    let n527: ZB = zb_and(n214, n525);
    let n528: ZB = zb_and(n215, n525);
    let n529: ZB = zb_and(n200, n527);
    let n530: ZB = zb_and(n201, n527);
    let n531: ZB = zb_or(n529, n530);
    let n532: ZB = zb_and(n200, n531);
    let n533: ZB = zb_and(n201, n531);
    let n534: ZB = zb_or(n532, n533);
    let n535: ZB = zn_tile_flag_at(g.cache, g.cart, n480, n217, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n536: ZB = zb_not(n535);
    let n537: ZB = zb_and(n534, n536);
    let n538: ZB = zb_and(n534, n535);
    let n539: ZB = zb_or(n537, n538);
    let n540: ZB = zb_and(n536, n539);
    let n541: ZB = zb_and(n535, n539);
    let n542: ZB = zb_or(n540, n541);
    let n543: ZB = zb_and(n536, n542);
    let n544: ZB = zb_and(n535, n542);
    let n545: ZB = zb_and(n219, n543);
    let n546: ZB = zb_and(n220, n543);
    let n547: ZB = zb_and(n200, n545);
    let n548: ZB = zb_and(n201, n545);
    let n549: ZB = zb_or(n547, n548);
    let n550: ZB = zb_and(n200, n549);
    let n551: ZB = zb_and(n201, n549);
    let n552: ZB = zb_or(n550, n551);
    let n553: ZB = zn_tile_flag_at(g.cache, g.cart, n480, n222, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n554: ZB = zb_not(n553);
    let n555: ZB = zb_and(n552, n554);
    let n556: ZB = zb_and(n552, n553);
    let n557: ZB = zb_or(n555, n556);
    let n558: ZB = zb_and(n554, n557);
    let n559: ZB = zb_and(n553, n557);
    let n560: ZB = zb_or(n558, n559);
    let n561: ZB = zb_and(n554, n560);
    let n562: ZB = zb_and(n553, n560);
    let n563: ZB = zb_and(n224, n561);
    let n564: ZB = zb_and(n225, n561);
    let n565: ZB = zb_and(n200, n563);
    let n566: ZB = zb_and(n201, n563);
    let n567: ZB = zb_or(n565, n566);
    let n568: ZB = zb_and(n200, n567);
    let n569: ZB = zb_and(n201, n567);
    let n570: ZB = zb_or(n568, n569);
    let n571: ZB = zn_tile_flag_at(g.cache, g.cart, n480, n227, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n572: ZB = zb_not(n571);
    let n573: ZB = zb_and(n570, n572);
    let n574: ZB = zb_and(n570, n571);
    let n575: ZB = zb_or(n573, n574);
    let n576: ZB = zb_and(n572, n575);
    let n577: ZB = zb_and(n571, n575);
    let n578: ZB = zb_or(n576, n577);
    let n579: ZB = zb_and(n572, n578);
    let n580: ZB = zb_and(n571, n578);
    let n581: ZB = zb_and(n229, n579);
    let n582: ZB = zb_and(n230, n579);
    let n583: ZB = zb_and(n200, n581);
    let n584: ZB = zb_and(n201, n581);
    let n585: ZB = zb_or(n583, n584);
    let n586: ZB = zb_and(n200, n585);
    let n587: ZB = zb_and(n201, n585);
    let n588: ZB = zb_or(n586, n587);
    let n589: ZB = zn_tile_flag_at(g.cache, g.cart, n480, n232, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n590: ZB = zb_not(n589);
    let n591: ZB = zb_and(n588, n590);
    let n592: ZB = zb_and(n588, n589);
    let n593: ZB = zb_or(n591, n592);
    let n594: ZB = zb_and(n590, n593);
    let n595: ZB = zb_and(n589, n593);
    let n596: ZB = zb_or(n594, n595);
    let n597: ZB = zb_and(n590, n596);
    let n598: ZB = zb_and(n589, n596);
    let n599: ZB = zb_and(n234, n597);
    let n600: ZB = zb_and(n235, n597);
    let n601: ZB = zb_and(n200, n599);
    let n602: ZB = zb_and(n201, n599);
    let n603: ZB = zb_or(n601, n602);
    let n604: ZB = zb_and(n200, n603);
    let n605: ZB = zb_and(n201, n603);
    let n606: ZB = zb_or(n604, n605);
    let n607: ZB = zn_tile_flag_at(g.cache, g.cart, n480, n237, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n608: ZB = zb_not(n607);
    let n609: ZB = zb_and(n606, n608);
    let n610: ZB = zb_and(n606, n607);
    let n611: ZB = zb_or(n609, n610);
    let n612: ZB = zb_and(n608, n611);
    let n613: ZB = zb_and(n607, n611);
    let n614: ZB = zb_or(n612, n613);
    let n615: ZB = zb_and(n608, n614);
    let n616: ZB = zb_and(n607, n614);
    let n617: ZB = zb_and(n239, n466);
    let n618: ZN = zsel_n(n607, n233, n238);
    let n619: ZN = zsel_n(n607, zn_splat(P8::from_raw(0i32)), n192);
    let n620: ZN = zsel_n(n607, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(-98304i32)));
    let n621: ZB = zb_or(n615, n616);
    let n622: ZB = zsel_b(n607, n466, n617);
    let n623: ZN = zsel_n(n235, n233, n618);
    let n624: ZN = zsel_n(n235, n192, n619);
    let n625: ZN = zsel_n(n235, zn_splat(P8::from_raw(-98304i32)), n620);
    let n626: ZB = zb_or(n600, n621);
    let n627: ZB = zsel_b(n235, n466, n622);
    let n628: ZN = zsel_n(n589, n228, n623);
    let n629: ZN = zsel_n(n589, zn_splat(P8::from_raw(0i32)), n624);
    let n630: ZN = zsel_n(n589, zn_splat(P8::from_raw(0i32)), n625);
    let n631: ZB = zb_or(n598, n626);
    let n632: ZB = zsel_b(n589, n466, n627);
    let n633: ZN = zsel_n(n230, n228, n628);
    let n634: ZN = zsel_n(n230, n192, n629);
    let n635: ZN = zsel_n(n230, zn_splat(P8::from_raw(-98304i32)), n630);
    let n636: ZB = zb_or(n582, n631);
    let n637: ZB = zsel_b(n230, n466, n632);
    let n638: ZN = zsel_n(n571, n223, n633);
    let n639: ZN = zsel_n(n571, zn_splat(P8::from_raw(0i32)), n634);
    let n640: ZN = zsel_n(n571, zn_splat(P8::from_raw(0i32)), n635);
    let n641: ZB = zb_or(n580, n636);
    let n642: ZB = zsel_b(n571, n466, n637);
    let n643: ZN = zsel_n(n225, n223, n638);
    let n644: ZN = zsel_n(n225, n192, n639);
    let n645: ZN = zsel_n(n225, zn_splat(P8::from_raw(-98304i32)), n640);
    let n646: ZB = zb_or(n564, n641);
    let n647: ZB = zsel_b(n225, n466, n642);
    let n648: ZN = zsel_n(n553, n218, n643);
    let n649: ZN = zsel_n(n553, zn_splat(P8::from_raw(0i32)), n644);
    let n650: ZN = zsel_n(n553, zn_splat(P8::from_raw(0i32)), n645);
    let n651: ZB = zb_or(n562, n646);
    let n652: ZB = zsel_b(n553, n466, n647);
    let n653: ZN = zsel_n(n220, n218, n648);
    let n654: ZN = zsel_n(n220, n192, n649);
    let n655: ZN = zsel_n(n220, zn_splat(P8::from_raw(-98304i32)), n650);
    let n656: ZB = zb_or(n546, n651);
    let n657: ZB = zsel_b(n220, n466, n652);
    let n658: ZN = zsel_n(n535, n213, n653);
    let n659: ZN = zsel_n(n535, zn_splat(P8::from_raw(0i32)), n654);
    let n660: ZN = zsel_n(n535, zn_splat(P8::from_raw(0i32)), n655);
    let n661: ZB = zb_or(n544, n656);
    let n662: ZB = zsel_b(n535, n466, n657);
    let n663: ZN = zsel_n(n215, n213, n658);
    let n664: ZN = zsel_n(n215, n192, n659);
    let n665: ZN = zsel_n(n215, zn_splat(P8::from_raw(-98304i32)), n660);
    let n666: ZB = zb_or(n528, n661);
    let n667: ZB = zsel_b(n215, n466, n662);
    let n668: ZN = zsel_n(n517, n208, n663);
    let n669: ZN = zsel_n(n517, zn_splat(P8::from_raw(0i32)), n664);
    let n670: ZN = zsel_n(n517, zn_splat(P8::from_raw(0i32)), n665);
    let n671: ZB = zb_or(n526, n666);
    let n672: ZB = zsel_b(n517, n466, n667);
    let n673: ZN = zsel_n(n210, n208, n668);
    let n674: ZN = zsel_n(n210, n192, n669);
    let n675: ZN = zsel_n(n210, zn_splat(P8::from_raw(-98304i32)), n670);
    let n676: ZB = zb_or(n510, n671);
    let n677: ZB = zsel_b(n210, n466, n672);
    let n678: ZN = zsel_n(n499, n203, n673);
    let n679: ZN = zsel_n(n499, zn_splat(P8::from_raw(0i32)), n674);
    let n680: ZN = zsel_n(n499, zn_splat(P8::from_raw(0i32)), n675);
    let n681: ZB = zb_or(n508, n676);
    let n682: ZB = zsel_b(n499, n466, n677);
    let n683: ZN = zsel_n(n205, n203, n678);
    let n684: ZN = zsel_n(n205, n192, n679);
    let n685: ZN = zsel_n(n205, zn_splat(P8::from_raw(-98304i32)), n680);
    let n686: ZB = zb_or(n492, n681);
    let n687: ZB = zsel_b(n205, n466, n682);
    let n688: ZN = zsel_n(n481, r_c273, n683);
    let n689: ZN = zsel_n(n481, zn_splat(P8::from_raw(0i32)), n684);
    let n690: ZN = zsel_n(n481, zn_splat(P8::from_raw(0i32)), n685);
    let n691: ZB = zb_or(n490, n686);
    let n692: ZB = zsel_b(n481, n466, n687);
    let n693: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n688);
    let n694: ZN = zn_div(n479, zn_splat(P8::from_raw(524288i32)));
    let n695: ZN = zn_flr(n694);
    let n696: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n695);
    let n697: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n479);
    let n698: ZN = zn_sub(n697, zn_splat(P8::from_raw(65536i32)));
    let n699: ZN = zn_div(n698, zn_splat(P8::from_raw(524288i32)));
    let n700: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n699);
    let n701: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n696);
    let n702: ZB = zn_le(n701, n700);
    let n703: ZB = zn_gt(n701, n700);
    let n704: ZB = zb_and(n691, n702);
    let n705: ZB = zb_and(n691, n703);
    let n706: ZN = zn_div(n693, zn_splat(P8::from_raw(524288i32)));
    let n707: ZN = zn_flr(n706);
    let n708: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n707);
    let n709: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n693);
    let n710: ZN = zn_sub(n709, zn_splat(P8::from_raw(65536i32)));
    let n711: ZN = zn_div(n710, zn_splat(P8::from_raw(524288i32)));
    let n712: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n711);
    let n713: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n708);
    let n714: ZB = zn_le(n713, n712);
    let n715: ZB = zn_gt(n713, n712);
    let n716: ZB = zb_and(n704, n714);
    let n717: ZB = zb_and(n704, n715);
    let n718: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n701);
    let n719: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n713);
    let n720: ZN = zn_mget(g.cart, n718, n719);
    let n721: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n720);
    let n722: ZB = zb_not(n721);
    let n723: ZB = zb_and(n716, n721);
    let n724: ZB = zb_and(n716, n722);
    let n725: ZN = zn_rem(n710, zn_splat(P8::from_raw(524288i32)));
    let n726: ZB = zn_ge(n725, zn_splat(P8::from_raw(393216i32)));
    let n727: ZB = zn_lt(n725, zn_splat(P8::from_raw(393216i32)));
    let n728: ZB = zb_and(n723, n727);
    let n729: ZB = zb_and(n723, n726);
    let n730: ZN = zn_mul(n713, zn_splat(P8::from_raw(524288i32)));
    let n731: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n730);
    let n732: ZB = zn_eq(n709, n731);
    let n733: ZB = zb_or(n728, n729);
    let n734: ZB = zb_or(n726, n732);
    let n735: ZB = zb_or(n724, n733);
    let n736: ZB = zb_and(n721, n734);
    let n737: ZB = zb_not(n736);
    let n738: ZB = zb_and(n735, n736);
    let n739: ZB = zb_and(n735, n737);
    let n740: ZB = zn_ge(n690, zn_splat(P8::from_raw(0i32)));
    let n741: ZB = zb_or(n738, n739);
    let n742: ZB = zb_and(n736, n740);
    let n743: ZB = zb_not(n742);
    let n744: ZB = zb_and(n741, n743);
    let n745: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n720);
    let n746: ZB = zb_not(n745);
    let n747: ZB = zb_and(n744, n745);
    let n748: ZB = zb_and(n744, n746);
    let n749: ZN = zn_rem(n693, zn_splat(P8::from_raw(524288i32)));
    let n750: ZB = zn_le(n749, zn_splat(P8::from_raw(131072i32)));
    let n751: ZB = zb_or(n747, n748);
    let n752: ZB = zb_and(n745, n750);
    let n753: ZB = zb_not(n752);
    let n754: ZB = zb_and(n751, n752);
    let n755: ZB = zb_and(n751, n753);
    let n756: ZB = zb_or(n754, n755);
    let n757: ZB = zb_and(n753, n756);
    let n758: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n720);
    let n759: ZB = zb_not(n758);
    let n760: ZB = zb_and(n757, n758);
    let n761: ZB = zb_and(n757, n759);
    let n762: ZN = zn_rem(n479, zn_splat(P8::from_raw(524288i32)));
    let n763: ZB = zn_le(n762, zn_splat(P8::from_raw(131072i32)));
    let n764: ZB = zb_or(n760, n761);
    let n765: ZB = zb_and(n758, n763);
    let n766: ZB = zb_not(n765);
    let n767: ZB = zb_and(n764, n765);
    let n768: ZB = zb_and(n764, n766);
    let n769: ZB = zn_le(n464, zn_splat(P8::from_raw(0i32)));
    let n770: ZB = zb_or(n767, n768);
    let n771: ZB = zb_and(n765, n769);
    let n772: ZB = zb_not(n771);
    let n773: ZB = zb_and(n770, n772);
    let n774: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n720);
    let n775: ZB = zb_not(n774);
    let n776: ZB = zb_and(n773, n774);
    let n777: ZB = zb_and(n773, n775);
    let n778: ZN = zn_rem(n698, zn_splat(P8::from_raw(524288i32)));
    let n779: ZB = zn_ge(n778, zn_splat(P8::from_raw(393216i32)));
    let n780: ZB = zn_lt(n778, zn_splat(P8::from_raw(393216i32)));
    let n781: ZB = zb_and(n776, n780);
    let n782: ZB = zb_and(n776, n779);
    let n783: ZN = zn_mul(n701, zn_splat(P8::from_raw(524288i32)));
    let n784: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n783);
    let n785: ZB = zn_eq(n697, n784);
    let n786: ZB = zb_or(n781, n782);
    let n787: ZB = zb_or(n779, n785);
    let n788: ZB = zb_or(n777, n786);
    let n789: ZB = zb_and(n774, n787);
    let n790: ZB = zb_not(n789);
    let n791: ZB = zb_and(n788, n789);
    let n792: ZB = zb_and(n788, n790);
    let n793: ZB = zn_ge(n464, zn_splat(P8::from_raw(0i32)));
    let n794: ZB = zb_or(n791, n792);
    let n795: ZB = zb_and(n789, n793);
    let n796: ZB = zb_not(n795);
    let n797: ZB = zb_and(n794, n796);
    let n798: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n708);
    let n799: ZB = zn_le(n798, n712);
    let n800: ZB = zn_gt(n798, n712);
    let n801: ZB = zb_and(n797, n799);
    let n802: ZB = zb_and(n797, n800);
    let n803: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n798);
    let n804: ZN = zn_mget(g.cart, n718, n803);
    let n805: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n804);
    let n806: ZB = zb_not(n805);
    let n807: ZB = zb_and(n801, n805);
    let n808: ZB = zb_and(n801, n806);
    let n809: ZB = zb_and(n727, n807);
    let n810: ZB = zb_and(n726, n807);
    let n811: ZN = zn_mul(n798, zn_splat(P8::from_raw(524288i32)));
    let n812: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n811);
    let n813: ZB = zn_eq(n709, n812);
    let n814: ZB = zb_or(n809, n810);
    let n815: ZB = zb_or(n726, n813);
    let n816: ZB = zb_or(n808, n814);
    let n817: ZB = zb_and(n805, n815);
    let n818: ZB = zb_not(n817);
    let n819: ZB = zb_and(n816, n817);
    let n820: ZB = zb_and(n816, n818);
    let n821: ZB = zb_or(n819, n820);
    let n822: ZB = zb_and(n740, n817);
    let n823: ZB = zb_not(n822);
    let n824: ZB = zb_and(n821, n823);
    let n825: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n804);
    let n826: ZB = zb_not(n825);
    let n827: ZB = zb_and(n824, n825);
    let n828: ZB = zb_and(n824, n826);
    let n829: ZB = zb_or(n827, n828);
    let n830: ZB = zb_and(n750, n825);
    let n831: ZB = zb_not(n830);
    let n832: ZB = zb_and(n829, n830);
    let n833: ZB = zb_and(n829, n831);
    let n834: ZB = zb_or(n832, n833);
    let n835: ZB = zb_and(n831, n834);
    let n836: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n804);
    let n837: ZB = zb_not(n836);
    let n838: ZB = zb_and(n835, n836);
    let n839: ZB = zb_and(n835, n837);
    let n840: ZB = zb_or(n838, n839);
    let n841: ZB = zb_and(n763, n836);
    let n842: ZB = zb_not(n841);
    let n843: ZB = zb_and(n840, n841);
    let n844: ZB = zb_and(n840, n842);
    let n845: ZB = zb_or(n843, n844);
    let n846: ZB = zb_and(n769, n841);
    let n847: ZB = zb_not(n846);
    let n848: ZB = zb_and(n845, n847);
    let n849: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n804);
    let n850: ZB = zb_not(n849);
    let n851: ZB = zb_and(n848, n849);
    let n852: ZB = zb_and(n848, n850);
    let n853: ZB = zb_and(n780, n851);
    let n854: ZB = zb_and(n779, n851);
    let n855: ZB = zb_or(n853, n854);
    let n856: ZB = zb_or(n852, n855);
    let n857: ZB = zb_and(n787, n849);
    let n858: ZB = zb_not(n857);
    let n859: ZB = zb_and(n856, n857);
    let n860: ZB = zb_and(n856, n858);
    let n861: ZB = zb_or(n859, n860);
    let n862: ZB = zb_and(n793, n857);
    let n863: ZB = zb_not(n862);
    let n864: ZB = zb_and(n861, n863);
    let n865: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n708);
    let n866: ZB = zn_le(n865, n712);
    let n867: ZB = zn_gt(n865, n712);
    let n868: ZB = zb_and(n864, n866);
    let n869: ZB = zb_and(n864, n867);
    let n870: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n865);
    let n871: ZN = zn_mget(g.cart, n718, n870);
    let n872: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n871);
    let n873: ZB = zb_not(n872);
    let n874: ZB = zb_and(n868, n872);
    let n875: ZB = zb_and(n868, n873);
    let n876: ZB = zb_and(n727, n874);
    let n877: ZB = zb_and(n726, n874);
    let n878: ZN = zn_mul(n865, zn_splat(P8::from_raw(524288i32)));
    let n879: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n878);
    let n880: ZB = zn_eq(n709, n879);
    let n881: ZB = zb_or(n876, n877);
    let n882: ZB = zb_or(n726, n880);
    let n883: ZB = zb_or(n875, n881);
    let n884: ZB = zb_and(n872, n882);
    let n885: ZB = zb_not(n884);
    let n886: ZB = zb_and(n883, n884);
    let n887: ZB = zb_and(n883, n885);
    let n888: ZB = zb_or(n886, n887);
    let n889: ZB = zb_and(n740, n884);
    let n890: ZB = zb_not(n889);
    let n891: ZB = zb_and(n888, n890);
    let n892: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n871);
    let n893: ZB = zb_not(n892);
    let n894: ZB = zb_and(n891, n892);
    let n895: ZB = zb_and(n891, n893);
    let n896: ZB = zb_or(n894, n895);
    let n897: ZB = zb_and(n750, n892);
    let n898: ZB = zb_not(n897);
    let n899: ZB = zb_and(n896, n897);
    let n900: ZB = zb_and(n896, n898);
    let n901: ZB = zb_or(n899, n900);
    let n902: ZB = zb_and(n898, n901);
    let n903: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n871);
    let n904: ZB = zb_not(n903);
    let n905: ZB = zb_and(n902, n903);
    let n906: ZB = zb_and(n902, n904);
    let n907: ZB = zb_or(n905, n906);
    let n908: ZB = zb_and(n763, n903);
    let n909: ZB = zb_not(n908);
    let n910: ZB = zb_and(n907, n908);
    let n911: ZB = zb_and(n907, n909);
    let n912: ZB = zb_or(n910, n911);
    let n913: ZB = zb_and(n769, n908);
    let n914: ZB = zb_not(n913);
    let n915: ZB = zb_and(n912, n914);
    let n916: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n871);
    let n917: ZB = zb_not(n916);
    let n918: ZB = zb_and(n915, n916);
    let n919: ZB = zb_and(n915, n917);
    let n920: ZB = zb_and(n780, n918);
    let n921: ZB = zb_and(n779, n918);
    let n922: ZB = zb_or(n920, n921);
    let n923: ZB = zb_or(n919, n922);
    let n924: ZB = zb_and(n787, n916);
    let n925: ZB = zb_not(n924);
    let n926: ZB = zb_and(n923, n924);
    let n927: ZB = zb_and(n923, n925);
    let n928: ZB = zb_or(n926, n927);
    let n929: ZB = zb_and(n793, n924);
    let n930: ZB = zb_not(n929);
    let n931: ZB = zb_and(n928, n930);
    let n932: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n708);
    let n933: ZB = zn_gt(n932, n712);
    let n934: ZB = zb_and(n692, n933);
    let n935: ZB = zb_or(n869, n931);
    let n936: ZB = zsel_b(n867, n692, n934);
    let n937: ZB = zb_or(n802, n935);
    let n938: ZB = zsel_b(n800, n692, n936);
    let n939: ZB = zb_or(n717, n937);
    let n940: ZB = zsel_b(n715, n692, n938);
    let n941: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n696);
    let n942: ZB = zn_le(n941, n700);
    let n943: ZB = zn_gt(n941, n700);
    let n944: ZB = zb_and(n939, n942);
    let n945: ZB = zb_and(n939, n943);
    let n946: ZB = zb_and(n714, n944);
    let n947: ZB = zb_and(n715, n944);
    let n948: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n941);
    let n949: ZN = zn_mget(g.cart, n948, n719);
    let n950: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n949);
    let n951: ZB = zb_not(n950);
    let n952: ZB = zb_and(n946, n950);
    let n953: ZB = zb_and(n946, n951);
    let n954: ZB = zb_and(n727, n952);
    let n955: ZB = zb_and(n726, n952);
    let n956: ZB = zb_or(n954, n955);
    let n957: ZB = zb_or(n953, n956);
    let n958: ZB = zb_and(n734, n950);
    let n959: ZB = zb_not(n958);
    let n960: ZB = zb_and(n957, n958);
    let n961: ZB = zb_and(n957, n959);
    let n962: ZB = zb_or(n960, n961);
    let n963: ZB = zb_and(n740, n958);
    let n964: ZB = zb_not(n963);
    let n965: ZB = zb_and(n962, n964);
    let n966: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n949);
    let n967: ZB = zb_not(n966);
    let n968: ZB = zb_and(n965, n966);
    let n969: ZB = zb_and(n965, n967);
    let n970: ZB = zb_or(n968, n969);
    let n971: ZB = zb_and(n750, n966);
    let n972: ZB = zb_not(n971);
    let n973: ZB = zb_and(n970, n971);
    let n974: ZB = zb_and(n970, n972);
    let n975: ZB = zb_or(n973, n974);
    let n976: ZB = zb_and(n972, n975);
    let n977: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n949);
    let n978: ZB = zb_not(n977);
    let n979: ZB = zb_and(n976, n977);
    let n980: ZB = zb_and(n976, n978);
    let n981: ZB = zb_or(n979, n980);
    let n982: ZB = zb_and(n763, n977);
    let n983: ZB = zb_not(n982);
    let n984: ZB = zb_and(n981, n982);
    let n985: ZB = zb_and(n981, n983);
    let n986: ZB = zb_or(n984, n985);
    let n987: ZB = zb_and(n769, n982);
    let n988: ZB = zb_not(n987);
    let n989: ZB = zb_and(n986, n988);
    let n990: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n949);
    let n991: ZB = zb_not(n990);
    let n992: ZB = zb_and(n989, n990);
    let n993: ZB = zb_and(n989, n991);
    let n994: ZB = zb_and(n780, n992);
    let n995: ZB = zb_and(n779, n992);
    let n996: ZN = zn_mul(n941, zn_splat(P8::from_raw(524288i32)));
    let n997: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n996);
    let n998: ZB = zn_eq(n697, n997);
    let n999: ZB = zb_or(n994, n995);
    let n1000: ZB = zb_or(n779, n998);
    let n1001: ZB = zb_or(n993, n999);
    let n1002: ZB = zb_and(n990, n1000);
    let n1003: ZB = zb_not(n1002);
    let n1004: ZB = zb_and(n1001, n1002);
    let n1005: ZB = zb_and(n1001, n1003);
    let n1006: ZB = zb_or(n1004, n1005);
    let n1007: ZB = zb_and(n793, n1002);
    let n1008: ZB = zb_not(n1007);
    let n1009: ZB = zb_and(n1006, n1008);
    let n1010: ZB = zb_and(n799, n1009);
    let n1011: ZB = zb_and(n800, n1009);
    let n1012: ZN = zn_mget(g.cart, n948, n803);
    let n1013: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1012);
    let n1014: ZB = zb_not(n1013);
    let n1015: ZB = zb_and(n1010, n1013);
    let n1016: ZB = zb_and(n1010, n1014);
    let n1017: ZB = zb_and(n727, n1015);
    let n1018: ZB = zb_and(n726, n1015);
    let n1019: ZB = zb_or(n1017, n1018);
    let n1020: ZB = zb_or(n1016, n1019);
    let n1021: ZB = zb_and(n815, n1013);
    let n1022: ZB = zb_not(n1021);
    let n1023: ZB = zb_and(n1020, n1021);
    let n1024: ZB = zb_and(n1020, n1022);
    let n1025: ZB = zb_or(n1023, n1024);
    let n1026: ZB = zb_and(n740, n1021);
    let n1027: ZB = zb_not(n1026);
    let n1028: ZB = zb_and(n1025, n1027);
    let n1029: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1012);
    let n1030: ZB = zb_not(n1029);
    let n1031: ZB = zb_and(n1028, n1029);
    let n1032: ZB = zb_and(n1028, n1030);
    let n1033: ZB = zb_or(n1031, n1032);
    let n1034: ZB = zb_and(n750, n1029);
    let n1035: ZB = zb_not(n1034);
    let n1036: ZB = zb_and(n1033, n1034);
    let n1037: ZB = zb_and(n1033, n1035);
    let n1038: ZB = zb_or(n1036, n1037);
    let n1039: ZB = zb_and(n1035, n1038);
    let n1040: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1012);
    let n1041: ZB = zb_not(n1040);
    let n1042: ZB = zb_and(n1039, n1040);
    let n1043: ZB = zb_and(n1039, n1041);
    let n1044: ZB = zb_or(n1042, n1043);
    let n1045: ZB = zb_and(n763, n1040);
    let n1046: ZB = zb_not(n1045);
    let n1047: ZB = zb_and(n1044, n1045);
    let n1048: ZB = zb_and(n1044, n1046);
    let n1049: ZB = zb_or(n1047, n1048);
    let n1050: ZB = zb_and(n769, n1045);
    let n1051: ZB = zb_not(n1050);
    let n1052: ZB = zb_and(n1049, n1051);
    let n1053: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1012);
    let n1054: ZB = zb_not(n1053);
    let n1055: ZB = zb_and(n1052, n1053);
    let n1056: ZB = zb_and(n1052, n1054);
    let n1057: ZB = zb_and(n780, n1055);
    let n1058: ZB = zb_and(n779, n1055);
    let n1059: ZB = zb_or(n1057, n1058);
    let n1060: ZB = zb_or(n1056, n1059);
    let n1061: ZB = zb_and(n1000, n1053);
    let n1062: ZB = zb_not(n1061);
    let n1063: ZB = zb_and(n1060, n1061);
    let n1064: ZB = zb_and(n1060, n1062);
    let n1065: ZB = zb_or(n1063, n1064);
    let n1066: ZB = zb_and(n793, n1061);
    let n1067: ZB = zb_not(n1066);
    let n1068: ZB = zb_and(n1065, n1067);
    let n1069: ZB = zb_and(n866, n1068);
    let n1070: ZB = zb_and(n867, n1068);
    let n1071: ZN = zn_mget(g.cart, n948, n870);
    let n1072: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1071);
    let n1073: ZB = zb_not(n1072);
    let n1074: ZB = zb_and(n1069, n1072);
    let n1075: ZB = zb_and(n1069, n1073);
    let n1076: ZB = zb_and(n727, n1074);
    let n1077: ZB = zb_and(n726, n1074);
    let n1078: ZB = zb_or(n1076, n1077);
    let n1079: ZB = zb_or(n1075, n1078);
    let n1080: ZB = zb_and(n882, n1072);
    let n1081: ZB = zb_not(n1080);
    let n1082: ZB = zb_and(n1079, n1080);
    let n1083: ZB = zb_and(n1079, n1081);
    let n1084: ZB = zb_or(n1082, n1083);
    let n1085: ZB = zb_and(n740, n1080);
    let n1086: ZB = zb_not(n1085);
    let n1087: ZB = zb_and(n1084, n1086);
    let n1088: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1071);
    let n1089: ZB = zb_not(n1088);
    let n1090: ZB = zb_and(n1087, n1088);
    let n1091: ZB = zb_and(n1087, n1089);
    let n1092: ZB = zb_or(n1090, n1091);
    let n1093: ZB = zb_and(n750, n1088);
    let n1094: ZB = zb_not(n1093);
    let n1095: ZB = zb_and(n1092, n1093);
    let n1096: ZB = zb_and(n1092, n1094);
    let n1097: ZB = zb_or(n1095, n1096);
    let n1098: ZB = zb_and(n1094, n1097);
    let n1099: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1071);
    let n1100: ZB = zb_not(n1099);
    let n1101: ZB = zb_and(n1098, n1099);
    let n1102: ZB = zb_and(n1098, n1100);
    let n1103: ZB = zb_or(n1101, n1102);
    let n1104: ZB = zb_and(n763, n1099);
    let n1105: ZB = zb_not(n1104);
    let n1106: ZB = zb_and(n1103, n1104);
    let n1107: ZB = zb_and(n1103, n1105);
    let n1108: ZB = zb_or(n1106, n1107);
    let n1109: ZB = zb_and(n769, n1104);
    let n1110: ZB = zb_not(n1109);
    let n1111: ZB = zb_and(n1108, n1110);
    let n1112: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1071);
    let n1113: ZB = zb_not(n1112);
    let n1114: ZB = zb_and(n1111, n1112);
    let n1115: ZB = zb_and(n1111, n1113);
    let n1116: ZB = zb_and(n780, n1114);
    let n1117: ZB = zb_and(n779, n1114);
    let n1118: ZB = zb_or(n1116, n1117);
    let n1119: ZB = zb_or(n1115, n1118);
    let n1120: ZB = zb_and(n1000, n1112);
    let n1121: ZB = zb_not(n1120);
    let n1122: ZB = zb_and(n1119, n1120);
    let n1123: ZB = zb_and(n1119, n1121);
    let n1124: ZB = zb_or(n1122, n1123);
    let n1125: ZB = zb_and(n793, n1120);
    let n1126: ZB = zb_not(n1125);
    let n1127: ZB = zb_and(n1124, n1126);
    let n1128: ZB = zb_and(n933, n940);
    let n1129: ZB = zb_or(n1070, n1127);
    let n1130: ZB = zsel_b(n867, n940, n1128);
    let n1131: ZB = zb_or(n1011, n1129);
    let n1132: ZB = zsel_b(n800, n940, n1130);
    let n1133: ZB = zb_or(n947, n1131);
    let n1134: ZB = zsel_b(n715, n940, n1132);
    let n1135: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n696);
    let n1136: ZB = zn_le(n1135, n700);
    let n1137: ZB = zn_gt(n1135, n700);
    let n1138: ZB = zb_and(n1133, n1136);
    let n1139: ZB = zb_and(n1133, n1137);
    let n1140: ZB = zb_and(n714, n1138);
    let n1141: ZB = zb_and(n715, n1138);
    let n1142: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1135);
    let n1143: ZN = zn_mget(g.cart, n1142, n719);
    let n1144: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1143);
    let n1145: ZB = zb_not(n1144);
    let n1146: ZB = zb_and(n1140, n1144);
    let n1147: ZB = zb_and(n1140, n1145);
    let n1148: ZB = zb_and(n727, n1146);
    let n1149: ZB = zb_and(n726, n1146);
    let n1150: ZB = zb_or(n1148, n1149);
    let n1151: ZB = zb_or(n1147, n1150);
    let n1152: ZB = zb_and(n734, n1144);
    let n1153: ZB = zb_not(n1152);
    let n1154: ZB = zb_and(n1151, n1152);
    let n1155: ZB = zb_and(n1151, n1153);
    let n1156: ZB = zb_or(n1154, n1155);
    let n1157: ZB = zb_and(n740, n1152);
    let n1158: ZB = zb_not(n1157);
    let n1159: ZB = zb_and(n1156, n1158);
    let n1160: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1143);
    let n1161: ZB = zb_not(n1160);
    let n1162: ZB = zb_and(n1159, n1160);
    let n1163: ZB = zb_and(n1159, n1161);
    let n1164: ZB = zb_or(n1162, n1163);
    let n1165: ZB = zb_and(n750, n1160);
    let n1166: ZB = zb_not(n1165);
    let n1167: ZB = zb_and(n1164, n1165);
    let n1168: ZB = zb_and(n1164, n1166);
    let n1169: ZB = zb_or(n1167, n1168);
    let n1170: ZB = zb_and(n1166, n1169);
    let n1171: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1143);
    let n1172: ZB = zb_not(n1171);
    let n1173: ZB = zb_and(n1170, n1171);
    let n1174: ZB = zb_and(n1170, n1172);
    let n1175: ZB = zb_or(n1173, n1174);
    let n1176: ZB = zb_and(n763, n1171);
    let n1177: ZB = zb_not(n1176);
    let n1178: ZB = zb_and(n1175, n1176);
    let n1179: ZB = zb_and(n1175, n1177);
    let n1180: ZB = zb_or(n1178, n1179);
    let n1181: ZB = zb_and(n769, n1176);
    let n1182: ZB = zb_not(n1181);
    let n1183: ZB = zb_and(n1180, n1182);
    let n1184: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1143);
    let n1185: ZB = zb_not(n1184);
    let n1186: ZB = zb_and(n1183, n1184);
    let n1187: ZB = zb_and(n1183, n1185);
    let n1188: ZB = zb_and(n780, n1186);
    let n1189: ZB = zb_and(n779, n1186);
    let n1190: ZN = zn_mul(n1135, zn_splat(P8::from_raw(524288i32)));
    let n1191: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1190);
    let n1192: ZB = zn_eq(n697, n1191);
    let n1193: ZB = zb_or(n1188, n1189);
    let n1194: ZB = zb_or(n779, n1192);
    let n1195: ZB = zb_or(n1187, n1193);
    let n1196: ZB = zb_and(n1184, n1194);
    let n1197: ZB = zb_not(n1196);
    let n1198: ZB = zb_and(n1195, n1196);
    let n1199: ZB = zb_and(n1195, n1197);
    let n1200: ZB = zb_or(n1198, n1199);
    let n1201: ZB = zb_and(n793, n1196);
    let n1202: ZB = zb_not(n1201);
    let n1203: ZB = zb_and(n1200, n1202);
    let n1204: ZB = zb_and(n799, n1203);
    let n1205: ZB = zb_and(n800, n1203);
    let n1206: ZN = zn_mget(g.cart, n1142, n803);
    let n1207: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1206);
    let n1208: ZB = zb_not(n1207);
    let n1209: ZB = zb_and(n1204, n1207);
    let n1210: ZB = zb_and(n1204, n1208);
    let n1211: ZB = zb_and(n727, n1209);
    let n1212: ZB = zb_and(n726, n1209);
    let n1213: ZB = zb_or(n1211, n1212);
    let n1214: ZB = zb_or(n1210, n1213);
    let n1215: ZB = zb_and(n815, n1207);
    let n1216: ZB = zb_not(n1215);
    let n1217: ZB = zb_and(n1214, n1215);
    let n1218: ZB = zb_and(n1214, n1216);
    let n1219: ZB = zb_or(n1217, n1218);
    let n1220: ZB = zb_and(n740, n1215);
    let n1221: ZB = zb_not(n1220);
    let n1222: ZB = zb_and(n1219, n1221);
    let n1223: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1206);
    let n1224: ZB = zb_not(n1223);
    let n1225: ZB = zb_and(n1222, n1223);
    let n1226: ZB = zb_and(n1222, n1224);
    let n1227: ZB = zb_or(n1225, n1226);
    let n1228: ZB = zb_and(n750, n1223);
    let n1229: ZB = zb_not(n1228);
    let n1230: ZB = zb_and(n1227, n1228);
    let n1231: ZB = zb_and(n1227, n1229);
    let n1232: ZB = zb_or(n1230, n1231);
    let n1233: ZB = zb_and(n1229, n1232);
    let n1234: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1206);
    let n1235: ZB = zb_not(n1234);
    let n1236: ZB = zb_and(n1233, n1234);
    let n1237: ZB = zb_and(n1233, n1235);
    let n1238: ZB = zb_or(n1236, n1237);
    let n1239: ZB = zb_and(n763, n1234);
    let n1240: ZB = zb_not(n1239);
    let n1241: ZB = zb_and(n1238, n1239);
    let n1242: ZB = zb_and(n1238, n1240);
    let n1243: ZB = zb_or(n1241, n1242);
    let n1244: ZB = zb_and(n769, n1239);
    let n1245: ZB = zb_not(n1244);
    let n1246: ZB = zb_and(n1243, n1245);
    let n1247: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1206);
    let n1248: ZB = zb_not(n1247);
    let n1249: ZB = zb_and(n1246, n1247);
    let n1250: ZB = zb_and(n1246, n1248);
    let n1251: ZB = zb_and(n780, n1249);
    let n1252: ZB = zb_and(n779, n1249);
    let n1253: ZB = zb_or(n1251, n1252);
    let n1254: ZB = zb_or(n1250, n1253);
    let n1255: ZB = zb_and(n1194, n1247);
    let n1256: ZB = zb_not(n1255);
    let n1257: ZB = zb_and(n1254, n1255);
    let n1258: ZB = zb_and(n1254, n1256);
    let n1259: ZB = zb_or(n1257, n1258);
    let n1260: ZB = zb_and(n793, n1255);
    let n1261: ZB = zb_not(n1260);
    let n1262: ZB = zb_and(n1259, n1261);
    let n1263: ZB = zb_and(n866, n1262);
    let n1264: ZB = zb_and(n867, n1262);
    let n1265: ZN = zn_mget(g.cart, n1142, n870);
    let n1266: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1265);
    let n1267: ZB = zb_not(n1266);
    let n1268: ZB = zb_and(n1263, n1266);
    let n1269: ZB = zb_and(n1263, n1267);
    let n1270: ZB = zb_and(n727, n1268);
    let n1271: ZB = zb_and(n726, n1268);
    let n1272: ZB = zb_or(n1270, n1271);
    let n1273: ZB = zb_or(n1269, n1272);
    let n1274: ZB = zb_and(n882, n1266);
    let n1275: ZB = zb_not(n1274);
    let n1276: ZB = zb_and(n1273, n1274);
    let n1277: ZB = zb_and(n1273, n1275);
    let n1278: ZB = zb_or(n1276, n1277);
    let n1279: ZB = zb_and(n740, n1274);
    let n1280: ZB = zb_not(n1279);
    let n1281: ZB = zb_and(n1278, n1280);
    let n1282: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1265);
    let n1283: ZB = zb_not(n1282);
    let n1284: ZB = zb_and(n1281, n1282);
    let n1285: ZB = zb_and(n1281, n1283);
    let n1286: ZB = zb_or(n1284, n1285);
    let n1287: ZB = zb_and(n750, n1282);
    let n1288: ZB = zb_not(n1287);
    let n1289: ZB = zb_and(n1286, n1287);
    let n1290: ZB = zb_and(n1286, n1288);
    let n1291: ZB = zb_or(n1289, n1290);
    let n1292: ZB = zb_and(n1288, n1291);
    let n1293: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1265);
    let n1294: ZB = zb_not(n1293);
    let n1295: ZB = zb_and(n1292, n1293);
    let n1296: ZB = zb_and(n1292, n1294);
    let n1297: ZB = zb_or(n1295, n1296);
    let n1298: ZB = zb_and(n763, n1293);
    let n1299: ZB = zb_not(n1298);
    let n1300: ZB = zb_and(n1297, n1298);
    let n1301: ZB = zb_and(n1297, n1299);
    let n1302: ZB = zb_or(n1300, n1301);
    let n1303: ZB = zb_and(n769, n1298);
    let n1304: ZB = zb_not(n1303);
    let n1305: ZB = zb_and(n1302, n1304);
    let n1306: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1265);
    let n1307: ZB = zb_not(n1306);
    let n1308: ZB = zb_and(n1305, n1306);
    let n1309: ZB = zb_and(n1305, n1307);
    let n1310: ZB = zb_and(n780, n1308);
    let n1311: ZB = zb_and(n779, n1308);
    let n1312: ZB = zb_or(n1310, n1311);
    let n1313: ZB = zb_or(n1309, n1312);
    let n1314: ZB = zb_and(n1194, n1306);
    let n1315: ZB = zb_not(n1314);
    let n1316: ZB = zb_and(n1313, n1314);
    let n1317: ZB = zb_and(n1313, n1315);
    let n1318: ZB = zb_or(n1316, n1317);
    let n1319: ZB = zb_and(n793, n1314);
    let n1320: ZB = zb_not(n1319);
    let n1321: ZB = zb_and(n1318, n1320);
    let n1322: ZB = zb_and(n933, n1134);
    let n1323: ZB = zb_or(n1264, n1321);
    let n1324: ZB = zsel_b(n867, n1134, n1322);
    let n1325: ZB = zb_or(n1205, n1323);
    let n1326: ZB = zsel_b(n800, n1134, n1324);
    let n1327: ZB = zb_or(n1141, n1325);
    let n1328: ZB = zsel_b(n715, n1134, n1326);
    let n1329: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n696);
    let n1330: ZB = zn_gt(n1329, n700);
    let n1331: ZB = zb_and(n1328, n1330);
    let n1332: ZB = zb_or(n1139, n1327);
    let n1333: ZB = zsel_b(n1137, n1134, n1331);
    let n1334: ZB = zb_or(n945, n1332);
    let n1335: ZB = zsel_b(n943, n940, n1333);
    let n1336: ZB = zb_or(n705, n1334);
    let n1337: ZB = zsel_b(n703, n692, n1335);
    let n1338: ZB = zn_le(n688, zn_splat(P8::from_raw(8388608i32)));
    let n1339: ZB = zb_and(n1336, n1338);
    let n1340: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n693);
    let n1341: ZB = zn_tile_flag_at(g.cache, g.cart, n480, n1340, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1342: ZB = zb_not(n1341);
    let n1343: ZB = zb_and(n1339, n1342);
    let n1344: ZB = zb_and(n1339, n1341);
    let n1345: ZB = zb_or(n1343, n1344);
    let n1346: ZB = zb_and(n1342, n1345);
    let n1347: ZB = zb_and(n1341, n1345);
    let n1348: ZB = zb_or(n1346, n1347);
    let n1349: ZB = zb_and(n1341, n1348);
    let n1350: ZB = zb_and(n1342, n1348);
    let n1351: ZB = zb_and(n242, n1349);
    let n1352: ZB = zb_and(n243, n1349);
    let n1353: ZB = zb_or(n1351, n1352);
    let n1354: ZB = zb_and(n245, n1350);
    let n1355: ZB = zb_and(n246, n1350);
    let n1356: ZB = zb_or(n1354, n1355);
    let n1357: ZN = zsel_n(n1341, n244, r_c256);
    let n1358: ZN = zsel_n(n1341, zn_splat(P8::from_raw(393216i32)), n248);
    let n1359: ZB = zb_or(n1353, n1356);
    let n1360: ZB = zb_and(n1342, n1359);
    let n1361: ZB = zb_and(n1341, n1359);
    let n1362: ZN = zsel_n(n1342, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1363: ZB = zb_or(n1360, n1361);
    let n1364: ZN = zn_abs(n464);
    let n1365: ZB = zn_gt(n1364, zn_splat(P8::from_raw(65536i32)));
    let n1366: ZB = zn_le(n1364, zn_splat(P8::from_raw(65536i32)));
    let n1367: ZB = zb_and(n1363, n1365);
    let n1368: ZB = zb_and(n1363, n1366);
    let n1369: ZB = zn_gt(n464, zn_splat(P8::from_raw(0i32)));
    let n1370: ZB = zb_and(n1367, n1369);
    let n1371: ZB = zb_and(n769, n1367);
    let n1372: ZB = zn_lt(n464, zn_splat(P8::from_raw(0i32)));
    let n1373: ZB = zb_and(n1371, n1372);
    let n1374: ZB = zb_and(n793, n1371);
    let n1375: ZB = zn_gt(n464, zn_splat(P8::from_raw(65536i32)));
    let n1376: ZB = zn_le(n464, zn_splat(P8::from_raw(65536i32)));
    let n1377: ZB = zb_and(n1370, n1375);
    let n1378: ZB = zb_and(n1370, n1376);
    let n1379: ZN = zn_sub(n464, zn_splat(P8::from_raw(9830i32)));
    let n1380: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1379);
    let n1381: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n464);
    let n1382: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1381);
    let n1383: ZB = zn_gt(n464, zn_splat(P8::from_raw(-65536i32)));
    let n1384: ZB = zn_le(n464, zn_splat(P8::from_raw(-65536i32)));
    let n1385: ZB = zb_and(n1373, n1383);
    let n1386: ZB = zb_and(n1373, n1384);
    let n1387: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1379);
    let n1388: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1381);
    let n1389: ZB = zb_and(n769, n1374);
    let n1390: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1379);
    let n1391: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1381);
    let n1392: ZN = zsel_n(n1383, n1387, n1388);
    let n1393: ZB = zb_or(n1385, n1386);
    let n1394: ZN = zsel_n(n1369, n1390, n1391);
    let n1395: ZN = zsel_n(n1375, n1380, n1382);
    let n1396: ZB = zb_or(n1377, n1378);
    let n1397: ZN = zsel_n(n1372, n1392, n1394);
    let n1398: ZB = zb_or(n1389, n1393);
    let n1399: ZN = zsel_n(n1369, n1395, n1397);
    let n1400: ZB = zb_or(n1396, n1398);
    let n1401: ZB = zb_and(n1368, n1369);
    let n1402: ZB = zb_and(n769, n1368);
    let n1403: ZN = zn_sub(n464, n1362);
    let n1404: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1403);
    let n1405: ZN = zn_add(n464, n1362);
    let n1406: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1405);
    let n1407: ZN = zsel_n(n1369, n1404, n1406);
    let n1408: ZB = zb_or(n1401, n1402);
    let n1409: ZN = zsel_n(n1365, n1399, n1407);
    let n1410: ZB = zb_or(n1400, n1408);
    let n1411: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1409);
    let n1412: ZB = zb_not(n1411);
    let n1413: ZB = zb_and(n1410, n1412);
    let n1414: ZB = zb_and(n1410, n1411);
    let n1415: ZB = zn_lt(n1409, zn_splat(P8::from_raw(0i32)));
    let n1416: ZB = zsel_b(n1412, n1415, r_c312);
    let n1417: ZB = zb_or(n1413, n1414);
    let n1418: ZN = zn_abs(n690);
    let n1419: ZB = zn_le(n1418, zn_splat(P8::from_raw(9830i32)));
    let n1420: ZB = zn_gt(n1418, zn_splat(P8::from_raw(9830i32)));
    let n1421: ZB = zb_and(n1417, n1419);
    let n1422: ZB = zb_and(n1417, n1420);
    let n1423: ZN = zsel_n(n1419, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n1424: ZB = zb_or(n1421, n1422);
    let n1425: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n693);
    let n1426: ZB = zb_and(n1342, n1424);
    let n1427: ZB = zb_and(n1341, n1424);
    let n1428: ZN = zn_add(n690, n1423);
    let n1429: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n1428);
    let n1430: ZN = zsel_n(n1342, n1429, n690);
    let n1431: ZB = zb_or(n1426, n1427);
    let n1432: ZB = zn_gt(n1358, zn_splat(P8::from_raw(0i32)));
    let n1433: ZB = zn_le(n1358, zn_splat(P8::from_raw(0i32)));
    let n1434: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n479);
    let n1435: ZB = zn_tile_flag_at(g.cache, g.cart, n1434, n1425, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1436: ZB = zb_not(n1435);
    let n1437: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n479);
    let n1438: ZB = zn_tile_flag_at(g.cache, g.cart, n1437, n1425, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1439: ZB = zb_not(n1438);
    let n1440: ZN = zsel_n(n1438, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1441: ZN = zsel_n(n1435, zn_splat(P8::from_raw(-65536i32)), n1440);
    let n1442: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1441);
    let n1443: ZB = zb_not(n1442);
    let n1444: ZN = zn_neg(n1441);
    let n1445: ZN = zn_mul(n1444, zn_splat(P8::from_raw(131072i32)));
    let n1446: ZN = zsel_n(n1443, n1445, n1409);
    let n1447: ZN = zsel_n(n1443, zn_splat(P8::from_raw(-131072i32)), n1430);
    let n1448: ZN = zsel_n(n1432, zn_splat(P8::from_raw(0i32)), n1358);
    let n1449: ZN = zsel_n(n1432, n1409, n1446);
    let n1450: ZN = zsel_n(n1432, zn_splat(P8::from_raw(-131072i32)), n1447);
    let n1451: ZB = zn_gt(n1357, zn_splat(P8::from_raw(0i32)));
    let n1452: ZB = zn_le(n1357, zn_splat(P8::from_raw(0i32)));
    let n1453: ZB = zb_and(n1431, n1451);
    let n1454: ZB = zb_and(n1431, n1452);
    let n1455: ZB = zb_or(n1453, n1454);
    let n1456: ZB = zb_not(n1416);
    let n1457: ZN = zsel_n(n1416, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1458: ZB = zn_gt(n1457, zn_splat(P8::from_raw(0i32)));
    let n1459: ZB = zn_le(n1457, zn_splat(P8::from_raw(0i32)));
    let n1460: ZB = zn_lt(n1457, zn_splat(P8::from_raw(0i32)));
    let n1461: ZB = zn_ge(n1457, zn_splat(P8::from_raw(0i32)));
    let n1462: ZN = zsel_n(n1460, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n1463: ZN = zsel_n(n1458, zn_splat(P8::from_raw(131072i32)), n1462);
    let n1464: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1457);
    let n1465: ZB = zb_not(n1464);
    let n1466: ZN = zsel_n(n1465, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n1467: ZB = zn_lt(n688, zn_splat(P8::from_raw(-262144i32)));
    let n1468: ZB = zn_ge(n688, zn_splat(P8::from_raw(-262144i32)));
    let n1469: ZB = zb_and(n1455, n1467);
    let n1470: ZB = zb_and(n1455, n1468);
    let n1471: ZB = zb_or(n1469, n1470);
    let n1472: ZB = zb_and(n1468, n1471);
    let n1473: ZB = zn_gt(n697, zn_splat(P8::from_raw(786432i32)));
    let n1474: ZB = zn_le(n697, zn_splat(P8::from_raw(786432i32)));
    let n1475: ZB = zb_and(n1472, n1473);
    let n1476: ZB = zb_and(n1472, n1474);
    let n1477: ZB = zn_gt(n709, zn_splat(P8::from_raw(2359296i32)));
    let n1478: ZB = zb_or(n1475, n1476);
    let n1479: ZB = zb_and(n1473, n1477);
    let n1480: ZB = zb_not(n1479);
    let n1481: ZB = zb_and(n1478, n1479);
    let n1482: ZB = zb_and(n1478, n1480);
    let n1483: ZB = zn_lt(n479, zn_splat(P8::from_raw(1310720i32)));
    let n1484: ZB = zb_or(n1481, n1482);
    let n1485: ZB = zb_and(n1479, n1483);
    let n1486: ZB = zb_not(n1485);
    let n1487: ZB = zb_and(n1484, n1485);
    let n1488: ZB = zb_and(n1484, n1486);
    let n1489: ZB = zn_lt(n693, zn_splat(P8::from_raw(2883584i32)));
    let n1490: ZB = zb_or(n1487, n1488);
    let n1491: ZB = zb_and(n1485, n1489);
    let n1492: ZB = zb_and(n1490, n1491);
    let n1493: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n1494: ZB = zb_and(n111, n1492);
    let n1495: ZB = zn_lt(n462, zn_splat(P8::from_raw(-65536i32)));
    let n1496: ZB = zn_ge(n462, zn_splat(P8::from_raw(-65536i32)));
    let n1497: ZB = zb_and(n1494, n1496);
    let n1498: ZB = zb_and(n1494, n1495);
    let n1499: ZB = zn_gt(n462, zn_splat(P8::from_raw(7929856i32)));
    let n1500: ZB = zb_or(n1497, n1498);
    let n1501: ZB = zb_or(n1495, n1499);
    let n1502: ZB = zb_not(n1501);
    let n1503: ZB = zb_and(n1500, n1501);
    let n1504: ZB = zb_and(n1500, n1502);
    let n1505: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n462);
    let n1506: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1505);
    let n1507: ZN = zsel_n(n1501, n1506, n462);
    let n1508: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n1409);
    let n1509: ZB = zb_or(n1503, n1504);
    let n1510: ZN = zsel_n(n1493, n462, n1507);
    let n1511: ZN = zsel_n(n1493, n1409, n1508);
    let n1524: ZB = zb_and(n1368, n1383);
    let n1525: ZB = zb_and(n1368, n1384);
    let n1526: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1403);
    let n1527: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1405);
    let n1528: ZN = zsel_n(n1383, n1526, n1527);
    let n1529: ZB = zb_or(n1524, n1525);
    let n1530: ZN = zsel_n(n1365, n1399, n1528);
    let n1531: ZB = zb_or(n1400, n1529);
    let n1532: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1530);
    let n1533: ZB = zb_not(n1532);
    let n1534: ZB = zb_and(n1531, n1533);
    let n1535: ZB = zb_and(n1531, n1532);
    let n1536: ZB = zn_lt(n1530, zn_splat(P8::from_raw(0i32)));
    let n1537: ZB = zsel_b(n1533, n1536, r_c312);
    let n1538: ZB = zb_or(n1534, n1535);
    let n1539: ZB = zb_and(n1419, n1538);
    let n1540: ZB = zb_and(n1420, n1538);
    let n1541: ZB = zb_or(n1539, n1540);
    let n1542: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n479);
    let n1543: ZB = zn_tile_flag_at(g.cache, g.cart, n1542, n1425, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1544: ZB = zb_not(n1543);
    let n1545: ZB = zb_and(n1541, n1544);
    let n1546: ZB = zb_and(n1541, n1543);
    let n1547: ZB = zb_or(n1545, n1546);
    let n1548: ZB = zb_and(n1544, n1547);
    let n1549: ZB = zb_and(n1543, n1547);
    let n1550: ZB = zb_or(n1548, n1549);
    let n1551: ZB = zb_and(n1543, n1550);
    let n1552: ZB = zb_and(n1544, n1550);
    let n1553: ZB = zb_or(n1551, n1552);
    let n1554: ZB = zb_and(n1543, n1553);
    let n1555: ZB = zb_and(n1544, n1553);
    let n1556: ZN = zsel_n(n1543, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1557: ZB = zb_or(n1554, n1555);
    let n1558: ZB = zb_and(n1342, n1557);
    let n1559: ZB = zb_and(n1341, n1557);
    let n1560: ZN = zn_min(n1428, n1556);
    let n1561: ZN = zsel_n(n1342, n1560, n690);
    let n1562: ZB = zb_or(n1558, n1559);
    let n1563: ZN = zsel_n(n1443, n1445, n1530);
    let n1564: ZN = zsel_n(n1443, zn_splat(P8::from_raw(-131072i32)), n1561);
    let n1565: ZN = zsel_n(n1432, n1530, n1563);
    let n1566: ZN = zsel_n(n1432, zn_splat(P8::from_raw(-131072i32)), n1564);
    let n1567: ZB = zb_and(n1451, n1562);
    let n1568: ZB = zb_and(n1452, n1562);
    let n1569: ZB = zb_or(n1567, n1568);
    let n1570: ZB = zb_and(n1467, n1569);
    let n1571: ZB = zb_and(n1468, n1569);
    let n1572: ZB = zb_or(n1570, n1571);
    let n1573: ZB = zb_and(n1468, n1572);
    let n1574: ZB = zb_and(n1473, n1573);
    let n1575: ZB = zb_and(n1474, n1573);
    let n1576: ZB = zb_or(n1574, n1575);
    let n1577: ZB = zb_and(n1479, n1576);
    let n1578: ZB = zb_and(n1480, n1576);
    let n1579: ZB = zb_or(n1577, n1578);
    let n1580: ZB = zb_and(n1485, n1579);
    let n1581: ZB = zb_and(n1486, n1579);
    let n1582: ZB = zb_or(n1580, n1581);
    let n1583: ZB = zb_and(n1491, n1582);
    let n1584: ZB = zb_and(n111, n1583);
    let n1585: ZB = zb_and(n1496, n1584);
    let n1586: ZB = zb_and(n1495, n1584);
    let n1587: ZB = zb_or(n1585, n1586);
    let n1588: ZB = zb_and(n1501, n1587);
    let n1589: ZB = zb_and(n1502, n1587);
    let n1590: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n1530);
    let n1591: ZB = zb_or(n1588, n1589);
    let n1592: ZN = zsel_n(n1493, n1530, n1590);
    let n1593: ZB = zb_and(n1368, n1375);
    let n1594: ZB = zb_and(n1368, n1376);
    let n1595: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1403);
    let n1596: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1405);
    let n1597: ZN = zsel_n(n1375, n1595, n1596);
    let n1598: ZB = zb_or(n1593, n1594);
    let n1599: ZN = zsel_n(n1365, n1399, n1597);
    let n1600: ZB = zb_or(n1400, n1598);
    let n1601: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1599);
    let n1602: ZB = zb_not(n1601);
    let n1603: ZB = zb_and(n1600, n1602);
    let n1604: ZB = zb_and(n1600, n1601);
    let n1605: ZB = zn_lt(n1599, zn_splat(P8::from_raw(0i32)));
    let n1606: ZB = zsel_b(n1602, n1605, r_c312);
    let n1607: ZB = zb_or(n1603, n1604);
    let n1608: ZB = zb_and(n1419, n1607);
    let n1609: ZB = zb_and(n1420, n1607);
    let n1610: ZB = zb_or(n1608, n1609);
    let n1611: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n479);
    let n1612: ZB = zn_tile_flag_at(g.cache, g.cart, n1611, n1425, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1613: ZB = zb_not(n1612);
    let n1614: ZB = zb_and(n1610, n1613);
    let n1615: ZB = zb_and(n1610, n1612);
    let n1616: ZB = zb_or(n1614, n1615);
    let n1617: ZB = zb_and(n1613, n1616);
    let n1618: ZB = zb_and(n1612, n1616);
    let n1619: ZB = zb_or(n1617, n1618);
    let n1620: ZB = zb_and(n1612, n1619);
    let n1621: ZB = zb_and(n1613, n1619);
    let n1622: ZB = zb_or(n1620, n1621);
    let n1623: ZB = zb_and(n1612, n1622);
    let n1624: ZB = zb_and(n1613, n1622);
    let n1625: ZN = zsel_n(n1612, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1626: ZB = zb_or(n1623, n1624);
    let n1627: ZB = zb_and(n1342, n1626);
    let n1628: ZB = zb_and(n1341, n1626);
    let n1629: ZN = zn_min(n1428, n1625);
    let n1630: ZN = zsel_n(n1342, n1629, n690);
    let n1631: ZB = zb_or(n1627, n1628);
    let n1632: ZN = zsel_n(n1443, n1445, n1599);
    let n1633: ZN = zsel_n(n1443, zn_splat(P8::from_raw(-131072i32)), n1630);
    let n1634: ZN = zsel_n(n1432, n1599, n1632);
    let n1635: ZN = zsel_n(n1432, zn_splat(P8::from_raw(-131072i32)), n1633);
    let n1636: ZB = zb_and(n1451, n1631);
    let n1637: ZB = zb_and(n1452, n1631);
    let n1638: ZB = zb_or(n1636, n1637);
    let n1639: ZB = zb_and(n1467, n1638);
    let n1640: ZB = zb_and(n1468, n1638);
    let n1641: ZB = zb_or(n1639, n1640);
    let n1642: ZB = zb_and(n1468, n1641);
    let n1643: ZB = zb_and(n1473, n1642);
    let n1644: ZB = zb_and(n1474, n1642);
    let n1645: ZB = zb_or(n1643, n1644);
    let n1646: ZB = zb_and(n1479, n1645);
    let n1647: ZB = zb_and(n1480, n1645);
    let n1648: ZB = zb_or(n1646, n1647);
    let n1649: ZB = zb_and(n1485, n1648);
    let n1650: ZB = zb_and(n1486, n1648);
    let n1651: ZB = zb_or(n1649, n1650);
    let n1652: ZB = zb_and(n1491, n1651);
    let n1653: ZB = zb_and(n111, n1652);
    let n1654: ZB = zb_and(n1496, n1653);
    let n1655: ZB = zb_and(n1495, n1653);
    let n1656: ZB = zb_or(n1654, n1655);
    let n1657: ZB = zb_and(n1501, n1656);
    let n1658: ZB = zb_and(n1502, n1656);
    let n1659: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n1599);
    let n1660: ZB = zb_or(n1657, n1658);
    let n1661: ZN = zsel_n(n1493, n1599, n1659);
    let n1662: ZB = zb_and(n240, n1431);
    let n1663: ZB = zb_and(r_c266, n1431);
    let n1664: ZB = zb_and(n1432, n1662);
    let n1665: ZB = zb_and(n1433, n1662);
    let n1666: ZB = zb_and(n1436, n1665);
    let n1667: ZB = zb_and(n1435, n1665);
    let n1668: ZB = zb_or(n1666, n1667);
    let n1669: ZB = zb_and(n1436, n1668);
    let n1670: ZB = zb_and(n1435, n1668);
    let n1671: ZB = zb_or(n1669, n1670);
    let n1672: ZB = zb_and(n1435, n1671);
    let n1673: ZB = zb_and(n1436, n1671);
    let n1674: ZB = zb_and(n1439, n1673);
    let n1675: ZB = zb_and(n1438, n1673);
    let n1676: ZB = zb_or(n1674, n1675);
    let n1677: ZB = zb_and(n1439, n1676);
    let n1678: ZB = zb_and(n1438, n1676);
    let n1679: ZB = zb_or(n1677, n1678);
    let n1680: ZB = zb_and(n1438, n1679);
    let n1681: ZB = zb_and(n1439, n1679);
    let n1682: ZB = zb_or(n1680, n1681);
    let n1683: ZB = zb_or(n1672, n1682);
    let n1684: ZB = zb_and(n1443, n1683);
    let n1685: ZB = zb_and(n1442, n1683);
    let n1686: ZB = zb_or(n1684, n1685);
    let n1687: ZB = zb_or(n1664, n1686);
    let n1688: ZN = zsel_n(n240, n1448, n1358);
    let n1689: ZN = zsel_n(n240, n1449, n1409);
    let n1690: ZN = zsel_n(n240, n1450, n1430);
    let n1691: ZB = zb_or(n1663, n1687);
    let n1692: ZB = zb_and(n1451, n1691);
    let n1693: ZB = zb_and(n1452, n1691);
    let n1694: ZB = zb_or(n1692, n1693);
    let n1695: ZB = zb_and(n1467, n1694);
    let n1696: ZB = zb_and(n1468, n1694);
    let n1697: ZB = zb_or(n1695, n1696);
    let n1698: ZB = zb_and(n1468, n1697);
    let n1699: ZB = zb_and(n1473, n1698);
    let n1700: ZB = zb_and(n1474, n1698);
    let n1701: ZB = zb_or(n1699, n1700);
    let n1702: ZB = zb_and(n1479, n1701);
    let n1703: ZB = zb_and(n1480, n1701);
    let n1704: ZB = zb_or(n1702, n1703);
    let n1705: ZB = zb_and(n1485, n1704);
    let n1706: ZB = zb_and(n1486, n1704);
    let n1707: ZB = zb_or(n1705, n1706);
    let n1708: ZB = zb_and(n1491, n1707);
    let n1709: ZB = zb_and(n111, n1708);
    let n1710: ZB = zb_and(n1496, n1709);
    let n1711: ZB = zb_and(n1495, n1709);
    let n1712: ZB = zb_or(n1710, n1711);
    let n1713: ZB = zb_and(n1501, n1712);
    let n1714: ZB = zb_and(n1502, n1712);
    let n1715: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n1689);
    let n1716: ZB = zb_or(n1713, n1714);
    let n1717: ZN = zsel_n(n1493, n1689, n1715);
    let n1718: ZB = zb_and(n240, n1562);
    let n1719: ZB = zb_and(r_c266, n1562);
    let n1720: ZB = zb_and(n1432, n1718);
    let n1721: ZB = zb_and(n1433, n1718);
    let n1722: ZB = zb_and(n1436, n1721);
    let n1723: ZB = zb_and(n1435, n1721);
    let n1724: ZB = zb_or(n1722, n1723);
    let n1725: ZB = zb_and(n1436, n1724);
    let n1726: ZB = zb_and(n1435, n1724);
    let n1727: ZB = zb_or(n1725, n1726);
    let n1728: ZB = zb_and(n1435, n1727);
    let n1729: ZB = zb_and(n1436, n1727);
    let n1730: ZB = zb_and(n1439, n1729);
    let n1731: ZB = zb_and(n1438, n1729);
    let n1732: ZB = zb_or(n1730, n1731);
    let n1733: ZB = zb_and(n1439, n1732);
    let n1734: ZB = zb_and(n1438, n1732);
    let n1735: ZB = zb_or(n1733, n1734);
    let n1736: ZB = zb_and(n1438, n1735);
    let n1737: ZB = zb_and(n1439, n1735);
    let n1738: ZB = zb_or(n1736, n1737);
    let n1739: ZB = zb_or(n1728, n1738);
    let n1740: ZB = zb_and(n1443, n1739);
    let n1741: ZB = zb_and(n1442, n1739);
    let n1742: ZB = zb_or(n1740, n1741);
    let n1743: ZB = zb_or(n1720, n1742);
    let n1744: ZN = zsel_n(n240, n1565, n1530);
    let n1745: ZN = zsel_n(n240, n1566, n1561);
    let n1746: ZB = zb_or(n1719, n1743);
    let n1747: ZB = zb_and(n1451, n1746);
    let n1748: ZB = zb_and(n1452, n1746);
    let n1749: ZB = zb_or(n1747, n1748);
    let n1750: ZB = zb_and(n1467, n1749);
    let n1751: ZB = zb_and(n1468, n1749);
    let n1752: ZB = zb_or(n1750, n1751);
    let n1753: ZB = zb_and(n1468, n1752);
    let n1754: ZB = zb_and(n1473, n1753);
    let n1755: ZB = zb_and(n1474, n1753);
    let n1756: ZB = zb_or(n1754, n1755);
    let n1757: ZB = zb_and(n1479, n1756);
    let n1758: ZB = zb_and(n1480, n1756);
    let n1759: ZB = zb_or(n1757, n1758);
    let n1760: ZB = zb_and(n1485, n1759);
    let n1761: ZB = zb_and(n1486, n1759);
    let n1762: ZB = zb_or(n1760, n1761);
    let n1763: ZB = zb_and(n1491, n1762);
    let n1764: ZB = zb_and(n111, n1763);
    let n1765: ZB = zb_and(n1496, n1764);
    let n1766: ZB = zb_and(n1495, n1764);
    let n1767: ZB = zb_or(n1765, n1766);
    let n1768: ZB = zb_and(n1501, n1767);
    let n1769: ZB = zb_and(n1502, n1767);
    let n1770: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n1744);
    let n1771: ZB = zb_or(n1768, n1769);
    let n1772: ZN = zsel_n(n1493, n1744, n1770);
    let n1773: ZB = zb_and(n240, n1631);
    let n1774: ZB = zb_and(r_c266, n1631);
    let n1775: ZB = zb_and(n1432, n1773);
    let n1776: ZB = zb_and(n1433, n1773);
    let n1777: ZB = zb_and(n1436, n1776);
    let n1778: ZB = zb_and(n1435, n1776);
    let n1779: ZB = zb_or(n1777, n1778);
    let n1780: ZB = zb_and(n1436, n1779);
    let n1781: ZB = zb_and(n1435, n1779);
    let n1782: ZB = zb_or(n1780, n1781);
    let n1783: ZB = zb_and(n1435, n1782);
    let n1784: ZB = zb_and(n1436, n1782);
    let n1785: ZB = zb_and(n1439, n1784);
    let n1786: ZB = zb_and(n1438, n1784);
    let n1787: ZB = zb_or(n1785, n1786);
    let n1788: ZB = zb_and(n1439, n1787);
    let n1789: ZB = zb_and(n1438, n1787);
    let n1790: ZB = zb_or(n1788, n1789);
    let n1791: ZB = zb_and(n1438, n1790);
    let n1792: ZB = zb_and(n1439, n1790);
    let n1793: ZB = zb_or(n1791, n1792);
    let n1794: ZB = zb_or(n1783, n1793);
    let n1795: ZB = zb_and(n1443, n1794);
    let n1796: ZB = zb_and(n1442, n1794);
    let n1797: ZB = zb_or(n1795, n1796);
    let n1798: ZB = zb_or(n1775, n1797);
    let n1799: ZN = zsel_n(n240, n1634, n1599);
    let n1800: ZN = zsel_n(n240, n1635, n1630);
    let n1801: ZB = zb_or(n1774, n1798);
    let n1802: ZB = zb_and(n1451, n1801);
    let n1803: ZB = zb_and(n1452, n1801);
    let n1804: ZB = zb_or(n1802, n1803);
    let n1805: ZB = zb_and(n1467, n1804);
    let n1806: ZB = zb_and(n1468, n1804);
    let n1807: ZB = zb_or(n1805, n1806);
    let n1808: ZB = zb_and(n1468, n1807);
    let n1809: ZB = zb_and(n1473, n1808);
    let n1810: ZB = zb_and(n1474, n1808);
    let n1811: ZB = zb_or(n1809, n1810);
    let n1812: ZB = zb_and(n1479, n1811);
    let n1813: ZB = zb_and(n1480, n1811);
    let n1814: ZB = zb_or(n1812, n1813);
    let n1815: ZB = zb_and(n1485, n1814);
    let n1816: ZB = zb_and(n1486, n1814);
    let n1817: ZB = zb_or(n1815, n1816);
    let n1818: ZB = zb_and(n1491, n1817);
    let n1819: ZB = zb_and(n111, n1818);
    let n1820: ZB = zb_and(n1496, n1819);
    let n1821: ZB = zb_and(n1495, n1819);
    let n1822: ZB = zb_or(n1820, n1821);
    let n1823: ZB = zb_and(n1501, n1822);
    let n1824: ZB = zb_and(n1502, n1822);
    let n1825: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n1799);
    let n1826: ZB = zb_or(n1823, n1824);
    let n1827: ZN = zsel_n(n1493, n1799, n1825);
    let n1828: ZB = zb_and(n241, n1451);
    let n1829: ZB = zb_not(n1828);
    let n1830: ZB = zb_and(n1455, n1828);
    let n1831: ZB = zb_and(n1455, n1829);
    let n1832: ZB = zb_and(n1416, n1830);
    let n1833: ZB = zb_and(n1456, n1830);
    let n1834: ZB = zb_or(n1832, n1833);
    let n1835: ZB = zb_and(n1458, n1834);
    let n1836: ZB = zb_and(n1459, n1834);
    let n1837: ZB = zb_and(n1460, n1836);
    let n1838: ZB = zb_and(n1461, n1836);
    let n1839: ZB = zb_or(n1837, n1838);
    let n1840: ZB = zb_or(n1835, n1839);
    let n1841: ZB = zb_and(n1465, n1840);
    let n1842: ZB = zb_and(n1464, n1840);
    let n1843: ZB = zb_or(n1841, n1842);
    let n1844: ZN = zsel_n(n1828, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1845: ZB = zb_or(r_c41, n1828);
    let n1846: ZN = zsel_n(n1828, zn_splat(P8::from_raw(655360i32)), n249);
    let n1847: ZN = zsel_n(n1828, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(-65536i32)));
    let n1848: ZN = zsel_n(n1828, zn_splat(P8::from_raw(98304i32)), r_c308);
    let n1849: ZN = zsel_n(n1828, n1466, r_c309);
    let n1850: ZN = zsel_n(n1828, n1463, r_c310);
    let n1851: ZN = zsel_n(n1828, zn_splat(P8::from_raw(0i32)), r_c311);
    let n1852: ZN = zsel_n(n1828, n1457, n1409);
    let n1853: ZN = zsel_n(n1828, zn_splat(P8::from_raw(0i32)), n1430);
    let n1854: ZB = zb_or(n1831, n1843);
    let n1855: ZB = zb_and(n1467, n1854);
    let n1856: ZB = zb_and(n1468, n1854);
    let n1857: ZB = zb_or(n1855, n1856);
    let n1858: ZB = zb_and(n1468, n1857);
    let n1859: ZB = zb_and(n1473, n1858);
    let n1860: ZB = zb_and(n1474, n1858);
    let n1861: ZB = zb_or(n1859, n1860);
    let n1862: ZB = zb_and(n1479, n1861);
    let n1863: ZB = zb_and(n1480, n1861);
    let n1864: ZB = zb_or(n1862, n1863);
    let n1865: ZB = zb_and(n1485, n1864);
    let n1866: ZB = zb_and(n1486, n1864);
    let n1867: ZB = zb_or(n1865, n1866);
    let n1868: ZB = zb_and(n1491, n1867);
    let n1869: ZB = zn_gt(n1844, zn_splat(P8::from_raw(0i32)));
    let n1870: ZB = zn_le(n1844, zn_splat(P8::from_raw(0i32)));
    let n1871: ZB = zb_and(n1868, n1869);
    let n1872: ZB = zb_and(n1868, n1870);
    let n1873: ZB = zb_and(n1496, n1872);
    let n1874: ZB = zb_and(n1495, n1872);
    let n1875: ZB = zb_or(n1873, n1874);
    let n1876: ZB = zb_and(n1501, n1875);
    let n1877: ZB = zb_and(n1502, n1875);
    let n1878: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n1852);
    let n1879: ZB = zb_or(n1876, n1877);
    let n1880: ZN = zsel_n(n1869, n462, n1507);
    let n1881: ZN = zsel_n(n1869, n1852, n1878);
    let n1882: ZB = zb_or(n1871, n1879);
    let n1883: ZB = zb_and(n1569, n1828);
    let n1884: ZB = zb_and(n1569, n1829);
    let n1885: ZN = zsel_n(n1828, zn_splat(P8::from_raw(69510i32)), r_c309);
    let n1886: ZN = zsel_n(n1828, zn_splat(P8::from_raw(-131072i32)), r_c310);
    let n1887: ZN = zsel_n(n1828, zn_splat(P8::from_raw(-327680i32)), n1530);
    let n1888: ZN = zsel_n(n1828, zn_splat(P8::from_raw(0i32)), n1561);
    let n1889: ZB = zb_or(n1883, n1884);
    let n1890: ZB = zb_and(n1467, n1889);
    let n1891: ZB = zb_and(n1468, n1889);
    let n1892: ZB = zb_or(n1890, n1891);
    let n1893: ZB = zb_and(n1468, n1892);
    let n1894: ZB = zb_and(n1473, n1893);
    let n1895: ZB = zb_and(n1474, n1893);
    let n1896: ZB = zb_or(n1894, n1895);
    let n1897: ZB = zb_and(n1479, n1896);
    let n1898: ZB = zb_and(n1480, n1896);
    let n1899: ZB = zb_or(n1897, n1898);
    let n1900: ZB = zb_and(n1485, n1899);
    let n1901: ZB = zb_and(n1486, n1899);
    let n1902: ZB = zb_or(n1900, n1901);
    let n1903: ZB = zb_and(n1491, n1902);
    let n1904: ZB = zb_and(n1869, n1903);
    let n1905: ZB = zb_and(n1870, n1903);
    let n1906: ZB = zb_and(n1496, n1905);
    let n1907: ZB = zb_and(n1495, n1905);
    let n1908: ZB = zb_or(n1906, n1907);
    let n1909: ZB = zb_and(n1501, n1908);
    let n1910: ZB = zb_and(n1502, n1908);
    let n1911: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n1887);
    let n1912: ZB = zb_or(n1909, n1910);
    let n1913: ZN = zsel_n(n1869, n1887, n1911);
    let n1914: ZB = zb_or(n1904, n1912);
    let n1915: ZB = zb_and(n1638, n1828);
    let n1916: ZB = zb_and(n1638, n1829);
    let n1917: ZN = zsel_n(n1828, zn_splat(P8::from_raw(131072i32)), r_c310);
    let n1918: ZN = zsel_n(n1828, zn_splat(P8::from_raw(327680i32)), n1599);
    let n1919: ZN = zsel_n(n1828, zn_splat(P8::from_raw(0i32)), n1630);
    let n1920: ZB = zb_or(n1915, n1916);
    let n1921: ZB = zb_and(n1467, n1920);
    let n1922: ZB = zb_and(n1468, n1920);
    let n1923: ZB = zb_or(n1921, n1922);
    let n1924: ZB = zb_and(n1468, n1923);
    let n1925: ZB = zb_and(n1473, n1924);
    let n1926: ZB = zb_and(n1474, n1924);
    let n1927: ZB = zb_or(n1925, n1926);
    let n1928: ZB = zb_and(n1479, n1927);
    let n1929: ZB = zb_and(n1480, n1927);
    let n1930: ZB = zb_or(n1928, n1929);
    let n1931: ZB = zb_and(n1485, n1930);
    let n1932: ZB = zb_and(n1486, n1930);
    let n1933: ZB = zb_or(n1931, n1932);
    let n1934: ZB = zb_and(n1491, n1933);
    let n1935: ZB = zb_and(n1869, n1934);
    let n1936: ZB = zb_and(n1870, n1934);
    let n1937: ZB = zb_and(n1496, n1936);
    let n1938: ZB = zb_and(n1495, n1936);
    let n1939: ZB = zb_or(n1937, n1938);
    let n1940: ZB = zb_and(n1501, n1939);
    let n1941: ZB = zb_and(n1502, n1939);
    let n1942: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n1918);
    let n1943: ZB = zb_or(n1940, n1941);
    let n1944: ZN = zsel_n(n1869, n1918, n1942);
    let n1945: ZB = zb_or(n1935, n1943);
    let n1946: ZN = zsel_n(n1828, zn_splat(P8::from_raw(69510i32)), r_c308);
    let n1947: ZN = zsel_n(n1828, zn_splat(P8::from_raw(98304i32)), r_c309);
    let n1948: ZN = zsel_n(n1828, zn_splat(P8::from_raw(0i32)), r_c310);
    let n1949: ZN = zsel_n(n1828, zn_splat(P8::from_raw(-98304i32)), r_c311);
    let n1950: ZN = zsel_n(n1828, zn_splat(P8::from_raw(0i32)), n1409);
    let n1951: ZN = zsel_n(n1828, zn_splat(P8::from_raw(-327680i32)), n1430);
    let n1952: ZB = zb_or(n1830, n1831);
    let n1953: ZB = zb_and(n1467, n1952);
    let n1954: ZB = zb_and(n1468, n1952);
    let n1955: ZB = zb_or(n1953, n1954);
    let n1956: ZB = zb_and(n1468, n1955);
    let n1957: ZB = zb_and(n1473, n1956);
    let n1958: ZB = zb_and(n1474, n1956);
    let n1959: ZB = zb_or(n1957, n1958);
    let n1960: ZB = zb_and(n1479, n1959);
    let n1961: ZB = zb_and(n1480, n1959);
    let n1962: ZB = zb_or(n1960, n1961);
    let n1963: ZB = zb_and(n1485, n1962);
    let n1964: ZB = zb_and(n1486, n1962);
    let n1965: ZB = zb_or(n1963, n1964);
    let n1966: ZB = zb_and(n1491, n1965);
    let n1967: ZB = zb_and(n1869, n1966);
    let n1968: ZB = zb_and(n1870, n1966);
    let n1969: ZB = zb_and(n1496, n1968);
    let n1970: ZB = zb_and(n1495, n1968);
    let n1971: ZB = zb_or(n1969, n1970);
    let n1972: ZB = zb_and(n1501, n1971);
    let n1973: ZB = zb_and(n1502, n1971);
    let n1974: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n1950);
    let n1975: ZB = zb_or(n1972, n1973);
    let n1976: ZN = zsel_n(n1869, n1950, n1974);
    let n1977: ZB = zb_or(n1967, n1975);
    let n1978: ZN = zsel_n(n1828, zn_splat(P8::from_raw(-231700i32)), n1530);
    let n1979: ZN = zsel_n(n1828, zn_splat(P8::from_raw(-231700i32)), n1561);
    let n1980: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n1978);
    let n1981: ZN = zsel_n(n1869, n1978, n1980);
    let n1982: ZN = zsel_n(n1828, zn_splat(P8::from_raw(231700i32)), n1599);
    let n1983: ZN = zsel_n(n1828, zn_splat(P8::from_raw(-231700i32)), n1630);
    let n1984: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n1982);
    let n1985: ZN = zsel_n(n1869, n1982, n1984);
    let n1986: ZN = zsel_n(n1828, zn_splat(P8::from_raw(131072i32)), r_c311);
    let n1987: ZN = zsel_n(n1828, zn_splat(P8::from_raw(327680i32)), n1430);
    let n1988: ZN = zsel_n(n1828, zn_splat(P8::from_raw(231700i32)), n1561);
    let n1989: ZN = zsel_n(n1828, zn_splat(P8::from_raw(231700i32)), n1630);
    let n1990: ZB = zb_and(n1694, n1828);
    let n1991: ZB = zb_and(n1694, n1829);
    let n1992: ZB = zb_and(n1416, n1990);
    let n1993: ZB = zb_and(n1456, n1990);
    let n1994: ZB = zb_or(n1992, n1993);
    let n1995: ZB = zb_and(n1458, n1994);
    let n1996: ZB = zb_and(n1459, n1994);
    let n1997: ZB = zb_and(n1460, n1996);
    let n1998: ZB = zb_and(n1461, n1996);
    let n1999: ZB = zb_or(n1997, n1998);
    let n2000: ZB = zb_or(n1995, n1999);
    let n2001: ZB = zb_and(n1465, n2000);
    let n2002: ZB = zb_and(n1464, n2000);
    let n2003: ZB = zb_or(n2001, n2002);
    let n2004: ZN = zsel_n(n1828, n1457, n1689);
    let n2005: ZN = zsel_n(n1828, zn_splat(P8::from_raw(0i32)), n1690);
    let n2006: ZB = zb_or(n1991, n2003);
    let n2007: ZB = zb_and(n1467, n2006);
    let n2008: ZB = zb_and(n1468, n2006);
    let n2009: ZB = zb_or(n2007, n2008);
    let n2010: ZB = zb_and(n1468, n2009);
    let n2011: ZB = zb_and(n1473, n2010);
    let n2012: ZB = zb_and(n1474, n2010);
    let n2013: ZB = zb_or(n2011, n2012);
    let n2014: ZB = zb_and(n1479, n2013);
    let n2015: ZB = zb_and(n1480, n2013);
    let n2016: ZB = zb_or(n2014, n2015);
    let n2017: ZB = zb_and(n1485, n2016);
    let n2018: ZB = zb_and(n1486, n2016);
    let n2019: ZB = zb_or(n2017, n2018);
    let n2020: ZB = zb_and(n1491, n2019);
    let n2021: ZB = zb_and(n1869, n2020);
    let n2022: ZB = zb_and(n1870, n2020);
    let n2023: ZB = zb_and(n1496, n2022);
    let n2024: ZB = zb_and(n1495, n2022);
    let n2025: ZB = zb_or(n2023, n2024);
    let n2026: ZB = zb_and(n1501, n2025);
    let n2027: ZB = zb_and(n1502, n2025);
    let n2028: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n2004);
    let n2029: ZB = zb_or(n2026, n2027);
    let n2030: ZN = zsel_n(n1869, n2004, n2028);
    let n2031: ZB = zb_or(n2021, n2029);
    let n2032: ZB = zb_and(n1749, n1828);
    let n2033: ZB = zb_and(n1749, n1829);
    let n2034: ZN = zsel_n(n1828, zn_splat(P8::from_raw(-327680i32)), n1744);
    let n2035: ZN = zsel_n(n1828, zn_splat(P8::from_raw(0i32)), n1745);
    let n2036: ZB = zb_or(n2032, n2033);
    let n2037: ZB = zb_and(n1467, n2036);
    let n2038: ZB = zb_and(n1468, n2036);
    let n2039: ZB = zb_or(n2037, n2038);
    let n2040: ZB = zb_and(n1468, n2039);
    let n2041: ZB = zb_and(n1473, n2040);
    let n2042: ZB = zb_and(n1474, n2040);
    let n2043: ZB = zb_or(n2041, n2042);
    let n2044: ZB = zb_and(n1479, n2043);
    let n2045: ZB = zb_and(n1480, n2043);
    let n2046: ZB = zb_or(n2044, n2045);
    let n2047: ZB = zb_and(n1485, n2046);
    let n2048: ZB = zb_and(n1486, n2046);
    let n2049: ZB = zb_or(n2047, n2048);
    let n2050: ZB = zb_and(n1491, n2049);
    let n2051: ZB = zb_and(n1869, n2050);
    let n2052: ZB = zb_and(n1870, n2050);
    let n2053: ZB = zb_and(n1496, n2052);
    let n2054: ZB = zb_and(n1495, n2052);
    let n2055: ZB = zb_or(n2053, n2054);
    let n2056: ZB = zb_and(n1501, n2055);
    let n2057: ZB = zb_and(n1502, n2055);
    let n2058: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n2034);
    let n2059: ZB = zb_or(n2056, n2057);
    let n2060: ZN = zsel_n(n1869, n2034, n2058);
    let n2061: ZB = zb_or(n2051, n2059);
    let n2062: ZB = zb_and(n1804, n1828);
    let n2063: ZB = zb_and(n1804, n1829);
    let n2064: ZN = zsel_n(n1828, zn_splat(P8::from_raw(327680i32)), n1799);
    let n2065: ZN = zsel_n(n1828, zn_splat(P8::from_raw(0i32)), n1800);
    let n2066: ZB = zb_or(n2062, n2063);
    let n2067: ZB = zb_and(n1467, n2066);
    let n2068: ZB = zb_and(n1468, n2066);
    let n2069: ZB = zb_or(n2067, n2068);
    let n2070: ZB = zb_and(n1468, n2069);
    let n2071: ZB = zb_and(n1473, n2070);
    let n2072: ZB = zb_and(n1474, n2070);
    let n2073: ZB = zb_or(n2071, n2072);
    let n2074: ZB = zb_and(n1479, n2073);
    let n2075: ZB = zb_and(n1480, n2073);
    let n2076: ZB = zb_or(n2074, n2075);
    let n2077: ZB = zb_and(n1485, n2076);
    let n2078: ZB = zb_and(n1486, n2076);
    let n2079: ZB = zb_or(n2077, n2078);
    let n2080: ZB = zb_and(n1491, n2079);
    let n2081: ZB = zb_and(n1869, n2080);
    let n2082: ZB = zb_and(n1870, n2080);
    let n2083: ZB = zb_and(n1496, n2082);
    let n2084: ZB = zb_and(n1495, n2082);
    let n2085: ZB = zb_or(n2083, n2084);
    let n2086: ZB = zb_and(n1501, n2085);
    let n2087: ZB = zb_and(n1502, n2085);
    let n2088: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n2064);
    let n2089: ZB = zb_or(n2086, n2087);
    let n2090: ZN = zsel_n(n1869, n2064, n2088);
    let n2091: ZB = zb_or(n2081, n2089);
    let n2092: ZN = zsel_n(n1828, zn_splat(P8::from_raw(0i32)), n1689);
    let n2093: ZN = zsel_n(n1828, zn_splat(P8::from_raw(-327680i32)), n1690);
    let n2094: ZB = zb_or(n1990, n1991);
    let n2095: ZB = zb_and(n1467, n2094);
    let n2096: ZB = zb_and(n1468, n2094);
    let n2097: ZB = zb_or(n2095, n2096);
    let n2098: ZB = zb_and(n1468, n2097);
    let n2099: ZB = zb_and(n1473, n2098);
    let n2100: ZB = zb_and(n1474, n2098);
    let n2101: ZB = zb_or(n2099, n2100);
    let n2102: ZB = zb_and(n1479, n2101);
    let n2103: ZB = zb_and(n1480, n2101);
    let n2104: ZB = zb_or(n2102, n2103);
    let n2105: ZB = zb_and(n1485, n2104);
    let n2106: ZB = zb_and(n1486, n2104);
    let n2107: ZB = zb_or(n2105, n2106);
    let n2108: ZB = zb_and(n1491, n2107);
    let n2109: ZB = zb_and(n1869, n2108);
    let n2110: ZB = zb_and(n1870, n2108);
    let n2111: ZB = zb_and(n1496, n2110);
    let n2112: ZB = zb_and(n1495, n2110);
    let n2113: ZB = zb_or(n2111, n2112);
    let n2114: ZB = zb_and(n1501, n2113);
    let n2115: ZB = zb_and(n1502, n2113);
    let n2116: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n2092);
    let n2117: ZB = zb_or(n2114, n2115);
    let n2118: ZN = zsel_n(n1869, n2092, n2116);
    let n2119: ZB = zb_or(n2109, n2117);
    let n2120: ZN = zsel_n(n1828, zn_splat(P8::from_raw(-231700i32)), n1744);
    let n2121: ZN = zsel_n(n1828, zn_splat(P8::from_raw(-231700i32)), n1745);
    let n2122: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n2120);
    let n2123: ZN = zsel_n(n1869, n2120, n2122);
    let n2124: ZN = zsel_n(n1828, zn_splat(P8::from_raw(231700i32)), n1799);
    let n2125: ZN = zsel_n(n1828, zn_splat(P8::from_raw(-231700i32)), n1800);
    let n2126: ZN = zsel_n(n1501, zn_splat(P8::from_raw(0i32)), n2124);
    let n2127: ZN = zsel_n(n1869, n2124, n2126);
    let n2128: ZN = zsel_n(n1828, zn_splat(P8::from_raw(327680i32)), n1690);
    let n2129: ZN = zsel_n(n1828, zn_splat(P8::from_raw(231700i32)), n1745);
    let n2130: ZN = zsel_n(n1828, zn_splat(P8::from_raw(231700i32)), n1800);
    let n2132: ZN = zn_sub(n1357, zn_splat(P8::from_raw(65536i32)));
    let n2133: ZB = zb_not(n1491);
    let n2134: ZB = zb_and(n1490, n2133);
    let n2135: ZB = zb_and(n111, n2134);
    let n2136: ZB = zb_and(n1496, n2135);
    let n2137: ZB = zb_and(n1495, n2135);
    let n2138: ZB = zb_or(n2136, n2137);
    let n2139: ZB = zb_and(n1501, n2138);
    let n2140: ZB = zb_and(n1502, n2138);
    let n2141: ZB = zb_or(n2139, n2140);
    let n2142: ZB = zb_and(n1582, n2133);
    let n2143: ZB = zb_and(n111, n2142);
    let n2144: ZB = zb_and(n1496, n2143);
    let n2145: ZB = zb_and(n1495, n2143);
    let n2146: ZB = zb_or(n2144, n2145);
    let n2147: ZB = zb_and(n1501, n2146);
    let n2148: ZB = zb_and(n1502, n2146);
    let n2149: ZB = zb_or(n2147, n2148);
    let n2150: ZB = zb_and(n1651, n2133);
    let n2151: ZB = zb_and(n111, n2150);
    let n2152: ZB = zb_and(n1496, n2151);
    let n2153: ZB = zb_and(n1495, n2151);
    let n2154: ZB = zb_or(n2152, n2153);
    let n2155: ZB = zb_and(n1501, n2154);
    let n2156: ZB = zb_and(n1502, n2154);
    let n2157: ZB = zb_or(n2155, n2156);
    let n2158: ZB = zb_and(n1707, n2133);
    let n2159: ZB = zb_and(n111, n2158);
    let n2160: ZB = zb_and(n1496, n2159);
    let n2161: ZB = zb_and(n1495, n2159);
    let n2162: ZB = zb_or(n2160, n2161);
    let n2163: ZB = zb_and(n1501, n2162);
    let n2164: ZB = zb_and(n1502, n2162);
    let n2165: ZB = zb_or(n2163, n2164);
    let n2166: ZB = zb_and(n1762, n2133);
    let n2167: ZB = zb_and(n111, n2166);
    let n2168: ZB = zb_and(n1496, n2167);
    let n2169: ZB = zb_and(n1495, n2167);
    let n2170: ZB = zb_or(n2168, n2169);
    let n2171: ZB = zb_and(n1501, n2170);
    let n2172: ZB = zb_and(n1502, n2170);
    let n2173: ZB = zb_or(n2171, n2172);
    let n2174: ZB = zb_and(n1817, n2133);
    let n2175: ZB = zb_and(n111, n2174);
    let n2176: ZB = zb_and(n1496, n2175);
    let n2177: ZB = zb_and(n1495, n2175);
    let n2178: ZB = zb_or(n2176, n2177);
    let n2179: ZB = zb_and(n1501, n2178);
    let n2180: ZB = zb_and(n1502, n2178);
    let n2181: ZB = zb_or(n2179, n2180);
    let n2182: ZN = zsel_n(n1828, n2132, n1357);
    let n2183: ZB = zb_and(n1867, n2133);
    let n2184: ZB = zb_and(n1869, n2183);
    let n2185: ZB = zb_and(n1870, n2183);
    let n2186: ZB = zb_and(n1496, n2185);
    let n2187: ZB = zb_and(n1495, n2185);
    let n2188: ZB = zb_or(n2186, n2187);
    let n2189: ZB = zb_and(n1501, n2188);
    let n2190: ZB = zb_and(n1502, n2188);
    let n2191: ZB = zb_or(n2189, n2190);
    let n2192: ZB = zb_or(n2184, n2191);
    let n2193: ZB = zb_and(n1902, n2133);
    let n2194: ZB = zb_and(n1869, n2193);
    let n2195: ZB = zb_and(n1870, n2193);
    let n2196: ZB = zb_and(n1496, n2195);
    let n2197: ZB = zb_and(n1495, n2195);
    let n2198: ZB = zb_or(n2196, n2197);
    let n2199: ZB = zb_and(n1501, n2198);
    let n2200: ZB = zb_and(n1502, n2198);
    let n2201: ZB = zb_or(n2199, n2200);
    let n2202: ZB = zb_or(n2194, n2201);
    let n2203: ZB = zb_and(n1933, n2133);
    let n2204: ZB = zb_and(n1869, n2203);
    let n2205: ZB = zb_and(n1870, n2203);
    let n2206: ZB = zb_and(n1496, n2205);
    let n2207: ZB = zb_and(n1495, n2205);
    let n2208: ZB = zb_or(n2206, n2207);
    let n2209: ZB = zb_and(n1501, n2208);
    let n2210: ZB = zb_and(n1502, n2208);
    let n2211: ZB = zb_or(n2209, n2210);
    let n2212: ZB = zb_or(n2204, n2211);
    let n2213: ZB = zb_and(n1965, n2133);
    let n2214: ZB = zb_and(n1869, n2213);
    let n2215: ZB = zb_and(n1870, n2213);
    let n2216: ZB = zb_and(n1496, n2215);
    let n2217: ZB = zb_and(n1495, n2215);
    let n2218: ZB = zb_or(n2216, n2217);
    let n2219: ZB = zb_and(n1501, n2218);
    let n2220: ZB = zb_and(n1502, n2218);
    let n2221: ZB = zb_or(n2219, n2220);
    let n2222: ZB = zb_or(n2214, n2221);
    let n2223: ZB = zb_and(n2019, n2133);
    let n2224: ZB = zb_and(n1869, n2223);
    let n2225: ZB = zb_and(n1870, n2223);
    let n2226: ZB = zb_and(n1496, n2225);
    let n2227: ZB = zb_and(n1495, n2225);
    let n2228: ZB = zb_or(n2226, n2227);
    let n2229: ZB = zb_and(n1501, n2228);
    let n2230: ZB = zb_and(n1502, n2228);
    let n2231: ZB = zb_or(n2229, n2230);
    let n2232: ZB = zb_or(n2224, n2231);
    let n2233: ZB = zb_and(n2049, n2133);
    let n2234: ZB = zb_and(n1869, n2233);
    let n2235: ZB = zb_and(n1870, n2233);
    let n2236: ZB = zb_and(n1496, n2235);
    let n2237: ZB = zb_and(n1495, n2235);
    let n2238: ZB = zb_or(n2236, n2237);
    let n2239: ZB = zb_and(n1501, n2238);
    let n2240: ZB = zb_and(n1502, n2238);
    let n2241: ZB = zb_or(n2239, n2240);
    let n2242: ZB = zb_or(n2234, n2241);
    let n2243: ZB = zb_and(n2079, n2133);
    let n2244: ZB = zb_and(n1869, n2243);
    let n2245: ZB = zb_and(n1870, n2243);
    let n2246: ZB = zb_and(n1496, n2245);
    let n2247: ZB = zb_and(n1495, n2245);
    let n2248: ZB = zb_or(n2246, n2247);
    let n2249: ZB = zb_and(n1501, n2248);
    let n2250: ZB = zb_and(n1502, n2248);
    let n2251: ZB = zb_or(n2249, n2250);
    let n2252: ZB = zb_or(n2244, n2251);
    let n2253: ZB = zb_and(n2107, n2133);
    let n2254: ZB = zb_and(n1869, n2253);
    let n2255: ZB = zb_and(n1870, n2253);
    let n2256: ZB = zb_and(n1496, n2255);
    let n2257: ZB = zb_and(n1495, n2255);
    let n2258: ZB = zb_or(n2256, n2257);
    let n2259: ZB = zb_and(n1501, n2258);
    let n2260: ZB = zb_and(n1502, n2258);
    let n2261: ZB = zb_or(n2259, n2260);
    let n2262: ZB = zb_or(n2254, n2261);
    let n2263: ZB = zb_and(n741, n742);
    let n2264: ZB = zb_and(n752, n756);
    let n2265: ZB = zb_and(n770, n771);
    let n2266: ZB = zb_and(n794, n795);
    let n2267: ZB = zb_or(n2265, n2266);
    let n2268: ZB = zb_or(n2264, n2267);
    let n2269: ZB = zb_or(n2263, n2268);
    let n2270: ZB = zb_and(n821, n822);
    let n2271: ZB = zb_and(n830, n834);
    let n2272: ZB = zb_and(n845, n846);
    let n2273: ZB = zb_and(n861, n862);
    let n2274: ZB = zb_or(n2272, n2273);
    let n2275: ZB = zb_or(n2271, n2274);
    let n2276: ZB = zb_or(n2270, n2275);
    let n2277: ZB = zb_and(n888, n889);
    let n2278: ZB = zb_and(n897, n901);
    let n2279: ZB = zb_and(n912, n913);
    let n2280: ZB = zb_and(n928, n929);
    let n2281: ZB = zb_or(n2279, n2280);
    let n2282: ZB = zb_or(n2278, n2281);
    let n2283: ZB = zb_or(n2277, n2282);
    let n2284: ZB = zb_or(n2276, n2283);
    let n2285: ZB = zb_or(n2269, n2284);
    let n2286: ZB = zb_and(n962, n963);
    let n2287: ZB = zb_and(n971, n975);
    let n2288: ZB = zb_and(n986, n987);
    let n2289: ZB = zb_and(n1006, n1007);
    let n2290: ZB = zb_or(n2288, n2289);
    let n2291: ZB = zb_or(n2287, n2290);
    let n2292: ZB = zb_or(n2286, n2291);
    let n2293: ZB = zb_and(n1025, n1026);
    let n2294: ZB = zb_and(n1034, n1038);
    let n2295: ZB = zb_and(n1049, n1050);
    let n2296: ZB = zb_and(n1065, n1066);
    let n2297: ZB = zb_or(n2295, n2296);
    let n2298: ZB = zb_or(n2294, n2297);
    let n2299: ZB = zb_or(n2293, n2298);
    let n2300: ZB = zb_and(n1084, n1085);
    let n2301: ZB = zb_and(n1093, n1097);
    let n2302: ZB = zb_and(n1108, n1109);
    let n2303: ZB = zb_and(n1124, n1125);
    let n2304: ZB = zb_or(n2302, n2303);
    let n2305: ZB = zb_or(n2301, n2304);
    let n2306: ZB = zb_or(n2300, n2305);
    let n2307: ZB = zb_or(n2299, n2306);
    let n2308: ZB = zb_or(n2292, n2307);
    let n2309: ZB = zb_and(n1156, n1157);
    let n2310: ZB = zb_and(n1165, n1169);
    let n2311: ZB = zb_and(n1180, n1181);
    let n2312: ZB = zb_and(n1200, n1201);
    let n2313: ZB = zb_or(n2311, n2312);
    let n2314: ZB = zb_or(n2310, n2313);
    let n2315: ZB = zb_or(n2309, n2314);
    let n2316: ZB = zb_and(n1219, n1220);
    let n2317: ZB = zb_and(n1228, n1232);
    let n2318: ZB = zb_and(n1243, n1244);
    let n2319: ZB = zb_and(n1259, n1260);
    let n2320: ZB = zb_or(n2318, n2319);
    let n2321: ZB = zb_or(n2317, n2320);
    let n2322: ZB = zb_or(n2316, n2321);
    let n2323: ZB = zb_and(n1278, n1279);
    let n2324: ZB = zb_and(n1287, n1291);
    let n2325: ZB = zb_and(n1302, n1303);
    let n2326: ZB = zb_and(n1318, n1319);
    let n2327: ZB = zb_or(n2325, n2326);
    let n2328: ZB = zb_or(n2324, n2327);
    let n2329: ZB = zb_or(n2323, n2328);
    let n2330: ZB = zb_or(n2322, n2329);
    let n2331: ZB = zb_or(n2315, n2330);
    let n2332: ZB = zb_or(n2308, n2331);
    let n2333: ZB = zsel_b(n2308, n940, n1134);
    let n2334: ZB = zb_or(n2285, n2332);
    let n2335: ZB = zsel_b(n2285, n692, n2333);
    let n2336: ZB = zn_gt(n688, zn_splat(P8::from_raw(8388608i32)));
    let n2337: ZB = zb_and(n2334, n2336);
    let n2338: ZB = zb_and(n1338, n2334);
    let n2339: ZB = zb_or(n2337, n2338);
    let n2340: ZB = zb_and(n1336, n2336);
    let n2341: ZB = zb_or(n2339, n2340);
    let n2342: ZB = zsel_b(n2339, n2335, n1337);
    let n2343: ZB = zb_and(n1342, n2341);
    let n2344: ZB = zb_and(n1341, n2341);
    let n2345: ZB = zb_or(n2343, n2344);
    let n2346: ZB = zb_and(n1342, n2345);
    let n2347: ZB = zb_and(n1341, n2345);
    let n2348: ZB = zb_or(n2346, n2347);
    let n2349: ZB = zb_and(n1341, n2348);
    let n2350: ZB = zb_and(n1342, n2348);
    let n2351: ZB = zb_and(n242, n2349);
    let n2352: ZB = zb_and(n243, n2349);
    let n2353: ZB = zb_or(n2351, n2352);
    let n2354: ZB = zb_and(n245, n2350);
    let n2355: ZB = zb_and(n246, n2350);
    let n2356: ZB = zb_or(n2354, n2355);
    let n2357: ZB = zb_or(n2353, n2356);
    let n2358: ZB = zb_and(n1342, n2357);
    let n2359: ZB = zb_and(n1341, n2357);
    let n2360: ZB = zb_or(n2358, n2359);
    let n2361: ZB = zb_and(n1365, n2360);
    let n2362: ZB = zb_and(n1366, n2360);
    let n2363: ZB = zb_and(n1369, n2361);
    let n2364: ZB = zb_and(n769, n2361);
    let n2365: ZB = zb_and(n1372, n2364);
    let n2366: ZB = zb_and(n793, n2364);
    let n2367: ZB = zb_and(n1375, n2363);
    let n2368: ZB = zb_and(n1376, n2363);
    let n2369: ZB = zb_and(n1383, n2365);
    let n2370: ZB = zb_and(n1384, n2365);
    let n2371: ZB = zb_and(n769, n2366);
    let n2372: ZB = zb_or(n2369, n2370);
    let n2373: ZB = zb_or(n2367, n2368);
    let n2374: ZB = zb_or(n2371, n2372);
    let n2375: ZB = zb_or(n2373, n2374);
    let n2376: ZB = zb_and(n1369, n2362);
    let n2377: ZB = zb_and(n769, n2362);
    let n2378: ZB = zb_or(n2376, n2377);
    let n2379: ZB = zb_or(n2375, n2378);
    let n2380: ZB = zb_and(n1412, n2379);
    let n2381: ZB = zb_and(n1411, n2379);
    let n2382: ZB = zb_or(n2380, n2381);
    let n2383: ZB = zb_and(n1419, n2382);
    let n2384: ZB = zb_and(n1420, n2382);
    let n2385: ZB = zb_or(n2383, n2384);
    let n2386: ZB = zb_and(n1342, n2385);
    let n2387: ZB = zb_and(n1341, n2385);
    let n2388: ZB = zb_or(n2386, n2387);
    let n2389: ZB = zb_and(n1451, n2388);
    let n2390: ZB = zb_and(n1452, n2388);
    let n2391: ZB = zb_or(n2389, n2390);
    let n2392: ZB = zb_and(n1467, n2391);
    let n2393: ZB = zb_and(n1468, n2391);
    let n2394: ZB = zb_or(n2392, n2393);
    let n2397: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n2398: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2397);
    let n2399: ZN = zsel_n(n2336, n2398, n2397);
    let n2400: ZN = zsel_n(n2339, n2399, n2397);
    let n2402: ZB = zb_and(n1383, n2362);
    let n2403: ZB = zb_and(n1384, n2362);
    let n2404: ZB = zb_or(n2402, n2403);
    let n2405: ZB = zb_or(n2375, n2404);
    let n2406: ZB = zb_and(n1533, n2405);
    let n2407: ZB = zb_and(n1532, n2405);
    let n2408: ZB = zb_or(n2406, n2407);
    let n2409: ZB = zb_and(n1419, n2408);
    let n2410: ZB = zb_and(n1420, n2408);
    let n2411: ZB = zb_or(n2409, n2410);
    let n2412: ZB = zb_and(n1544, n2411);
    let n2413: ZB = zb_and(n1543, n2411);
    let n2414: ZB = zb_or(n2412, n2413);
    let n2415: ZB = zb_and(n1544, n2414);
    let n2416: ZB = zb_and(n1543, n2414);
    let n2417: ZB = zb_or(n2415, n2416);
    let n2418: ZB = zb_and(n1543, n2417);
    let n2419: ZB = zb_and(n1544, n2417);
    let n2420: ZB = zb_or(n2418, n2419);
    let n2421: ZB = zb_and(n1543, n2420);
    let n2422: ZB = zb_and(n1544, n2420);
    let n2423: ZB = zb_or(n2421, n2422);
    let n2424: ZB = zb_and(n1342, n2423);
    let n2425: ZB = zb_and(n1341, n2423);
    let n2426: ZB = zb_or(n2424, n2425);
    let n2427: ZB = zb_and(n1451, n2426);
    let n2428: ZB = zb_and(n1452, n2426);
    let n2429: ZB = zb_or(n2427, n2428);
    let n2430: ZB = zb_and(n1467, n2429);
    let n2431: ZB = zb_and(n1468, n2429);
    let n2432: ZB = zb_or(n2430, n2431);
    let n2435: ZB = zb_and(n1375, n2362);
    let n2436: ZB = zb_and(n1376, n2362);
    let n2437: ZB = zb_or(n2435, n2436);
    let n2438: ZB = zb_or(n2375, n2437);
    let n2439: ZB = zb_and(n1602, n2438);
    let n2440: ZB = zb_and(n1601, n2438);
    let n2441: ZB = zb_or(n2439, n2440);
    let n2442: ZB = zb_and(n1419, n2441);
    let n2443: ZB = zb_and(n1420, n2441);
    let n2444: ZB = zb_or(n2442, n2443);
    let n2445: ZB = zb_and(n1613, n2444);
    let n2446: ZB = zb_and(n1612, n2444);
    let n2447: ZB = zb_or(n2445, n2446);
    let n2448: ZB = zb_and(n1613, n2447);
    let n2449: ZB = zb_and(n1612, n2447);
    let n2450: ZB = zb_or(n2448, n2449);
    let n2451: ZB = zb_and(n1612, n2450);
    let n2452: ZB = zb_and(n1613, n2450);
    let n2453: ZB = zb_or(n2451, n2452);
    let n2454: ZB = zb_and(n1612, n2453);
    let n2455: ZB = zb_and(n1613, n2453);
    let n2456: ZB = zb_or(n2454, n2455);
    let n2457: ZB = zb_and(n1342, n2456);
    let n2458: ZB = zb_and(n1341, n2456);
    let n2459: ZB = zb_or(n2457, n2458);
    let n2460: ZB = zb_and(n1451, n2459);
    let n2461: ZB = zb_and(n1452, n2459);
    let n2462: ZB = zb_or(n2460, n2461);
    let n2463: ZB = zb_and(n1467, n2462);
    let n2464: ZB = zb_and(n1468, n2462);
    let n2465: ZB = zb_or(n2463, n2464);
    let n2468: ZB = zb_and(n240, n2388);
    let n2469: ZB = zb_and(r_c266, n2388);
    let n2470: ZB = zb_and(n1432, n2468);
    let n2471: ZB = zb_and(n1433, n2468);
    let n2472: ZB = zb_and(n1436, n2471);
    let n2473: ZB = zb_and(n1435, n2471);
    let n2474: ZB = zb_or(n2472, n2473);
    let n2475: ZB = zb_and(n1436, n2474);
    let n2476: ZB = zb_and(n1435, n2474);
    let n2477: ZB = zb_or(n2475, n2476);
    let n2478: ZB = zb_and(n1435, n2477);
    let n2479: ZB = zb_and(n1436, n2477);
    let n2480: ZB = zb_and(n1439, n2479);
    let n2481: ZB = zb_and(n1438, n2479);
    let n2482: ZB = zb_or(n2480, n2481);
    let n2483: ZB = zb_and(n1439, n2482);
    let n2484: ZB = zb_and(n1438, n2482);
    let n2485: ZB = zb_or(n2483, n2484);
    let n2486: ZB = zb_and(n1438, n2485);
    let n2487: ZB = zb_and(n1439, n2485);
    let n2488: ZB = zb_or(n2486, n2487);
    let n2489: ZB = zb_or(n2478, n2488);
    let n2490: ZB = zb_and(n1443, n2489);
    let n2491: ZB = zb_and(n1442, n2489);
    let n2492: ZB = zb_or(n2490, n2491);
    let n2493: ZB = zb_or(n2470, n2492);
    let n2494: ZB = zb_or(n2469, n2493);
    let n2495: ZB = zb_and(n1451, n2494);
    let n2496: ZB = zb_and(n1452, n2494);
    let n2497: ZB = zb_or(n2495, n2496);
    let n2498: ZB = zb_and(n1467, n2497);
    let n2499: ZB = zb_and(n1468, n2497);
    let n2500: ZB = zb_or(n2498, n2499);
    let n2503: ZB = zb_and(n240, n2426);
    let n2504: ZB = zb_and(r_c266, n2426);
    let n2505: ZB = zb_and(n1432, n2503);
    let n2506: ZB = zb_and(n1433, n2503);
    let n2507: ZB = zb_and(n1436, n2506);
    let n2508: ZB = zb_and(n1435, n2506);
    let n2509: ZB = zb_or(n2507, n2508);
    let n2510: ZB = zb_and(n1436, n2509);
    let n2511: ZB = zb_and(n1435, n2509);
    let n2512: ZB = zb_or(n2510, n2511);
    let n2513: ZB = zb_and(n1435, n2512);
    let n2514: ZB = zb_and(n1436, n2512);
    let n2515: ZB = zb_and(n1439, n2514);
    let n2516: ZB = zb_and(n1438, n2514);
    let n2517: ZB = zb_or(n2515, n2516);
    let n2518: ZB = zb_and(n1439, n2517);
    let n2519: ZB = zb_and(n1438, n2517);
    let n2520: ZB = zb_or(n2518, n2519);
    let n2521: ZB = zb_and(n1438, n2520);
    let n2522: ZB = zb_and(n1439, n2520);
    let n2523: ZB = zb_or(n2521, n2522);
    let n2524: ZB = zb_or(n2513, n2523);
    let n2525: ZB = zb_and(n1443, n2524);
    let n2526: ZB = zb_and(n1442, n2524);
    let n2527: ZB = zb_or(n2525, n2526);
    let n2528: ZB = zb_or(n2505, n2527);
    let n2529: ZB = zb_or(n2504, n2528);
    let n2530: ZB = zb_and(n1451, n2529);
    let n2531: ZB = zb_and(n1452, n2529);
    let n2532: ZB = zb_or(n2530, n2531);
    let n2533: ZB = zb_and(n1467, n2532);
    let n2534: ZB = zb_and(n1468, n2532);
    let n2535: ZB = zb_or(n2533, n2534);
    let n2538: ZB = zb_and(n240, n2459);
    let n2539: ZB = zb_and(r_c266, n2459);
    let n2540: ZB = zb_and(n1432, n2538);
    let n2541: ZB = zb_and(n1433, n2538);
    let n2542: ZB = zb_and(n1436, n2541);
    let n2543: ZB = zb_and(n1435, n2541);
    let n2544: ZB = zb_or(n2542, n2543);
    let n2545: ZB = zb_and(n1436, n2544);
    let n2546: ZB = zb_and(n1435, n2544);
    let n2547: ZB = zb_or(n2545, n2546);
    let n2548: ZB = zb_and(n1435, n2547);
    let n2549: ZB = zb_and(n1436, n2547);
    let n2550: ZB = zb_and(n1439, n2549);
    let n2551: ZB = zb_and(n1438, n2549);
    let n2552: ZB = zb_or(n2550, n2551);
    let n2553: ZB = zb_and(n1439, n2552);
    let n2554: ZB = zb_and(n1438, n2552);
    let n2555: ZB = zb_or(n2553, n2554);
    let n2556: ZB = zb_and(n1438, n2555);
    let n2557: ZB = zb_and(n1439, n2555);
    let n2558: ZB = zb_or(n2556, n2557);
    let n2559: ZB = zb_or(n2548, n2558);
    let n2560: ZB = zb_and(n1443, n2559);
    let n2561: ZB = zb_and(n1442, n2559);
    let n2562: ZB = zb_or(n2560, n2561);
    let n2563: ZB = zb_or(n2540, n2562);
    let n2564: ZB = zb_or(n2539, n2563);
    let n2565: ZB = zb_and(n1451, n2564);
    let n2566: ZB = zb_and(n1452, n2564);
    let n2567: ZB = zb_or(n2565, n2566);
    let n2568: ZB = zb_and(n1467, n2567);
    let n2569: ZB = zb_and(n1468, n2567);
    let n2570: ZB = zb_or(n2568, n2569);
    let n2573: ZB = zb_and(n1828, n2391);
    let n2574: ZB = zb_and(n1829, n2391);
    let n2575: ZB = zb_and(n1416, n2573);
    let n2576: ZB = zb_and(n1456, n2573);
    let n2577: ZB = zb_or(n2575, n2576);
    let n2578: ZB = zb_and(n1458, n2577);
    let n2579: ZB = zb_and(n1459, n2577);
    let n2580: ZB = zb_and(n1460, n2579);
    let n2581: ZB = zb_and(n1461, n2579);
    let n2582: ZB = zb_or(n2580, n2581);
    let n2583: ZB = zb_or(n2578, n2582);
    let n2584: ZB = zb_and(n1465, n2583);
    let n2585: ZB = zb_and(n1464, n2583);
    let n2586: ZB = zb_or(n2584, n2585);
    let n2587: ZB = zb_or(n2574, n2586);
    let n2588: ZB = zb_and(n1467, n2587);
    let n2589: ZB = zb_and(n1468, n2587);
    let n2590: ZB = zb_or(n2588, n2589);
    let n2591: ZB = zb_and(n1468, n2590);
    let n2592: ZB = zb_and(n1869, n2591);
    let n2593: ZB = zb_and(n1870, n2591);
    let n2594: ZB = zb_or(n2592, n2593);
    let n2595: ZB = zb_and(n1828, n2429);
    let n2596: ZB = zb_and(n1829, n2429);
    let n2597: ZB = zb_or(n2595, n2596);
    let n2598: ZB = zb_and(n1467, n2597);
    let n2599: ZB = zb_and(n1468, n2597);
    let n2600: ZB = zb_or(n2598, n2599);
    let n2601: ZB = zb_and(n1468, n2600);
    let n2602: ZB = zb_and(n1869, n2601);
    let n2603: ZB = zb_and(n1870, n2601);
    let n2604: ZB = zb_or(n2602, n2603);
    let n2605: ZB = zb_and(n1828, n2462);
    let n2606: ZB = zb_and(n1829, n2462);
    let n2607: ZB = zb_or(n2605, n2606);
    let n2608: ZB = zb_and(n1467, n2607);
    let n2609: ZB = zb_and(n1468, n2607);
    let n2610: ZB = zb_or(n2608, n2609);
    let n2611: ZB = zb_and(n1468, n2610);
    let n2612: ZB = zb_and(n1869, n2611);
    let n2613: ZB = zb_and(n1870, n2611);
    let n2614: ZB = zb_or(n2612, n2613);
    let n2615: ZB = zb_or(n2573, n2574);
    let n2616: ZB = zb_and(n1467, n2615);
    let n2617: ZB = zb_and(n1468, n2615);
    let n2618: ZB = zb_or(n2616, n2617);
    let n2619: ZB = zb_and(n1468, n2618);
    let n2620: ZB = zb_and(n1869, n2619);
    let n2621: ZB = zb_and(n1870, n2619);
    let n2622: ZB = zb_or(n2620, n2621);
    let n2623: ZB = zb_and(n1828, n2497);
    let n2624: ZB = zb_and(n1829, n2497);
    let n2625: ZB = zb_and(n1416, n2623);
    let n2626: ZB = zb_and(n1456, n2623);
    let n2627: ZB = zb_or(n2625, n2626);
    let n2628: ZB = zb_and(n1458, n2627);
    let n2629: ZB = zb_and(n1459, n2627);
    let n2630: ZB = zb_and(n1460, n2629);
    let n2631: ZB = zb_and(n1461, n2629);
    let n2632: ZB = zb_or(n2630, n2631);
    let n2633: ZB = zb_or(n2628, n2632);
    let n2634: ZB = zb_and(n1465, n2633);
    let n2635: ZB = zb_and(n1464, n2633);
    let n2636: ZB = zb_or(n2634, n2635);
    let n2637: ZB = zb_or(n2624, n2636);
    let n2638: ZB = zb_and(n1467, n2637);
    let n2639: ZB = zb_and(n1468, n2637);
    let n2640: ZB = zb_or(n2638, n2639);
    let n2641: ZB = zb_and(n1468, n2640);
    let n2642: ZB = zb_and(n1869, n2641);
    let n2643: ZB = zb_and(n1870, n2641);
    let n2644: ZB = zb_or(n2642, n2643);
    let n2645: ZB = zb_and(n1828, n2532);
    let n2646: ZB = zb_and(n1829, n2532);
    let n2647: ZB = zb_or(n2645, n2646);
    let n2648: ZB = zb_and(n1467, n2647);
    let n2649: ZB = zb_and(n1468, n2647);
    let n2650: ZB = zb_or(n2648, n2649);
    let n2651: ZB = zb_and(n1468, n2650);
    let n2652: ZB = zb_and(n1869, n2651);
    let n2653: ZB = zb_and(n1870, n2651);
    let n2654: ZB = zb_or(n2652, n2653);
    let n2655: ZB = zb_and(n1828, n2567);
    let n2656: ZB = zb_and(n1829, n2567);
    let n2657: ZB = zb_or(n2655, n2656);
    let n2658: ZB = zb_and(n1467, n2657);
    let n2659: ZB = zb_and(n1468, n2657);
    let n2660: ZB = zb_or(n2658, n2659);
    let n2661: ZB = zb_and(n1468, n2660);
    let n2662: ZB = zb_and(n1869, n2661);
    let n2663: ZB = zb_and(n1870, n2661);
    let n2664: ZB = zb_or(n2662, n2663);
    let n2665: ZB = zb_or(n2623, n2624);
    let n2666: ZB = zb_and(n1467, n2665);
    let n2667: ZB = zb_and(n1468, n2665);
    let n2668: ZB = zb_or(n2666, n2667);
    let n2669: ZB = zb_and(n1468, n2668);
    let n2670: ZB = zb_and(n1869, n2669);
    let n2671: ZB = zb_and(n1870, n2669);
    let n2672: ZB = zb_or(n2670, n2671);
    let n2675: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c320);
    let n2678: ZB = zn_le(r_c253, zn_splat(P8::from_raw(0i32)));
    let n2679: ZB = zb_not(n2675);
    let n2680: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c321);
    let n2681: ZB = zb_not(n2680);
    let n2682: ZB = zb_or(n2679, n2681);
    let n2683: ZB = zb_not(n2682);
    let n2684: ZN = zn_add(r_c318, r_c320);
    let n2685: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n2684);
    let n2686: ZN = zn_flr(n2685);
    let n2687: ZB = zn_gt(n2686, zn_splat(P8::from_raw(0i32)));
    let n2688: ZB = zn_le(n2686, zn_splat(P8::from_raw(0i32)));
    let n2689: ZB = zn_lt(n2686, zn_splat(P8::from_raw(0i32)));
    let n2690: ZB = zn_ge(n2686, zn_splat(P8::from_raw(0i32)));
    let n2691: ZN = zsel_n(n2689, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n2692: ZN = zsel_n(n2687, zn_splat(P8::from_raw(65536i32)), n2691);
    let n2693: ZN = zn_abs(n2686);
    let n2694: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n172);
    let n2695: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n147);
    let n2696: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n2695);
    let n2697: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n2693);
    let n2698: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n2693);
    let n2699: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n2693);
    let n2700: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n2693);
    let n2701: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n2693);
    let n2702: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n2693);
    let n2703: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n2693);
    let n2704: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n2693);
    let n2705: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n2693);
    let n2706: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n2693);
    let n2707: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n2693);
    let n2708: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n2693);
    let n2709: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n2693);
    let n2710: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n2693);
    let n2711: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n2693);
    let n2712: ZN = zn_add(r_c319, r_c321);
    let n2713: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n2712);
    let n2714: ZN = zn_flr(n2713);
    let n2715: ZB = zn_gt(n2714, zn_splat(P8::from_raw(0i32)));
    let n2716: ZB = zn_le(n2714, zn_splat(P8::from_raw(0i32)));
    let n2717: ZB = zn_lt(n2714, zn_splat(P8::from_raw(0i32)));
    let n2718: ZB = zn_ge(n2714, zn_splat(P8::from_raw(0i32)));
    let n2719: ZN = zsel_n(n2717, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n2720: ZN = zsel_n(n2715, zn_splat(P8::from_raw(65536i32)), n2719);
    let n2721: ZN = zn_abs(n2714);
    let n2722: ZB = zn_gt(n2720, zn_splat(P8::from_raw(0i32)));
    let n2723: ZB = zn_le(n2720, zn_splat(P8::from_raw(0i32)));
    let n2724: ZN = zn_add(n146, n2720);
    let n2725: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n2724);
    let n2726: ZN = zn_add(n147, n2720);
    let n2727: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n2726);
    let n2728: ZN = zn_add(r_c273, n2720);
    let n2729: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n2721);
    let n2730: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n2721);
    let n2731: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2728);
    let n2732: ZN = zn_add(n2720, n2731);
    let n2733: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n2732);
    let n2734: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n2731);
    let n2735: ZN = zn_add(n2720, n2734);
    let n2736: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n2735);
    let n2737: ZN = zn_add(n2720, n2728);
    let n2738: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n2721);
    let n2739: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n2721);
    let n2740: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2737);
    let n2741: ZN = zn_add(n2720, n2740);
    let n2742: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n2741);
    let n2743: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n2740);
    let n2744: ZN = zn_add(n2720, n2743);
    let n2745: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n2744);
    let n2746: ZN = zn_add(n2720, n2737);
    let n2747: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n2721);
    let n2748: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n2721);
    let n2749: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2746);
    let n2750: ZN = zn_add(n2720, n2749);
    let n2751: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n2750);
    let n2752: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n2749);
    let n2753: ZN = zn_add(n2720, n2752);
    let n2754: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n2753);
    let n2755: ZN = zn_add(n2720, n2746);
    let n2756: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n2721);
    let n2757: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n2721);
    let n2758: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2755);
    let n2759: ZN = zn_add(n2720, n2758);
    let n2760: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n2759);
    let n2761: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n2758);
    let n2762: ZN = zn_add(n2720, n2761);
    let n2763: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n2762);
    let n2764: ZN = zn_add(n2720, n2755);
    let n2765: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n2721);
    let n2766: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n2721);
    let n2767: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2764);
    let n2768: ZN = zn_add(n2720, n2767);
    let n2769: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n2768);
    let n2770: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n2767);
    let n2771: ZN = zn_add(n2720, n2770);
    let n2772: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n2771);
    let n2773: ZN = zn_add(n2720, n2764);
    let n2774: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n2721);
    let n2775: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n2721);
    let n2776: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2773);
    let n2777: ZN = zn_add(n2720, n2776);
    let n2778: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n2777);
    let n2779: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n2776);
    let n2780: ZN = zn_add(n2720, n2779);
    let n2781: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n2780);
    let n2782: ZN = zn_add(n2720, n2773);
    let n2783: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n2721);
    let n2784: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n2721);
    let n2785: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n2782);
    let n2786: ZN = zn_add(n2720, n2785);
    let n2787: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n2786);
    let n2788: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n2785);
    let n2789: ZN = zn_add(n2720, n2788);
    let n2790: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n2789);
    let n2791: ZN = zn_add(n2720, n2782);
    let n2792: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n2721);
    let n2793: ZB = zn_gt(r_c255, zn_splat(P8::from_raw(0i32)));
    let n2794: ZB = zn_le(r_c255, zn_splat(P8::from_raw(0i32)));
    let n2795: ZB = zb_not(n257);
    let n2796: ZB = zb_and(n111, n2795);
    let n2797: ZB = zb_and(n258, n2678);
    let n2798: ZB = zb_or(n2796, n2797);
    let n2799: ZB = zb_and(n2675, n2798);
    let n2800: ZB = zb_and(n2679, n2798);
    let n2801: ZB = zb_or(n2799, n2800);
    let n2802: ZB = zb_and(n2682, n2801);
    let n2803: ZB = zb_and(n2683, n2801);
    let n2804: ZB = zb_and(n2687, n2802);
    let n2805: ZB = zb_and(n2688, n2802);
    let n2806: ZB = zb_and(n2689, n2805);
    let n2807: ZB = zb_and(n2690, n2805);
    let n2808: ZB = zb_or(n2806, n2807);
    let n2809: ZB = zb_or(n2804, n2808);
    let n2810: ZN = zn_add(n251, n2692);
    let n2811: ZB = zn_tile_flag_at(g.cache, g.cart, n2810, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2812: ZB = zb_not(n2811);
    let n2813: ZB = zb_and(n2809, n2812);
    let n2814: ZB = zb_and(n2809, n2811);
    let n2815: ZB = zb_or(n2813, n2814);
    let n2816: ZB = zb_and(n2812, n2815);
    let n2817: ZB = zb_and(n2811, n2815);
    let n2818: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n2810);
    let n2819: ZB = zn_le(zn_splat(P8::from_raw(1572864i32)), n2810);
    let n2820: ZB = zb_and(n2816, n2818);
    let n2821: ZB = zb_and(n2816, n2819);
    let n2822: ZB = zb_or(n2820, n2821);
    let n2823: ZB = zb_and(n2694, n2818);
    let n2824: ZB = zb_not(n2823);
    let n2825: ZB = zb_and(n2822, n2823);
    let n2826: ZB = zb_and(n2822, n2824);
    let n2827: ZN = zn_add(n252, n2692);
    let n2828: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n2827);
    let n2829: ZB = zb_or(n2825, n2826);
    let n2830: ZB = zb_and(n2823, n2828);
    let n2831: ZB = zb_not(n2830);
    let n2832: ZB = zb_and(n2829, n2830);
    let n2833: ZB = zb_and(n2829, n2831);
    let n2834: ZB = zb_or(n2832, n2833);
    let n2835: ZB = zb_and(n2696, n2830);
    let n2836: ZB = zb_not(n2835);
    let n2837: ZB = zb_and(n2834, n2835);
    let n2838: ZB = zb_and(n2834, n2836);
    let n2839: ZB = zb_or(n2837, n2838);
    let n2840: ZB = zb_or(n2817, n2839);
    let n2841: ZB = zb_or(n2811, n2835);
    let n2842: ZB = zb_not(n2841);
    let n2843: ZB = zb_and(n2840, n2842);
    let n2844: ZB = zb_and(n2840, n2841);
    let n2845: ZN = zn_add(r_c272, n2692);
    let n2846: ZB = zb_and(n2697, n2843);
    let n2847: ZB = zb_and(n2698, n2843);
    let n2848: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2845);
    let n2849: ZN = zn_add(n2692, n2848);
    let n2850: ZB = zn_tile_flag_at(g.cache, g.cart, n2849, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2851: ZB = zb_not(n2850);
    let n2852: ZB = zb_and(n2846, n2851);
    let n2853: ZB = zb_and(n2846, n2850);
    let n2854: ZB = zb_or(n2852, n2853);
    let n2855: ZB = zb_and(n2851, n2854);
    let n2856: ZB = zb_and(n2850, n2854);
    let n2857: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n2849);
    let n2858: ZB = zn_le(zn_splat(P8::from_raw(1572864i32)), n2849);
    let n2859: ZB = zb_and(n2855, n2857);
    let n2860: ZB = zb_and(n2855, n2858);
    let n2861: ZB = zb_or(n2859, n2860);
    let n2862: ZB = zb_and(n2694, n2857);
    let n2863: ZB = zb_not(n2862);
    let n2864: ZB = zb_and(n2861, n2862);
    let n2865: ZB = zb_and(n2861, n2863);
    let n2866: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n2848);
    let n2867: ZN = zn_add(n2692, n2866);
    let n2868: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n2867);
    let n2869: ZB = zb_or(n2864, n2865);
    let n2870: ZB = zb_and(n2862, n2868);
    let n2871: ZB = zb_not(n2870);
    let n2872: ZB = zb_and(n2869, n2870);
    let n2873: ZB = zb_and(n2869, n2871);
    let n2874: ZB = zb_or(n2872, n2873);
    let n2875: ZB = zb_and(n2696, n2870);
    let n2876: ZB = zb_not(n2875);
    let n2877: ZB = zb_and(n2874, n2875);
    let n2878: ZB = zb_and(n2874, n2876);
    let n2879: ZB = zb_or(n2877, n2878);
    let n2880: ZB = zb_or(n2856, n2879);
    let n2881: ZB = zb_or(n2850, n2875);
    let n2882: ZB = zb_not(n2881);
    let n2883: ZB = zb_and(n2880, n2882);
    let n2884: ZB = zb_and(n2880, n2881);
    let n2885: ZN = zn_add(n2692, n2845);
    let n2886: ZB = zb_and(n2699, n2883);
    let n2887: ZB = zb_and(n2700, n2883);
    let n2888: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2885);
    let n2889: ZN = zn_add(n2692, n2888);
    let n2890: ZB = zn_tile_flag_at(g.cache, g.cart, n2889, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2891: ZB = zb_not(n2890);
    let n2892: ZB = zb_and(n2886, n2891);
    let n2893: ZB = zb_and(n2886, n2890);
    let n2894: ZB = zb_or(n2892, n2893);
    let n2895: ZB = zb_and(n2891, n2894);
    let n2896: ZB = zb_and(n2890, n2894);
    let n2897: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n2889);
    let n2898: ZB = zn_le(zn_splat(P8::from_raw(1572864i32)), n2889);
    let n2899: ZB = zb_and(n2895, n2897);
    let n2900: ZB = zb_and(n2895, n2898);
    let n2901: ZB = zb_or(n2899, n2900);
    let n2902: ZB = zb_and(n2694, n2897);
    let n2903: ZB = zb_not(n2902);
    let n2904: ZB = zb_and(n2901, n2902);
    let n2905: ZB = zb_and(n2901, n2903);
    let n2906: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n2888);
    let n2907: ZN = zn_add(n2692, n2906);
    let n2908: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n2907);
    let n2909: ZB = zb_or(n2904, n2905);
    let n2910: ZB = zb_and(n2902, n2908);
    let n2911: ZB = zb_not(n2910);
    let n2912: ZB = zb_and(n2909, n2910);
    let n2913: ZB = zb_and(n2909, n2911);
    let n2914: ZB = zb_or(n2912, n2913);
    let n2915: ZB = zb_and(n2696, n2910);
    let n2916: ZB = zb_not(n2915);
    let n2917: ZB = zb_and(n2914, n2915);
    let n2918: ZB = zb_and(n2914, n2916);
    let n2919: ZB = zb_or(n2917, n2918);
    let n2920: ZB = zb_or(n2896, n2919);
    let n2921: ZB = zb_or(n2890, n2915);
    let n2922: ZB = zb_not(n2921);
    let n2923: ZB = zb_and(n2920, n2922);
    let n2924: ZB = zb_and(n2920, n2921);
    let n2925: ZN = zn_add(n2692, n2885);
    let n2926: ZB = zb_and(n2701, n2923);
    let n2927: ZB = zb_and(n2702, n2923);
    let n2928: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2925);
    let n2929: ZN = zn_add(n2692, n2928);
    let n2930: ZB = zn_tile_flag_at(g.cache, g.cart, n2929, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2931: ZB = zb_not(n2930);
    let n2932: ZB = zb_and(n2926, n2931);
    let n2933: ZB = zb_and(n2926, n2930);
    let n2934: ZB = zb_or(n2932, n2933);
    let n2935: ZB = zb_and(n2931, n2934);
    let n2936: ZB = zb_and(n2930, n2934);
    let n2937: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n2929);
    let n2938: ZB = zn_le(zn_splat(P8::from_raw(1572864i32)), n2929);
    let n2939: ZB = zb_and(n2935, n2937);
    let n2940: ZB = zb_and(n2935, n2938);
    let n2941: ZB = zb_or(n2939, n2940);
    let n2942: ZB = zb_and(n2694, n2937);
    let n2943: ZB = zb_not(n2942);
    let n2944: ZB = zb_and(n2941, n2942);
    let n2945: ZB = zb_and(n2941, n2943);
    let n2946: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n2928);
    let n2947: ZN = zn_add(n2692, n2946);
    let n2948: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n2947);
    let n2949: ZB = zb_or(n2944, n2945);
    let n2950: ZB = zb_and(n2942, n2948);
    let n2951: ZB = zb_not(n2950);
    let n2952: ZB = zb_and(n2949, n2950);
    let n2953: ZB = zb_and(n2949, n2951);
    let n2954: ZB = zb_or(n2952, n2953);
    let n2955: ZB = zb_and(n2696, n2950);
    let n2956: ZB = zb_not(n2955);
    let n2957: ZB = zb_and(n2954, n2955);
    let n2958: ZB = zb_and(n2954, n2956);
    let n2959: ZB = zb_or(n2957, n2958);
    let n2960: ZB = zb_or(n2936, n2959);
    let n2961: ZB = zb_or(n2930, n2955);
    let n2962: ZB = zb_not(n2961);
    let n2963: ZB = zb_and(n2960, n2962);
    let n2964: ZB = zb_and(n2960, n2961);
    let n2965: ZN = zn_add(n2692, n2925);
    let n2966: ZB = zb_and(n2703, n2963);
    let n2967: ZB = zb_and(n2704, n2963);
    let n2968: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2965);
    let n2969: ZN = zn_add(n2692, n2968);
    let n2970: ZB = zn_tile_flag_at(g.cache, g.cart, n2969, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2971: ZB = zb_not(n2970);
    let n2972: ZB = zb_and(n2966, n2971);
    let n2973: ZB = zb_and(n2966, n2970);
    let n2974: ZB = zb_or(n2972, n2973);
    let n2975: ZB = zb_and(n2971, n2974);
    let n2976: ZB = zb_and(n2970, n2974);
    let n2977: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n2969);
    let n2978: ZB = zn_le(zn_splat(P8::from_raw(1572864i32)), n2969);
    let n2979: ZB = zb_and(n2975, n2977);
    let n2980: ZB = zb_and(n2975, n2978);
    let n2981: ZB = zb_or(n2979, n2980);
    let n2982: ZB = zb_and(n2694, n2977);
    let n2983: ZB = zb_not(n2982);
    let n2984: ZB = zb_and(n2981, n2982);
    let n2985: ZB = zb_and(n2981, n2983);
    let n2986: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n2968);
    let n2987: ZN = zn_add(n2692, n2986);
    let n2988: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n2987);
    let n2989: ZB = zb_or(n2984, n2985);
    let n2990: ZB = zb_and(n2982, n2988);
    let n2991: ZB = zb_not(n2990);
    let n2992: ZB = zb_and(n2989, n2990);
    let n2993: ZB = zb_and(n2989, n2991);
    let n2994: ZB = zb_or(n2992, n2993);
    let n2995: ZB = zb_and(n2696, n2990);
    let n2996: ZB = zb_not(n2995);
    let n2997: ZB = zb_and(n2994, n2995);
    let n2998: ZB = zb_and(n2994, n2996);
    let n2999: ZB = zb_or(n2997, n2998);
    let n3000: ZB = zb_or(n2976, n2999);
    let n3001: ZB = zb_or(n2970, n2995);
    let n3002: ZB = zb_not(n3001);
    let n3003: ZB = zb_and(n3000, n3002);
    let n3004: ZB = zb_and(n3000, n3001);
    let n3005: ZN = zn_add(n2692, n2965);
    let n3006: ZB = zb_and(n2705, n3003);
    let n3007: ZB = zb_and(n2706, n3003);
    let n3008: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3005);
    let n3009: ZN = zn_add(n2692, n3008);
    let n3010: ZB = zn_tile_flag_at(g.cache, g.cart, n3009, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3011: ZB = zb_not(n3010);
    let n3012: ZB = zb_and(n3006, n3011);
    let n3013: ZB = zb_and(n3006, n3010);
    let n3014: ZB = zb_or(n3012, n3013);
    let n3015: ZB = zb_and(n3011, n3014);
    let n3016: ZB = zb_and(n3010, n3014);
    let n3017: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n3009);
    let n3018: ZB = zn_le(zn_splat(P8::from_raw(1572864i32)), n3009);
    let n3019: ZB = zb_and(n3015, n3017);
    let n3020: ZB = zb_and(n3015, n3018);
    let n3021: ZB = zb_or(n3019, n3020);
    let n3022: ZB = zb_and(n2694, n3017);
    let n3023: ZB = zb_not(n3022);
    let n3024: ZB = zb_and(n3021, n3022);
    let n3025: ZB = zb_and(n3021, n3023);
    let n3026: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n3008);
    let n3027: ZN = zn_add(n2692, n3026);
    let n3028: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n3027);
    let n3029: ZB = zb_or(n3024, n3025);
    let n3030: ZB = zb_and(n3022, n3028);
    let n3031: ZB = zb_not(n3030);
    let n3032: ZB = zb_and(n3029, n3030);
    let n3033: ZB = zb_and(n3029, n3031);
    let n3034: ZB = zb_or(n3032, n3033);
    let n3035: ZB = zb_and(n2696, n3030);
    let n3036: ZB = zb_not(n3035);
    let n3037: ZB = zb_and(n3034, n3035);
    let n3038: ZB = zb_and(n3034, n3036);
    let n3039: ZB = zb_or(n3037, n3038);
    let n3040: ZB = zb_or(n3016, n3039);
    let n3041: ZB = zb_or(n3010, n3035);
    let n3042: ZB = zb_not(n3041);
    let n3043: ZB = zb_and(n3040, n3042);
    let n3044: ZB = zb_and(n3040, n3041);
    let n3045: ZN = zn_add(n2692, n3005);
    let n3046: ZB = zb_and(n2707, n3043);
    let n3047: ZB = zb_and(n2708, n3043);
    let n3048: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3045);
    let n3049: ZN = zn_add(n2692, n3048);
    let n3050: ZB = zn_tile_flag_at(g.cache, g.cart, n3049, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3051: ZB = zb_not(n3050);
    let n3052: ZB = zb_and(n3046, n3051);
    let n3053: ZB = zb_and(n3046, n3050);
    let n3054: ZB = zb_or(n3052, n3053);
    let n3055: ZB = zb_and(n3051, n3054);
    let n3056: ZB = zb_and(n3050, n3054);
    let n3057: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n3049);
    let n3058: ZB = zn_le(zn_splat(P8::from_raw(1572864i32)), n3049);
    let n3059: ZB = zb_and(n3055, n3057);
    let n3060: ZB = zb_and(n3055, n3058);
    let n3061: ZB = zb_or(n3059, n3060);
    let n3062: ZB = zb_and(n2694, n3057);
    let n3063: ZB = zb_not(n3062);
    let n3064: ZB = zb_and(n3061, n3062);
    let n3065: ZB = zb_and(n3061, n3063);
    let n3066: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n3048);
    let n3067: ZN = zn_add(n2692, n3066);
    let n3068: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n3067);
    let n3069: ZB = zb_or(n3064, n3065);
    let n3070: ZB = zb_and(n3062, n3068);
    let n3071: ZB = zb_not(n3070);
    let n3072: ZB = zb_and(n3069, n3070);
    let n3073: ZB = zb_and(n3069, n3071);
    let n3074: ZB = zb_or(n3072, n3073);
    let n3075: ZB = zb_and(n2696, n3070);
    let n3076: ZB = zb_not(n3075);
    let n3077: ZB = zb_and(n3074, n3075);
    let n3078: ZB = zb_and(n3074, n3076);
    let n3079: ZB = zb_or(n3077, n3078);
    let n3080: ZB = zb_or(n3056, n3079);
    let n3081: ZB = zb_or(n3050, n3075);
    let n3082: ZB = zb_not(n3081);
    let n3083: ZB = zb_and(n3080, n3082);
    let n3084: ZB = zb_and(n3080, n3081);
    let n3085: ZN = zn_add(n2692, n3045);
    let n3086: ZB = zb_and(n2709, n3083);
    let n3087: ZB = zb_and(n2710, n3083);
    let n3088: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3085);
    let n3089: ZN = zn_add(n2692, n3088);
    let n3090: ZB = zn_tile_flag_at(g.cache, g.cart, n3089, n172, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3091: ZB = zb_not(n3090);
    let n3092: ZB = zb_and(n3086, n3091);
    let n3093: ZB = zb_and(n3086, n3090);
    let n3094: ZB = zb_or(n3092, n3093);
    let n3095: ZB = zb_and(n3091, n3094);
    let n3096: ZB = zb_and(n3090, n3094);
    let n3097: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n3089);
    let n3098: ZB = zn_le(zn_splat(P8::from_raw(1572864i32)), n3089);
    let n3099: ZB = zb_and(n3095, n3097);
    let n3100: ZB = zb_and(n3095, n3098);
    let n3101: ZB = zb_or(n3099, n3100);
    let n3102: ZB = zb_and(n2694, n3097);
    let n3103: ZB = zb_not(n3102);
    let n3104: ZB = zb_and(n3101, n3102);
    let n3105: ZB = zb_and(n3101, n3103);
    let n3106: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n3088);
    let n3107: ZN = zn_add(n2692, n3106);
    let n3108: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n3107);
    let n3109: ZB = zb_or(n3104, n3105);
    let n3110: ZB = zb_and(n3102, n3108);
    let n3111: ZB = zb_not(n3110);
    let n3112: ZB = zb_and(n3109, n3110);
    let n3113: ZB = zb_and(n3109, n3111);
    let n3114: ZB = zb_or(n3112, n3113);
    let n3115: ZB = zb_and(n2696, n3110);
    let n3116: ZB = zb_not(n3115);
    let n3117: ZB = zb_and(n3114, n3115);
    let n3118: ZB = zb_and(n3114, n3116);
    let n3119: ZB = zb_or(n3117, n3118);
    let n3120: ZB = zb_or(n3096, n3119);
    let n3121: ZB = zb_or(n3090, n3115);
    let n3122: ZB = zb_not(n3121);
    let n3123: ZB = zb_and(n3120, n3122);
    let n3124: ZB = zb_and(n3120, n3121);
    let n3125: ZN = zn_add(n2692, n3085);
    let n3126: ZN = zsel_n(n3121, n3085, n3125);
    let n3127: ZN = zsel_n(n3121, zn_splat(P8::from_raw(0i32)), r_c320);
    let n3128: ZB = zb_or(n3123, n3124);
    let n3129: ZB = zb_or(n2711, n3121);
    let n3130: ZN = zsel_n(n2710, n3085, n3126);
    let n3131: ZN = zsel_n(n2710, r_c320, n3127);
    let n3132: ZB = zb_or(n3087, n3128);
    let n3133: ZB = zb_or(n2710, n3129);
    let n3134: ZN = zsel_n(n3081, n3045, n3130);
    let n3135: ZN = zsel_n(n3081, zn_splat(P8::from_raw(0i32)), n3131);
    let n3136: ZB = zb_or(n3084, n3132);
    let n3137: ZB = zb_or(n3081, n3133);
    let n3138: ZN = zsel_n(n2708, n3045, n3134);
    let n3139: ZN = zsel_n(n2708, r_c320, n3135);
    let n3140: ZB = zb_or(n3047, n3136);
    let n3141: ZB = zb_or(n2708, n3137);
    let n3142: ZN = zsel_n(n3041, n3005, n3138);
    let n3143: ZN = zsel_n(n3041, zn_splat(P8::from_raw(0i32)), n3139);
    let n3144: ZB = zb_or(n3044, n3140);
    let n3145: ZB = zb_or(n3041, n3141);
    let n3146: ZN = zsel_n(n2706, n3005, n3142);
    let n3147: ZN = zsel_n(n2706, r_c320, n3143);
    let n3148: ZB = zb_or(n3007, n3144);
    let n3149: ZB = zb_or(n2706, n3145);
    let n3150: ZN = zsel_n(n3001, n2965, n3146);
    let n3151: ZN = zsel_n(n3001, zn_splat(P8::from_raw(0i32)), n3147);
    let n3152: ZB = zb_or(n3004, n3148);
    let n3153: ZB = zb_or(n3001, n3149);
    let n3154: ZN = zsel_n(n2704, n2965, n3150);
    let n3155: ZN = zsel_n(n2704, r_c320, n3151);
    let n3156: ZB = zb_or(n2967, n3152);
    let n3157: ZB = zb_or(n2704, n3153);
    let n3158: ZN = zsel_n(n2961, n2925, n3154);
    let n3159: ZN = zsel_n(n2961, zn_splat(P8::from_raw(0i32)), n3155);
    let n3160: ZB = zb_or(n2964, n3156);
    let n3161: ZB = zb_or(n2961, n3157);
    let n3162: ZN = zsel_n(n2702, n2925, n3158);
    let n3163: ZN = zsel_n(n2702, r_c320, n3159);
    let n3164: ZB = zb_or(n2927, n3160);
    let n3165: ZB = zb_or(n2702, n3161);
    let n3166: ZN = zsel_n(n2921, n2885, n3162);
    let n3167: ZN = zsel_n(n2921, zn_splat(P8::from_raw(0i32)), n3163);
    let n3168: ZB = zb_or(n2924, n3164);
    let n3169: ZB = zb_or(n2921, n3165);
    let n3170: ZN = zsel_n(n2700, n2885, n3166);
    let n3171: ZN = zsel_n(n2700, r_c320, n3167);
    let n3172: ZB = zb_or(n2887, n3168);
    let n3173: ZB = zb_or(n2700, n3169);
    let n3174: ZN = zsel_n(n2881, n2845, n3170);
    let n3175: ZN = zsel_n(n2881, zn_splat(P8::from_raw(0i32)), n3171);
    let n3176: ZB = zb_or(n2884, n3172);
    let n3177: ZB = zb_or(n2881, n3173);
    let n3178: ZN = zsel_n(n2698, n2845, n3174);
    let n3179: ZN = zsel_n(n2698, r_c320, n3175);
    let n3180: ZB = zb_or(n2847, n3176);
    let n3181: ZB = zb_or(n2698, n3177);
    let n3182: ZN = zsel_n(n2841, r_c272, n3178);
    let n3183: ZN = zsel_n(n2841, zn_splat(P8::from_raw(0i32)), n3179);
    let n3184: ZB = zb_or(n2844, n3180);
    let n3185: ZB = zb_or(n2841, n3181);
    let n3186: ZB = zb_and(n2715, n3184);
    let n3187: ZB = zb_and(n2716, n3184);
    let n3188: ZB = zb_and(n2717, n3187);
    let n3189: ZB = zb_and(n2718, n3187);
    let n3190: ZB = zb_or(n3188, n3189);
    let n3191: ZB = zb_or(n3186, n3190);
    let n3192: ZB = zb_and(n2722, n3191);
    let n3193: ZB = zb_and(n2723, n3191);
    let n3194: ZB = zb_or(n3192, n3193);
    let n3195: ZB = zb_and(n2722, n3194);
    let n3196: ZB = zb_and(n2723, n3194);
    let n3197: ZB = zb_or(n3195, n3196);
    let n3198: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3182);
    let n3199: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3198);
    let n3200: ZB = zn_tile_flag_at(g.cache, g.cart, n3199, n2724, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3201: ZB = zb_not(n3200);
    let n3202: ZB = zb_and(n3197, n3201);
    let n3203: ZB = zb_and(n3197, n3200);
    let n3204: ZB = zb_or(n3202, n3203);
    let n3205: ZB = zb_and(n3201, n3204);
    let n3206: ZB = zb_and(n3200, n3204);
    let n3207: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n3199);
    let n3208: ZB = zn_le(zn_splat(P8::from_raw(1572864i32)), n3199);
    let n3209: ZB = zb_and(n3205, n3207);
    let n3210: ZB = zb_and(n3205, n3208);
    let n3211: ZB = zb_or(n3209, n3210);
    let n3212: ZB = zb_and(n2725, n3207);
    let n3213: ZB = zb_not(n3212);
    let n3214: ZB = zb_and(n3211, n3212);
    let n3215: ZB = zb_and(n3211, n3213);
    let n3216: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n3198);
    let n3217: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3216);
    let n3218: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n3217);
    let n3219: ZB = zb_or(n3214, n3215);
    let n3220: ZB = zb_and(n3212, n3218);
    let n3221: ZB = zb_not(n3220);
    let n3222: ZB = zb_and(n3219, n3220);
    let n3223: ZB = zb_and(n3219, n3221);
    let n3224: ZB = zb_or(n3222, n3223);
    let n3225: ZB = zb_and(n2727, n3220);
    let n3226: ZB = zb_not(n3225);
    let n3227: ZB = zb_and(n3224, n3225);
    let n3228: ZB = zb_and(n3224, n3226);
    let n3229: ZB = zb_or(n3227, n3228);
    let n3230: ZB = zb_or(n3206, n3229);
    let n3231: ZB = zb_or(n3200, n3225);
    let n3232: ZB = zb_not(n3231);
    let n3233: ZB = zb_and(n3230, n3232);
    let n3234: ZB = zb_and(n3230, n3231);
    let n3235: ZB = zb_and(n2729, n3233);
    let n3236: ZB = zb_and(n2730, n3233);
    let n3237: ZB = zb_and(n2722, n3235);
    let n3238: ZB = zb_and(n2723, n3235);
    let n3239: ZB = zb_or(n3237, n3238);
    let n3240: ZB = zb_and(n2722, n3239);
    let n3241: ZB = zb_and(n2723, n3239);
    let n3242: ZB = zb_or(n3240, n3241);
    let n3243: ZB = zn_tile_flag_at(g.cache, g.cart, n3199, n2732, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3244: ZB = zb_not(n3243);
    let n3245: ZB = zb_and(n3242, n3244);
    let n3246: ZB = zb_and(n3242, n3243);
    let n3247: ZB = zb_or(n3245, n3246);
    let n3248: ZB = zb_and(n3244, n3247);
    let n3249: ZB = zb_and(n3243, n3247);
    let n3250: ZB = zb_and(n3207, n3248);
    let n3251: ZB = zb_and(n3208, n3248);
    let n3252: ZB = zb_or(n3250, n3251);
    let n3253: ZB = zb_and(n2733, n3207);
    let n3254: ZB = zb_not(n3253);
    let n3255: ZB = zb_and(n3252, n3253);
    let n3256: ZB = zb_and(n3252, n3254);
    let n3257: ZB = zb_or(n3255, n3256);
    let n3258: ZB = zb_and(n3218, n3253);
    let n3259: ZB = zb_not(n3258);
    let n3260: ZB = zb_and(n3257, n3258);
    let n3261: ZB = zb_and(n3257, n3259);
    let n3262: ZB = zb_or(n3260, n3261);
    let n3263: ZB = zb_and(n2736, n3258);
    let n3264: ZB = zb_not(n3263);
    let n3265: ZB = zb_and(n3262, n3263);
    let n3266: ZB = zb_and(n3262, n3264);
    let n3267: ZB = zb_or(n3265, n3266);
    let n3268: ZB = zb_or(n3249, n3267);
    let n3269: ZB = zb_or(n3243, n3263);
    let n3270: ZB = zb_not(n3269);
    let n3271: ZB = zb_and(n3268, n3270);
    let n3272: ZB = zb_and(n3268, n3269);
    let n3273: ZB = zb_and(n2738, n3271);
    let n3274: ZB = zb_and(n2739, n3271);
    let n3275: ZB = zb_and(n2722, n3273);
    let n3276: ZB = zb_and(n2723, n3273);
    let n3277: ZB = zb_or(n3275, n3276);
    let n3278: ZB = zb_and(n2722, n3277);
    let n3279: ZB = zb_and(n2723, n3277);
    let n3280: ZB = zb_or(n3278, n3279);
    let n3281: ZB = zn_tile_flag_at(g.cache, g.cart, n3199, n2741, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3282: ZB = zb_not(n3281);
    let n3283: ZB = zb_and(n3280, n3282);
    let n3284: ZB = zb_and(n3280, n3281);
    let n3285: ZB = zb_or(n3283, n3284);
    let n3286: ZB = zb_and(n3282, n3285);
    let n3287: ZB = zb_and(n3281, n3285);
    let n3288: ZB = zb_and(n3207, n3286);
    let n3289: ZB = zb_and(n3208, n3286);
    let n3290: ZB = zb_or(n3288, n3289);
    let n3291: ZB = zb_and(n2742, n3207);
    let n3292: ZB = zb_not(n3291);
    let n3293: ZB = zb_and(n3290, n3291);
    let n3294: ZB = zb_and(n3290, n3292);
    let n3295: ZB = zb_or(n3293, n3294);
    let n3296: ZB = zb_and(n3218, n3291);
    let n3297: ZB = zb_not(n3296);
    let n3298: ZB = zb_and(n3295, n3296);
    let n3299: ZB = zb_and(n3295, n3297);
    let n3300: ZB = zb_or(n3298, n3299);
    let n3301: ZB = zb_and(n2745, n3296);
    let n3302: ZB = zb_not(n3301);
    let n3303: ZB = zb_and(n3300, n3301);
    let n3304: ZB = zb_and(n3300, n3302);
    let n3305: ZB = zb_or(n3303, n3304);
    let n3306: ZB = zb_or(n3287, n3305);
    let n3307: ZB = zb_or(n3281, n3301);
    let n3308: ZB = zb_not(n3307);
    let n3309: ZB = zb_and(n3306, n3308);
    let n3310: ZB = zb_and(n3306, n3307);
    let n3311: ZB = zb_and(n2747, n3309);
    let n3312: ZB = zb_and(n2748, n3309);
    let n3313: ZB = zb_and(n2722, n3311);
    let n3314: ZB = zb_and(n2723, n3311);
    let n3315: ZB = zb_or(n3313, n3314);
    let n3316: ZB = zb_and(n2722, n3315);
    let n3317: ZB = zb_and(n2723, n3315);
    let n3318: ZB = zb_or(n3316, n3317);
    let n3319: ZB = zn_tile_flag_at(g.cache, g.cart, n3199, n2750, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3320: ZB = zb_not(n3319);
    let n3321: ZB = zb_and(n3318, n3320);
    let n3322: ZB = zb_and(n3318, n3319);
    let n3323: ZB = zb_or(n3321, n3322);
    let n3324: ZB = zb_and(n3320, n3323);
    let n3325: ZB = zb_and(n3319, n3323);
    let n3326: ZB = zb_and(n3207, n3324);
    let n3327: ZB = zb_and(n3208, n3324);
    let n3328: ZB = zb_or(n3326, n3327);
    let n3329: ZB = zb_and(n2751, n3207);
    let n3330: ZB = zb_not(n3329);
    let n3331: ZB = zb_and(n3328, n3329);
    let n3332: ZB = zb_and(n3328, n3330);
    let n3333: ZB = zb_or(n3331, n3332);
    let n3334: ZB = zb_and(n3218, n3329);
    let n3335: ZB = zb_not(n3334);
    let n3336: ZB = zb_and(n3333, n3334);
    let n3337: ZB = zb_and(n3333, n3335);
    let n3338: ZB = zb_or(n3336, n3337);
    let n3339: ZB = zb_and(n2754, n3334);
    let n3340: ZB = zb_not(n3339);
    let n3341: ZB = zb_and(n3338, n3339);
    let n3342: ZB = zb_and(n3338, n3340);
    let n3343: ZB = zb_or(n3341, n3342);
    let n3344: ZB = zb_or(n3325, n3343);
    let n3345: ZB = zb_or(n3319, n3339);
    let n3346: ZB = zb_not(n3345);
    let n3347: ZB = zb_and(n3344, n3346);
    let n3348: ZB = zb_and(n3344, n3345);
    let n3349: ZB = zb_and(n2756, n3347);
    let n3350: ZB = zb_and(n2757, n3347);
    let n3351: ZB = zb_and(n2722, n3349);
    let n3352: ZB = zb_and(n2723, n3349);
    let n3353: ZB = zb_or(n3351, n3352);
    let n3354: ZB = zb_and(n2722, n3353);
    let n3355: ZB = zb_and(n2723, n3353);
    let n3356: ZB = zb_or(n3354, n3355);
    let n3357: ZB = zn_tile_flag_at(g.cache, g.cart, n3199, n2759, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3358: ZB = zb_not(n3357);
    let n3359: ZB = zb_and(n3356, n3358);
    let n3360: ZB = zb_and(n3356, n3357);
    let n3361: ZB = zb_or(n3359, n3360);
    let n3362: ZB = zb_and(n3358, n3361);
    let n3363: ZB = zb_and(n3357, n3361);
    let n3364: ZB = zb_and(n3207, n3362);
    let n3365: ZB = zb_and(n3208, n3362);
    let n3366: ZB = zb_or(n3364, n3365);
    let n3367: ZB = zb_and(n2760, n3207);
    let n3368: ZB = zb_not(n3367);
    let n3369: ZB = zb_and(n3366, n3367);
    let n3370: ZB = zb_and(n3366, n3368);
    let n3371: ZB = zb_or(n3369, n3370);
    let n3372: ZB = zb_and(n3218, n3367);
    let n3373: ZB = zb_not(n3372);
    let n3374: ZB = zb_and(n3371, n3372);
    let n3375: ZB = zb_and(n3371, n3373);
    let n3376: ZB = zb_or(n3374, n3375);
    let n3377: ZB = zb_and(n2763, n3372);
    let n3378: ZB = zb_not(n3377);
    let n3379: ZB = zb_and(n3376, n3377);
    let n3380: ZB = zb_and(n3376, n3378);
    let n3381: ZB = zb_or(n3379, n3380);
    let n3382: ZB = zb_or(n3363, n3381);
    let n3383: ZB = zb_or(n3357, n3377);
    let n3384: ZB = zb_not(n3383);
    let n3385: ZB = zb_and(n3382, n3384);
    let n3386: ZB = zb_and(n3382, n3383);
    let n3387: ZB = zb_and(n2765, n3385);
    let n3388: ZB = zb_and(n2766, n3385);
    let n3389: ZB = zb_and(n2722, n3387);
    let n3390: ZB = zb_and(n2723, n3387);
    let n3391: ZB = zb_or(n3389, n3390);
    let n3392: ZB = zb_and(n2722, n3391);
    let n3393: ZB = zb_and(n2723, n3391);
    let n3394: ZB = zb_or(n3392, n3393);
    let n3395: ZB = zn_tile_flag_at(g.cache, g.cart, n3199, n2768, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3396: ZB = zb_not(n3395);
    let n3397: ZB = zb_and(n3394, n3396);
    let n3398: ZB = zb_and(n3394, n3395);
    let n3399: ZB = zb_or(n3397, n3398);
    let n3400: ZB = zb_and(n3396, n3399);
    let n3401: ZB = zb_and(n3395, n3399);
    let n3402: ZB = zb_and(n3207, n3400);
    let n3403: ZB = zb_and(n3208, n3400);
    let n3404: ZB = zb_or(n3402, n3403);
    let n3405: ZB = zb_and(n2769, n3207);
    let n3406: ZB = zb_not(n3405);
    let n3407: ZB = zb_and(n3404, n3405);
    let n3408: ZB = zb_and(n3404, n3406);
    let n3409: ZB = zb_or(n3407, n3408);
    let n3410: ZB = zb_and(n3218, n3405);
    let n3411: ZB = zb_not(n3410);
    let n3412: ZB = zb_and(n3409, n3410);
    let n3413: ZB = zb_and(n3409, n3411);
    let n3414: ZB = zb_or(n3412, n3413);
    let n3415: ZB = zb_and(n2772, n3410);
    let n3416: ZB = zb_not(n3415);
    let n3417: ZB = zb_and(n3414, n3415);
    let n3418: ZB = zb_and(n3414, n3416);
    let n3419: ZB = zb_or(n3417, n3418);
    let n3420: ZB = zb_or(n3401, n3419);
    let n3421: ZB = zb_or(n3395, n3415);
    let n3422: ZB = zb_not(n3421);
    let n3423: ZB = zb_and(n3420, n3422);
    let n3424: ZB = zb_and(n3420, n3421);
    let n3425: ZB = zb_and(n2774, n3423);
    let n3426: ZB = zb_and(n2775, n3423);
    let n3427: ZB = zb_and(n2722, n3425);
    let n3428: ZB = zb_and(n2723, n3425);
    let n3429: ZB = zb_or(n3427, n3428);
    let n3430: ZB = zb_and(n2722, n3429);
    let n3431: ZB = zb_and(n2723, n3429);
    let n3432: ZB = zb_or(n3430, n3431);
    let n3433: ZB = zn_tile_flag_at(g.cache, g.cart, n3199, n2777, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3434: ZB = zb_not(n3433);
    let n3435: ZB = zb_and(n3432, n3434);
    let n3436: ZB = zb_and(n3432, n3433);
    let n3437: ZB = zb_or(n3435, n3436);
    let n3438: ZB = zb_and(n3434, n3437);
    let n3439: ZB = zb_and(n3433, n3437);
    let n3440: ZB = zb_and(n3207, n3438);
    let n3441: ZB = zb_and(n3208, n3438);
    let n3442: ZB = zb_or(n3440, n3441);
    let n3443: ZB = zb_and(n2778, n3207);
    let n3444: ZB = zb_not(n3443);
    let n3445: ZB = zb_and(n3442, n3443);
    let n3446: ZB = zb_and(n3442, n3444);
    let n3447: ZB = zb_or(n3445, n3446);
    let n3448: ZB = zb_and(n3218, n3443);
    let n3449: ZB = zb_not(n3448);
    let n3450: ZB = zb_and(n3447, n3448);
    let n3451: ZB = zb_and(n3447, n3449);
    let n3452: ZB = zb_or(n3450, n3451);
    let n3453: ZB = zb_and(n2781, n3448);
    let n3454: ZB = zb_not(n3453);
    let n3455: ZB = zb_and(n3452, n3453);
    let n3456: ZB = zb_and(n3452, n3454);
    let n3457: ZB = zb_or(n3455, n3456);
    let n3458: ZB = zb_or(n3439, n3457);
    let n3459: ZB = zb_or(n3433, n3453);
    let n3460: ZB = zb_not(n3459);
    let n3461: ZB = zb_and(n3458, n3460);
    let n3462: ZB = zb_and(n3458, n3459);
    let n3463: ZB = zb_and(n2783, n3461);
    let n3464: ZB = zb_and(n2784, n3461);
    let n3465: ZB = zb_and(n2722, n3463);
    let n3466: ZB = zb_and(n2723, n3463);
    let n3467: ZB = zb_or(n3465, n3466);
    let n3468: ZB = zb_and(n2722, n3467);
    let n3469: ZB = zb_and(n2723, n3467);
    let n3470: ZB = zb_or(n3468, n3469);
    let n3471: ZB = zn_tile_flag_at(g.cache, g.cart, n3199, n2786, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3472: ZB = zb_not(n3471);
    let n3473: ZB = zb_and(n3470, n3472);
    let n3474: ZB = zb_and(n3470, n3471);
    let n3475: ZB = zb_or(n3473, n3474);
    let n3476: ZB = zb_and(n3472, n3475);
    let n3477: ZB = zb_and(n3471, n3475);
    let n3478: ZB = zb_and(n3207, n3476);
    let n3479: ZB = zb_and(n3208, n3476);
    let n3480: ZB = zb_or(n3478, n3479);
    let n3481: ZB = zb_and(n2787, n3207);
    let n3482: ZB = zb_not(n3481);
    let n3483: ZB = zb_and(n3480, n3481);
    let n3484: ZB = zb_and(n3480, n3482);
    let n3485: ZB = zb_or(n3483, n3484);
    let n3486: ZB = zb_and(n3218, n3481);
    let n3487: ZB = zb_not(n3486);
    let n3488: ZB = zb_and(n3485, n3486);
    let n3489: ZB = zb_and(n3485, n3487);
    let n3490: ZB = zb_or(n3488, n3489);
    let n3491: ZB = zb_and(n2790, n3486);
    let n3492: ZB = zb_not(n3491);
    let n3493: ZB = zb_and(n3490, n3491);
    let n3494: ZB = zb_and(n3490, n3492);
    let n3495: ZB = zb_or(n3493, n3494);
    let n3496: ZB = zb_or(n3477, n3495);
    let n3497: ZB = zb_or(n3471, n3491);
    let n3498: ZB = zb_not(n3497);
    let n3499: ZB = zb_and(n3496, n3498);
    let n3500: ZB = zb_and(n3496, n3497);
    let n3501: ZB = zb_and(n2792, n3185);
    let n3502: ZN = zsel_n(n3497, n2782, n2791);
    let n3503: ZN = zsel_n(n3497, zn_splat(P8::from_raw(0i32)), r_c321);
    let n3504: ZB = zb_or(n3499, n3500);
    let n3505: ZB = zsel_b(n3497, n3185, n3501);
    let n3506: ZN = zsel_n(n2784, n2782, n3502);
    let n3507: ZN = zsel_n(n2784, r_c321, n3503);
    let n3508: ZB = zb_or(n3464, n3504);
    let n3509: ZB = zsel_b(n2784, n3185, n3505);
    let n3510: ZN = zsel_n(n3459, n2773, n3506);
    let n3511: ZN = zsel_n(n3459, zn_splat(P8::from_raw(0i32)), n3507);
    let n3512: ZB = zb_or(n3462, n3508);
    let n3513: ZB = zsel_b(n3459, n3185, n3509);
    let n3514: ZN = zsel_n(n2775, n2773, n3510);
    let n3515: ZN = zsel_n(n2775, r_c321, n3511);
    let n3516: ZB = zb_or(n3426, n3512);
    let n3517: ZB = zsel_b(n2775, n3185, n3513);
    let n3518: ZN = zsel_n(n3421, n2764, n3514);
    let n3519: ZN = zsel_n(n3421, zn_splat(P8::from_raw(0i32)), n3515);
    let n3520: ZB = zb_or(n3424, n3516);
    let n3521: ZB = zsel_b(n3421, n3185, n3517);
    let n3522: ZN = zsel_n(n2766, n2764, n3518);
    let n3523: ZN = zsel_n(n2766, r_c321, n3519);
    let n3524: ZB = zb_or(n3388, n3520);
    let n3525: ZB = zsel_b(n2766, n3185, n3521);
    let n3526: ZN = zsel_n(n3383, n2755, n3522);
    let n3527: ZN = zsel_n(n3383, zn_splat(P8::from_raw(0i32)), n3523);
    let n3528: ZB = zb_or(n3386, n3524);
    let n3529: ZB = zsel_b(n3383, n3185, n3525);
    let n3530: ZN = zsel_n(n2757, n2755, n3526);
    let n3531: ZN = zsel_n(n2757, r_c321, n3527);
    let n3532: ZB = zb_or(n3350, n3528);
    let n3533: ZB = zsel_b(n2757, n3185, n3529);
    let n3534: ZN = zsel_n(n3345, n2746, n3530);
    let n3535: ZN = zsel_n(n3345, zn_splat(P8::from_raw(0i32)), n3531);
    let n3536: ZB = zb_or(n3348, n3532);
    let n3537: ZB = zsel_b(n3345, n3185, n3533);
    let n3538: ZN = zsel_n(n2748, n2746, n3534);
    let n3539: ZN = zsel_n(n2748, r_c321, n3535);
    let n3540: ZB = zb_or(n3312, n3536);
    let n3541: ZB = zsel_b(n2748, n3185, n3537);
    let n3542: ZN = zsel_n(n3307, n2737, n3538);
    let n3543: ZN = zsel_n(n3307, zn_splat(P8::from_raw(0i32)), n3539);
    let n3544: ZB = zb_or(n3310, n3540);
    let n3545: ZB = zsel_b(n3307, n3185, n3541);
    let n3546: ZN = zsel_n(n2739, n2737, n3542);
    let n3547: ZN = zsel_n(n2739, r_c321, n3543);
    let n3548: ZB = zb_or(n3274, n3544);
    let n3549: ZB = zsel_b(n2739, n3185, n3545);
    let n3550: ZN = zsel_n(n3269, n2728, n3546);
    let n3551: ZN = zsel_n(n3269, zn_splat(P8::from_raw(0i32)), n3547);
    let n3552: ZB = zb_or(n3272, n3548);
    let n3553: ZB = zsel_b(n3269, n3185, n3549);
    let n3554: ZN = zsel_n(n2730, n2728, n3550);
    let n3555: ZN = zsel_n(n2730, r_c321, n3551);
    let n3556: ZB = zb_or(n3236, n3552);
    let n3557: ZB = zsel_b(n2730, n3185, n3553);
    let n3558: ZN = zsel_n(n3231, r_c273, n3554);
    let n3559: ZN = zsel_n(n3231, zn_splat(P8::from_raw(0i32)), n3555);
    let n3560: ZB = zb_or(n3234, n3556);
    let n3561: ZB = zsel_b(n3231, n3185, n3557);
    let n3562: ZN = zsel_n(n2682, n3182, r_c272);
    let n3563: ZN = zsel_n(n2682, n3558, r_c273);
    let n3564: ZN = zsel_n(n2682, n3183, r_c320);
    let n3565: ZN = zsel_n(n2682, n3559, r_c321);
    let n3566: ZB = zb_or(n2803, n3560);
    let n3567: ZB = zb_or(n2683, n3561);
    let n3568: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3562);
    let n3569: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n3563);
    let n3570: ZN = zn_div(n3568, zn_splat(P8::from_raw(524288i32)));
    let n3571: ZN = zn_flr(n3570);
    let n3572: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n3571);
    let n3573: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n3568);
    let n3574: ZN = zn_sub(n3573, zn_splat(P8::from_raw(65536i32)));
    let n3575: ZN = zn_div(n3574, zn_splat(P8::from_raw(524288i32)));
    let n3576: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n3575);
    let n3577: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3572);
    let n3578: ZB = zn_le(n3577, n3576);
    let n3579: ZB = zn_gt(n3577, n3576);
    let n3580: ZB = zb_and(n3566, n3578);
    let n3581: ZB = zb_and(n3566, n3579);
    let n3582: ZN = zn_div(n3569, zn_splat(P8::from_raw(524288i32)));
    let n3583: ZN = zn_flr(n3582);
    let n3584: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n3583);
    let n3585: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n3569);
    let n3586: ZN = zn_sub(n3585, zn_splat(P8::from_raw(65536i32)));
    let n3587: ZN = zn_div(n3586, zn_splat(P8::from_raw(524288i32)));
    let n3588: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n3587);
    let n3589: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3584);
    let n3590: ZB = zn_le(n3589, n3588);
    let n3591: ZB = zn_gt(n3589, n3588);
    let n3592: ZB = zb_and(n3580, n3590);
    let n3593: ZB = zb_and(n3580, n3591);
    let n3594: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3577);
    let n3595: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3589);
    let n3596: ZN = zn_mget(g.cart, n3594, n3595);
    let n3597: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3596);
    let n3598: ZB = zb_not(n3597);
    let n3599: ZB = zb_and(n3592, n3597);
    let n3600: ZB = zb_and(n3592, n3598);
    let n3601: ZN = zn_rem(n3586, zn_splat(P8::from_raw(524288i32)));
    let n3602: ZB = zn_ge(n3601, zn_splat(P8::from_raw(393216i32)));
    let n3603: ZB = zn_lt(n3601, zn_splat(P8::from_raw(393216i32)));
    let n3604: ZB = zb_and(n3599, n3603);
    let n3605: ZB = zb_and(n3599, n3602);
    let n3606: ZN = zn_mul(n3589, zn_splat(P8::from_raw(524288i32)));
    let n3607: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3606);
    let n3608: ZB = zn_eq(n3585, n3607);
    let n3609: ZB = zb_or(n3604, n3605);
    let n3610: ZB = zb_or(n3602, n3608);
    let n3611: ZB = zb_or(n3600, n3609);
    let n3612: ZB = zb_and(n3597, n3610);
    let n3613: ZB = zb_not(n3612);
    let n3614: ZB = zb_and(n3611, n3612);
    let n3615: ZB = zb_and(n3611, n3613);
    let n3616: ZB = zn_ge(n3565, zn_splat(P8::from_raw(0i32)));
    let n3617: ZB = zb_or(n3614, n3615);
    let n3618: ZB = zb_and(n3612, n3616);
    let n3619: ZB = zb_not(n3618);
    let n3620: ZB = zb_and(n3617, n3618);
    let n3621: ZB = zb_and(n3617, n3619);
    let n3622: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3596);
    let n3623: ZB = zb_not(n3622);
    let n3624: ZB = zb_and(n3621, n3622);
    let n3625: ZB = zb_and(n3621, n3623);
    let n3626: ZN = zn_rem(n3569, zn_splat(P8::from_raw(524288i32)));
    let n3627: ZB = zn_le(n3626, zn_splat(P8::from_raw(131072i32)));
    let n3628: ZB = zb_or(n3624, n3625);
    let n3629: ZB = zb_and(n3622, n3627);
    let n3630: ZB = zb_not(n3629);
    let n3631: ZB = zb_and(n3628, n3629);
    let n3632: ZB = zb_and(n3628, n3630);
    let n3633: ZB = zn_le(n3565, zn_splat(P8::from_raw(0i32)));
    let n3634: ZB = zb_or(n3631, n3632);
    let n3635: ZB = zb_and(n3629, n3633);
    let n3636: ZB = zb_not(n3635);
    let n3637: ZB = zb_and(n3634, n3635);
    let n3638: ZB = zb_and(n3634, n3636);
    let n3639: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3596);
    let n3640: ZB = zb_not(n3639);
    let n3641: ZB = zb_and(n3638, n3639);
    let n3642: ZB = zb_and(n3638, n3640);
    let n3643: ZN = zn_rem(n3568, zn_splat(P8::from_raw(524288i32)));
    let n3644: ZB = zn_le(n3643, zn_splat(P8::from_raw(131072i32)));
    let n3645: ZB = zb_or(n3641, n3642);
    let n3646: ZB = zb_and(n3639, n3644);
    let n3647: ZB = zb_not(n3646);
    let n3648: ZB = zb_and(n3645, n3646);
    let n3649: ZB = zb_and(n3645, n3647);
    let n3650: ZB = zn_le(n3564, zn_splat(P8::from_raw(0i32)));
    let n3651: ZB = zb_or(n3648, n3649);
    let n3652: ZB = zb_and(n3646, n3650);
    let n3653: ZB = zb_not(n3652);
    let n3654: ZB = zb_and(n3651, n3652);
    let n3655: ZB = zb_and(n3651, n3653);
    let n3656: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3596);
    let n3657: ZB = zb_not(n3656);
    let n3658: ZB = zb_and(n3655, n3656);
    let n3659: ZB = zb_and(n3655, n3657);
    let n3660: ZN = zn_rem(n3574, zn_splat(P8::from_raw(524288i32)));
    let n3661: ZB = zn_ge(n3660, zn_splat(P8::from_raw(393216i32)));
    let n3662: ZB = zn_lt(n3660, zn_splat(P8::from_raw(393216i32)));
    let n3663: ZB = zb_and(n3658, n3662);
    let n3664: ZB = zb_and(n3658, n3661);
    let n3665: ZN = zn_mul(n3577, zn_splat(P8::from_raw(524288i32)));
    let n3666: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3665);
    let n3667: ZB = zn_eq(n3573, n3666);
    let n3668: ZB = zb_or(n3663, n3664);
    let n3669: ZB = zb_or(n3661, n3667);
    let n3670: ZB = zb_or(n3659, n3668);
    let n3671: ZB = zb_and(n3656, n3669);
    let n3672: ZB = zb_not(n3671);
    let n3673: ZB = zb_and(n3670, n3671);
    let n3674: ZB = zb_and(n3670, n3672);
    let n3675: ZB = zn_ge(n3564, zn_splat(P8::from_raw(0i32)));
    let n3676: ZB = zb_or(n3673, n3674);
    let n3677: ZB = zb_and(n3671, n3675);
    let n3678: ZB = zb_not(n3677);
    let n3679: ZB = zb_and(n3676, n3677);
    let n3680: ZB = zb_and(n3676, n3678);
    let n3681: ZB = zb_or(n3654, n3679);
    let n3682: ZB = zb_or(n3637, n3681);
    let n3683: ZB = zb_or(n3620, n3682);
    let n3684: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3584);
    let n3685: ZB = zn_le(n3684, n3588);
    let n3686: ZB = zn_gt(n3684, n3588);
    let n3687: ZB = zb_and(n3680, n3685);
    let n3688: ZB = zb_and(n3680, n3686);
    let n3689: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3684);
    let n3690: ZN = zn_mget(g.cart, n3594, n3689);
    let n3691: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3690);
    let n3692: ZB = zb_not(n3691);
    let n3693: ZB = zb_and(n3687, n3691);
    let n3694: ZB = zb_and(n3687, n3692);
    let n3695: ZB = zb_and(n3603, n3693);
    let n3696: ZB = zb_and(n3602, n3693);
    let n3697: ZN = zn_mul(n3684, zn_splat(P8::from_raw(524288i32)));
    let n3698: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3697);
    let n3699: ZB = zn_eq(n3585, n3698);
    let n3700: ZB = zb_or(n3695, n3696);
    let n3701: ZB = zb_or(n3602, n3699);
    let n3702: ZB = zb_or(n3694, n3700);
    let n3703: ZB = zb_and(n3691, n3701);
    let n3704: ZB = zb_not(n3703);
    let n3705: ZB = zb_and(n3702, n3703);
    let n3706: ZB = zb_and(n3702, n3704);
    let n3707: ZB = zb_or(n3705, n3706);
    let n3708: ZB = zb_and(n3616, n3703);
    let n3709: ZB = zb_not(n3708);
    let n3710: ZB = zb_and(n3707, n3708);
    let n3711: ZB = zb_and(n3707, n3709);
    let n3712: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3690);
    let n3713: ZB = zb_not(n3712);
    let n3714: ZB = zb_and(n3711, n3712);
    let n3715: ZB = zb_and(n3711, n3713);
    let n3716: ZB = zb_or(n3714, n3715);
    let n3717: ZB = zb_and(n3627, n3712);
    let n3718: ZB = zb_not(n3717);
    let n3719: ZB = zb_and(n3716, n3717);
    let n3720: ZB = zb_and(n3716, n3718);
    let n3721: ZB = zb_or(n3719, n3720);
    let n3722: ZB = zb_and(n3633, n3717);
    let n3723: ZB = zb_not(n3722);
    let n3724: ZB = zb_and(n3721, n3722);
    let n3725: ZB = zb_and(n3721, n3723);
    let n3726: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3690);
    let n3727: ZB = zb_not(n3726);
    let n3728: ZB = zb_and(n3725, n3726);
    let n3729: ZB = zb_and(n3725, n3727);
    let n3730: ZB = zb_or(n3728, n3729);
    let n3731: ZB = zb_and(n3644, n3726);
    let n3732: ZB = zb_not(n3731);
    let n3733: ZB = zb_and(n3730, n3731);
    let n3734: ZB = zb_and(n3730, n3732);
    let n3735: ZB = zb_or(n3733, n3734);
    let n3736: ZB = zb_and(n3650, n3731);
    let n3737: ZB = zb_not(n3736);
    let n3738: ZB = zb_and(n3735, n3736);
    let n3739: ZB = zb_and(n3735, n3737);
    let n3740: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3690);
    let n3741: ZB = zb_not(n3740);
    let n3742: ZB = zb_and(n3739, n3740);
    let n3743: ZB = zb_and(n3739, n3741);
    let n3744: ZB = zb_and(n3662, n3742);
    let n3745: ZB = zb_and(n3661, n3742);
    let n3746: ZB = zb_or(n3744, n3745);
    let n3747: ZB = zb_or(n3743, n3746);
    let n3748: ZB = zb_and(n3669, n3740);
    let n3749: ZB = zb_not(n3748);
    let n3750: ZB = zb_and(n3747, n3748);
    let n3751: ZB = zb_and(n3747, n3749);
    let n3752: ZB = zb_or(n3750, n3751);
    let n3753: ZB = zb_and(n3675, n3748);
    let n3754: ZB = zb_not(n3753);
    let n3755: ZB = zb_and(n3752, n3753);
    let n3756: ZB = zb_and(n3752, n3754);
    let n3757: ZB = zb_or(n3738, n3755);
    let n3758: ZB = zb_or(n3724, n3757);
    let n3759: ZB = zb_or(n3710, n3758);
    let n3760: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n3584);
    let n3761: ZB = zn_le(n3760, n3588);
    let n3762: ZB = zn_gt(n3760, n3588);
    let n3763: ZB = zb_and(n3756, n3761);
    let n3764: ZB = zb_and(n3756, n3762);
    let n3765: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3760);
    let n3766: ZN = zn_mget(g.cart, n3594, n3765);
    let n3767: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3766);
    let n3768: ZB = zb_not(n3767);
    let n3769: ZB = zb_and(n3763, n3767);
    let n3770: ZB = zb_and(n3763, n3768);
    let n3771: ZB = zb_and(n3603, n3769);
    let n3772: ZB = zb_and(n3602, n3769);
    let n3773: ZN = zn_mul(n3760, zn_splat(P8::from_raw(524288i32)));
    let n3774: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3773);
    let n3775: ZB = zn_eq(n3585, n3774);
    let n3776: ZB = zb_or(n3771, n3772);
    let n3777: ZB = zb_or(n3602, n3775);
    let n3778: ZB = zb_or(n3770, n3776);
    let n3779: ZB = zb_and(n3767, n3777);
    let n3780: ZB = zb_not(n3779);
    let n3781: ZB = zb_and(n3778, n3779);
    let n3782: ZB = zb_and(n3778, n3780);
    let n3783: ZB = zb_or(n3781, n3782);
    let n3784: ZB = zb_and(n3616, n3779);
    let n3785: ZB = zb_not(n3784);
    let n3786: ZB = zb_and(n3783, n3784);
    let n3787: ZB = zb_and(n3783, n3785);
    let n3788: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3766);
    let n3789: ZB = zb_not(n3788);
    let n3790: ZB = zb_and(n3787, n3788);
    let n3791: ZB = zb_and(n3787, n3789);
    let n3792: ZB = zb_or(n3790, n3791);
    let n3793: ZB = zb_and(n3627, n3788);
    let n3794: ZB = zb_not(n3793);
    let n3795: ZB = zb_and(n3792, n3793);
    let n3796: ZB = zb_and(n3792, n3794);
    let n3797: ZB = zb_or(n3795, n3796);
    let n3798: ZB = zb_and(n3633, n3793);
    let n3799: ZB = zb_not(n3798);
    let n3800: ZB = zb_and(n3797, n3798);
    let n3801: ZB = zb_and(n3797, n3799);
    let n3802: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3766);
    let n3803: ZB = zb_not(n3802);
    let n3804: ZB = zb_and(n3801, n3802);
    let n3805: ZB = zb_and(n3801, n3803);
    let n3806: ZB = zb_or(n3804, n3805);
    let n3807: ZB = zb_and(n3644, n3802);
    let n3808: ZB = zb_not(n3807);
    let n3809: ZB = zb_and(n3806, n3807);
    let n3810: ZB = zb_and(n3806, n3808);
    let n3811: ZB = zb_or(n3809, n3810);
    let n3812: ZB = zb_and(n3650, n3807);
    let n3813: ZB = zb_not(n3812);
    let n3814: ZB = zb_and(n3811, n3812);
    let n3815: ZB = zb_and(n3811, n3813);
    let n3816: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3766);
    let n3817: ZB = zb_not(n3816);
    let n3818: ZB = zb_and(n3815, n3816);
    let n3819: ZB = zb_and(n3815, n3817);
    let n3820: ZB = zb_and(n3662, n3818);
    let n3821: ZB = zb_and(n3661, n3818);
    let n3822: ZB = zb_or(n3820, n3821);
    let n3823: ZB = zb_or(n3819, n3822);
    let n3824: ZB = zb_and(n3669, n3816);
    let n3825: ZB = zb_not(n3824);
    let n3826: ZB = zb_and(n3823, n3824);
    let n3827: ZB = zb_and(n3823, n3825);
    let n3828: ZB = zb_or(n3826, n3827);
    let n3829: ZB = zb_and(n3675, n3824);
    let n3830: ZB = zb_not(n3829);
    let n3831: ZB = zb_and(n3828, n3829);
    let n3832: ZB = zb_and(n3828, n3830);
    let n3833: ZB = zb_or(n3814, n3831);
    let n3834: ZB = zb_or(n3800, n3833);
    let n3835: ZB = zb_or(n3786, n3834);
    let n3836: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n3584);
    let n3837: ZB = zn_gt(n3836, n3588);
    let n3838: ZB = zb_and(n3567, n3837);
    let n3839: ZB = zb_or(n3764, n3832);
    let n3840: ZB = zsel_b(n3762, n3567, n3838);
    let n3841: ZB = zb_or(n3759, n3835);
    let n3842: ZB = zb_or(n3688, n3839);
    let n3843: ZB = zsel_b(n3686, n3567, n3840);
    let n3844: ZB = zb_or(n3683, n3841);
    let n3845: ZB = zb_or(n3593, n3842);
    let n3846: ZB = zsel_b(n3591, n3567, n3843);
    let n3847: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3572);
    let n3848: ZB = zn_le(n3847, n3576);
    let n3849: ZB = zn_gt(n3847, n3576);
    let n3850: ZB = zb_and(n3845, n3848);
    let n3851: ZB = zb_and(n3845, n3849);
    let n3852: ZB = zb_and(n3590, n3850);
    let n3853: ZB = zb_and(n3591, n3850);
    let n3854: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3847);
    let n3855: ZN = zn_mget(g.cart, n3854, n3595);
    let n3856: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3855);
    let n3857: ZB = zb_not(n3856);
    let n3858: ZB = zb_and(n3852, n3856);
    let n3859: ZB = zb_and(n3852, n3857);
    let n3860: ZB = zb_and(n3603, n3858);
    let n3861: ZB = zb_and(n3602, n3858);
    let n3862: ZB = zb_or(n3860, n3861);
    let n3863: ZB = zb_or(n3859, n3862);
    let n3864: ZB = zb_and(n3610, n3856);
    let n3865: ZB = zb_not(n3864);
    let n3866: ZB = zb_and(n3863, n3864);
    let n3867: ZB = zb_and(n3863, n3865);
    let n3868: ZB = zb_or(n3866, n3867);
    let n3869: ZB = zb_and(n3616, n3864);
    let n3870: ZB = zb_not(n3869);
    let n3871: ZB = zb_and(n3868, n3869);
    let n3872: ZB = zb_and(n3868, n3870);
    let n3873: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3855);
    let n3874: ZB = zb_not(n3873);
    let n3875: ZB = zb_and(n3872, n3873);
    let n3876: ZB = zb_and(n3872, n3874);
    let n3877: ZB = zb_or(n3875, n3876);
    let n3878: ZB = zb_and(n3627, n3873);
    let n3879: ZB = zb_not(n3878);
    let n3880: ZB = zb_and(n3877, n3878);
    let n3881: ZB = zb_and(n3877, n3879);
    let n3882: ZB = zb_or(n3880, n3881);
    let n3883: ZB = zb_and(n3633, n3878);
    let n3884: ZB = zb_not(n3883);
    let n3885: ZB = zb_and(n3882, n3883);
    let n3886: ZB = zb_and(n3882, n3884);
    let n3887: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3855);
    let n3888: ZB = zb_not(n3887);
    let n3889: ZB = zb_and(n3886, n3887);
    let n3890: ZB = zb_and(n3886, n3888);
    let n3891: ZB = zb_or(n3889, n3890);
    let n3892: ZB = zb_and(n3644, n3887);
    let n3893: ZB = zb_not(n3892);
    let n3894: ZB = zb_and(n3891, n3892);
    let n3895: ZB = zb_and(n3891, n3893);
    let n3896: ZB = zb_or(n3894, n3895);
    let n3897: ZB = zb_and(n3650, n3892);
    let n3898: ZB = zb_not(n3897);
    let n3899: ZB = zb_and(n3896, n3897);
    let n3900: ZB = zb_and(n3896, n3898);
    let n3901: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3855);
    let n3902: ZB = zb_not(n3901);
    let n3903: ZB = zb_and(n3900, n3901);
    let n3904: ZB = zb_and(n3900, n3902);
    let n3905: ZB = zb_and(n3662, n3903);
    let n3906: ZB = zb_and(n3661, n3903);
    let n3907: ZN = zn_mul(n3847, zn_splat(P8::from_raw(524288i32)));
    let n3908: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n3907);
    let n3909: ZB = zn_eq(n3573, n3908);
    let n3910: ZB = zb_or(n3905, n3906);
    let n3911: ZB = zb_or(n3661, n3909);
    let n3912: ZB = zb_or(n3904, n3910);
    let n3913: ZB = zb_and(n3901, n3911);
    let n3914: ZB = zb_not(n3913);
    let n3915: ZB = zb_and(n3912, n3913);
    let n3916: ZB = zb_and(n3912, n3914);
    let n3917: ZB = zb_or(n3915, n3916);
    let n3918: ZB = zb_and(n3675, n3913);
    let n3919: ZB = zb_not(n3918);
    let n3920: ZB = zb_and(n3917, n3918);
    let n3921: ZB = zb_and(n3917, n3919);
    let n3922: ZB = zb_or(n3899, n3920);
    let n3923: ZB = zb_or(n3885, n3922);
    let n3924: ZB = zb_or(n3871, n3923);
    let n3925: ZB = zb_and(n3685, n3921);
    let n3926: ZB = zb_and(n3686, n3921);
    let n3927: ZN = zn_mget(g.cart, n3854, n3689);
    let n3928: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3927);
    let n3929: ZB = zb_not(n3928);
    let n3930: ZB = zb_and(n3925, n3928);
    let n3931: ZB = zb_and(n3925, n3929);
    let n3932: ZB = zb_and(n3603, n3930);
    let n3933: ZB = zb_and(n3602, n3930);
    let n3934: ZB = zb_or(n3932, n3933);
    let n3935: ZB = zb_or(n3931, n3934);
    let n3936: ZB = zb_and(n3701, n3928);
    let n3937: ZB = zb_not(n3936);
    let n3938: ZB = zb_and(n3935, n3936);
    let n3939: ZB = zb_and(n3935, n3937);
    let n3940: ZB = zb_or(n3938, n3939);
    let n3941: ZB = zb_and(n3616, n3936);
    let n3942: ZB = zb_not(n3941);
    let n3943: ZB = zb_and(n3940, n3941);
    let n3944: ZB = zb_and(n3940, n3942);
    let n3945: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3927);
    let n3946: ZB = zb_not(n3945);
    let n3947: ZB = zb_and(n3944, n3945);
    let n3948: ZB = zb_and(n3944, n3946);
    let n3949: ZB = zb_or(n3947, n3948);
    let n3950: ZB = zb_and(n3627, n3945);
    let n3951: ZB = zb_not(n3950);
    let n3952: ZB = zb_and(n3949, n3950);
    let n3953: ZB = zb_and(n3949, n3951);
    let n3954: ZB = zb_or(n3952, n3953);
    let n3955: ZB = zb_and(n3633, n3950);
    let n3956: ZB = zb_not(n3955);
    let n3957: ZB = zb_and(n3954, n3955);
    let n3958: ZB = zb_and(n3954, n3956);
    let n3959: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3927);
    let n3960: ZB = zb_not(n3959);
    let n3961: ZB = zb_and(n3958, n3959);
    let n3962: ZB = zb_and(n3958, n3960);
    let n3963: ZB = zb_or(n3961, n3962);
    let n3964: ZB = zb_and(n3644, n3959);
    let n3965: ZB = zb_not(n3964);
    let n3966: ZB = zb_and(n3963, n3964);
    let n3967: ZB = zb_and(n3963, n3965);
    let n3968: ZB = zb_or(n3966, n3967);
    let n3969: ZB = zb_and(n3650, n3964);
    let n3970: ZB = zb_not(n3969);
    let n3971: ZB = zb_and(n3968, n3969);
    let n3972: ZB = zb_and(n3968, n3970);
    let n3973: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3927);
    let n3974: ZB = zb_not(n3973);
    let n3975: ZB = zb_and(n3972, n3973);
    let n3976: ZB = zb_and(n3972, n3974);
    let n3977: ZB = zb_and(n3662, n3975);
    let n3978: ZB = zb_and(n3661, n3975);
    let n3979: ZB = zb_or(n3977, n3978);
    let n3980: ZB = zb_or(n3976, n3979);
    let n3981: ZB = zb_and(n3911, n3973);
    let n3982: ZB = zb_not(n3981);
    let n3983: ZB = zb_and(n3980, n3981);
    let n3984: ZB = zb_and(n3980, n3982);
    let n3985: ZB = zb_or(n3983, n3984);
    let n3986: ZB = zb_and(n3675, n3981);
    let n3987: ZB = zb_not(n3986);
    let n3988: ZB = zb_and(n3985, n3986);
    let n3989: ZB = zb_and(n3985, n3987);
    let n3990: ZB = zb_or(n3971, n3988);
    let n3991: ZB = zb_or(n3957, n3990);
    let n3992: ZB = zb_or(n3943, n3991);
    let n3993: ZB = zb_and(n3761, n3989);
    let n3994: ZB = zb_and(n3762, n3989);
    let n3995: ZN = zn_mget(g.cart, n3854, n3765);
    let n3996: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n3995);
    let n3997: ZB = zb_not(n3996);
    let n3998: ZB = zb_and(n3993, n3996);
    let n3999: ZB = zb_and(n3993, n3997);
    let n4000: ZB = zb_and(n3603, n3998);
    let n4001: ZB = zb_and(n3602, n3998);
    let n4002: ZB = zb_or(n4000, n4001);
    let n4003: ZB = zb_or(n3999, n4002);
    let n4004: ZB = zb_and(n3777, n3996);
    let n4005: ZB = zb_not(n4004);
    let n4006: ZB = zb_and(n4003, n4004);
    let n4007: ZB = zb_and(n4003, n4005);
    let n4008: ZB = zb_or(n4006, n4007);
    let n4009: ZB = zb_and(n3616, n4004);
    let n4010: ZB = zb_not(n4009);
    let n4011: ZB = zb_and(n4008, n4009);
    let n4012: ZB = zb_and(n4008, n4010);
    let n4013: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n3995);
    let n4014: ZB = zb_not(n4013);
    let n4015: ZB = zb_and(n4012, n4013);
    let n4016: ZB = zb_and(n4012, n4014);
    let n4017: ZB = zb_or(n4015, n4016);
    let n4018: ZB = zb_and(n3627, n4013);
    let n4019: ZB = zb_not(n4018);
    let n4020: ZB = zb_and(n4017, n4018);
    let n4021: ZB = zb_and(n4017, n4019);
    let n4022: ZB = zb_or(n4020, n4021);
    let n4023: ZB = zb_and(n3633, n4018);
    let n4024: ZB = zb_not(n4023);
    let n4025: ZB = zb_and(n4022, n4023);
    let n4026: ZB = zb_and(n4022, n4024);
    let n4027: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n3995);
    let n4028: ZB = zb_not(n4027);
    let n4029: ZB = zb_and(n4026, n4027);
    let n4030: ZB = zb_and(n4026, n4028);
    let n4031: ZB = zb_or(n4029, n4030);
    let n4032: ZB = zb_and(n3644, n4027);
    let n4033: ZB = zb_not(n4032);
    let n4034: ZB = zb_and(n4031, n4032);
    let n4035: ZB = zb_and(n4031, n4033);
    let n4036: ZB = zb_or(n4034, n4035);
    let n4037: ZB = zb_and(n3650, n4032);
    let n4038: ZB = zb_not(n4037);
    let n4039: ZB = zb_and(n4036, n4037);
    let n4040: ZB = zb_and(n4036, n4038);
    let n4041: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n3995);
    let n4042: ZB = zb_not(n4041);
    let n4043: ZB = zb_and(n4040, n4041);
    let n4044: ZB = zb_and(n4040, n4042);
    let n4045: ZB = zb_and(n3662, n4043);
    let n4046: ZB = zb_and(n3661, n4043);
    let n4047: ZB = zb_or(n4045, n4046);
    let n4048: ZB = zb_or(n4044, n4047);
    let n4049: ZB = zb_and(n3911, n4041);
    let n4050: ZB = zb_not(n4049);
    let n4051: ZB = zb_and(n4048, n4049);
    let n4052: ZB = zb_and(n4048, n4050);
    let n4053: ZB = zb_or(n4051, n4052);
    let n4054: ZB = zb_and(n3675, n4049);
    let n4055: ZB = zb_not(n4054);
    let n4056: ZB = zb_and(n4053, n4054);
    let n4057: ZB = zb_and(n4053, n4055);
    let n4058: ZB = zb_or(n4039, n4056);
    let n4059: ZB = zb_or(n4025, n4058);
    let n4060: ZB = zb_or(n4011, n4059);
    let n4061: ZB = zb_and(n3837, n3846);
    let n4062: ZB = zb_or(n3994, n4057);
    let n4063: ZB = zsel_b(n3762, n3846, n4061);
    let n4064: ZB = zb_or(n3992, n4060);
    let n4065: ZB = zb_or(n3926, n4062);
    let n4066: ZB = zsel_b(n3686, n3846, n4063);
    let n4067: ZB = zb_or(n3924, n4064);
    let n4068: ZB = zb_or(n3853, n4065);
    let n4069: ZB = zsel_b(n3591, n3846, n4066);
    let n4070: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n3572);
    let n4071: ZB = zn_le(n4070, n3576);
    let n4072: ZB = zn_gt(n4070, n3576);
    let n4073: ZB = zb_and(n4068, n4071);
    let n4074: ZB = zb_and(n4068, n4072);
    let n4075: ZB = zb_and(n3590, n4073);
    let n4076: ZB = zb_and(n3591, n4073);
    let n4077: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n4070);
    let n4078: ZN = zn_mget(g.cart, n4077, n3595);
    let n4079: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n4078);
    let n4080: ZB = zb_not(n4079);
    let n4081: ZB = zb_and(n4075, n4079);
    let n4082: ZB = zb_and(n4075, n4080);
    let n4083: ZB = zb_and(n3603, n4081);
    let n4084: ZB = zb_and(n3602, n4081);
    let n4085: ZB = zb_or(n4083, n4084);
    let n4086: ZB = zb_or(n4082, n4085);
    let n4087: ZB = zb_and(n3610, n4079);
    let n4088: ZB = zb_not(n4087);
    let n4089: ZB = zb_and(n4086, n4087);
    let n4090: ZB = zb_and(n4086, n4088);
    let n4091: ZB = zb_or(n4089, n4090);
    let n4092: ZB = zb_and(n3616, n4087);
    let n4093: ZB = zb_not(n4092);
    let n4094: ZB = zb_and(n4091, n4092);
    let n4095: ZB = zb_and(n4091, n4093);
    let n4096: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n4078);
    let n4097: ZB = zb_not(n4096);
    let n4098: ZB = zb_and(n4095, n4096);
    let n4099: ZB = zb_and(n4095, n4097);
    let n4100: ZB = zb_or(n4098, n4099);
    let n4101: ZB = zb_and(n3627, n4096);
    let n4102: ZB = zb_not(n4101);
    let n4103: ZB = zb_and(n4100, n4101);
    let n4104: ZB = zb_and(n4100, n4102);
    let n4105: ZB = zb_or(n4103, n4104);
    let n4106: ZB = zb_and(n3633, n4101);
    let n4107: ZB = zb_not(n4106);
    let n4108: ZB = zb_and(n4105, n4106);
    let n4109: ZB = zb_and(n4105, n4107);
    let n4110: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n4078);
    let n4111: ZB = zb_not(n4110);
    let n4112: ZB = zb_and(n4109, n4110);
    let n4113: ZB = zb_and(n4109, n4111);
    let n4114: ZB = zb_or(n4112, n4113);
    let n4115: ZB = zb_and(n3644, n4110);
    let n4116: ZB = zb_not(n4115);
    let n4117: ZB = zb_and(n4114, n4115);
    let n4118: ZB = zb_and(n4114, n4116);
    let n4119: ZB = zb_or(n4117, n4118);
    let n4120: ZB = zb_and(n3650, n4115);
    let n4121: ZB = zb_not(n4120);
    let n4122: ZB = zb_and(n4119, n4120);
    let n4123: ZB = zb_and(n4119, n4121);
    let n4124: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n4078);
    let n4125: ZB = zb_not(n4124);
    let n4126: ZB = zb_and(n4123, n4124);
    let n4127: ZB = zb_and(n4123, n4125);
    let n4128: ZB = zb_and(n3662, n4126);
    let n4129: ZB = zb_and(n3661, n4126);
    let n4130: ZN = zn_mul(n4070, zn_splat(P8::from_raw(524288i32)));
    let n4131: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n4130);
    let n4132: ZB = zn_eq(n3573, n4131);
    let n4133: ZB = zb_or(n4128, n4129);
    let n4134: ZB = zb_or(n3661, n4132);
    let n4135: ZB = zb_or(n4127, n4133);
    let n4136: ZB = zb_and(n4124, n4134);
    let n4137: ZB = zb_not(n4136);
    let n4138: ZB = zb_and(n4135, n4136);
    let n4139: ZB = zb_and(n4135, n4137);
    let n4140: ZB = zb_or(n4138, n4139);
    let n4141: ZB = zb_and(n3675, n4136);
    let n4142: ZB = zb_not(n4141);
    let n4143: ZB = zb_and(n4140, n4141);
    let n4144: ZB = zb_and(n4140, n4142);
    let n4145: ZB = zb_or(n4122, n4143);
    let n4146: ZB = zb_or(n4108, n4145);
    let n4147: ZB = zb_or(n4094, n4146);
    let n4148: ZB = zb_and(n3685, n4144);
    let n4149: ZB = zb_and(n3686, n4144);
    let n4150: ZN = zn_mget(g.cart, n4077, n3689);
    let n4151: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n4150);
    let n4152: ZB = zb_not(n4151);
    let n4153: ZB = zb_and(n4148, n4151);
    let n4154: ZB = zb_and(n4148, n4152);
    let n4155: ZB = zb_and(n3603, n4153);
    let n4156: ZB = zb_and(n3602, n4153);
    let n4157: ZB = zb_or(n4155, n4156);
    let n4158: ZB = zb_or(n4154, n4157);
    let n4159: ZB = zb_and(n3701, n4151);
    let n4160: ZB = zb_not(n4159);
    let n4161: ZB = zb_and(n4158, n4159);
    let n4162: ZB = zb_and(n4158, n4160);
    let n4163: ZB = zb_or(n4161, n4162);
    let n4164: ZB = zb_and(n3616, n4159);
    let n4165: ZB = zb_not(n4164);
    let n4166: ZB = zb_and(n4163, n4164);
    let n4167: ZB = zb_and(n4163, n4165);
    let n4168: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n4150);
    let n4169: ZB = zb_not(n4168);
    let n4170: ZB = zb_and(n4167, n4168);
    let n4171: ZB = zb_and(n4167, n4169);
    let n4172: ZB = zb_or(n4170, n4171);
    let n4173: ZB = zb_and(n3627, n4168);
    let n4174: ZB = zb_not(n4173);
    let n4175: ZB = zb_and(n4172, n4173);
    let n4176: ZB = zb_and(n4172, n4174);
    let n4177: ZB = zb_or(n4175, n4176);
    let n4178: ZB = zb_and(n3633, n4173);
    let n4179: ZB = zb_not(n4178);
    let n4180: ZB = zb_and(n4177, n4178);
    let n4181: ZB = zb_and(n4177, n4179);
    let n4182: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n4150);
    let n4183: ZB = zb_not(n4182);
    let n4184: ZB = zb_and(n4181, n4182);
    let n4185: ZB = zb_and(n4181, n4183);
    let n4186: ZB = zb_or(n4184, n4185);
    let n4187: ZB = zb_and(n3644, n4182);
    let n4188: ZB = zb_not(n4187);
    let n4189: ZB = zb_and(n4186, n4187);
    let n4190: ZB = zb_and(n4186, n4188);
    let n4191: ZB = zb_or(n4189, n4190);
    let n4192: ZB = zb_and(n3650, n4187);
    let n4193: ZB = zb_not(n4192);
    let n4194: ZB = zb_and(n4191, n4192);
    let n4195: ZB = zb_and(n4191, n4193);
    let n4196: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n4150);
    let n4197: ZB = zb_not(n4196);
    let n4198: ZB = zb_and(n4195, n4196);
    let n4199: ZB = zb_and(n4195, n4197);
    let n4200: ZB = zb_and(n3662, n4198);
    let n4201: ZB = zb_and(n3661, n4198);
    let n4202: ZB = zb_or(n4200, n4201);
    let n4203: ZB = zb_or(n4199, n4202);
    let n4204: ZB = zb_and(n4134, n4196);
    let n4205: ZB = zb_not(n4204);
    let n4206: ZB = zb_and(n4203, n4204);
    let n4207: ZB = zb_and(n4203, n4205);
    let n4208: ZB = zb_or(n4206, n4207);
    let n4209: ZB = zb_and(n3675, n4204);
    let n4210: ZB = zb_not(n4209);
    let n4211: ZB = zb_and(n4208, n4209);
    let n4212: ZB = zb_and(n4208, n4210);
    let n4213: ZB = zb_or(n4194, n4211);
    let n4214: ZB = zb_or(n4180, n4213);
    let n4215: ZB = zb_or(n4166, n4214);
    let n4216: ZB = zb_and(n3761, n4212);
    let n4217: ZB = zb_and(n3762, n4212);
    let n4218: ZN = zn_mget(g.cart, n4077, n3765);
    let n4219: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n4218);
    let n4220: ZB = zb_not(n4219);
    let n4221: ZB = zb_and(n4216, n4219);
    let n4222: ZB = zb_and(n4216, n4220);
    let n4223: ZB = zb_and(n3603, n4221);
    let n4224: ZB = zb_and(n3602, n4221);
    let n4225: ZB = zb_or(n4223, n4224);
    let n4226: ZB = zb_or(n4222, n4225);
    let n4227: ZB = zb_and(n3777, n4219);
    let n4228: ZB = zb_not(n4227);
    let n4229: ZB = zb_and(n4226, n4227);
    let n4230: ZB = zb_and(n4226, n4228);
    let n4231: ZB = zb_or(n4229, n4230);
    let n4232: ZB = zb_and(n3616, n4227);
    let n4233: ZB = zb_not(n4232);
    let n4234: ZB = zb_and(n4231, n4232);
    let n4235: ZB = zb_and(n4231, n4233);
    let n4236: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n4218);
    let n4237: ZB = zb_not(n4236);
    let n4238: ZB = zb_and(n4235, n4236);
    let n4239: ZB = zb_and(n4235, n4237);
    let n4240: ZB = zb_or(n4238, n4239);
    let n4241: ZB = zb_and(n3627, n4236);
    let n4242: ZB = zb_not(n4241);
    let n4243: ZB = zb_and(n4240, n4241);
    let n4244: ZB = zb_and(n4240, n4242);
    let n4245: ZB = zb_or(n4243, n4244);
    let n4246: ZB = zb_and(n3633, n4241);
    let n4247: ZB = zb_not(n4246);
    let n4248: ZB = zb_and(n4245, n4246);
    let n4249: ZB = zb_and(n4245, n4247);
    let n4250: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n4218);
    let n4251: ZB = zb_not(n4250);
    let n4252: ZB = zb_and(n4249, n4250);
    let n4253: ZB = zb_and(n4249, n4251);
    let n4254: ZB = zb_or(n4252, n4253);
    let n4255: ZB = zb_and(n3644, n4250);
    let n4256: ZB = zb_not(n4255);
    let n4257: ZB = zb_and(n4254, n4255);
    let n4258: ZB = zb_and(n4254, n4256);
    let n4259: ZB = zb_or(n4257, n4258);
    let n4260: ZB = zb_and(n3650, n4255);
    let n4261: ZB = zb_not(n4260);
    let n4262: ZB = zb_and(n4259, n4260);
    let n4263: ZB = zb_and(n4259, n4261);
    let n4264: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n4218);
    let n4265: ZB = zb_not(n4264);
    let n4266: ZB = zb_and(n4263, n4264);
    let n4267: ZB = zb_and(n4263, n4265);
    let n4268: ZB = zb_and(n3662, n4266);
    let n4269: ZB = zb_and(n3661, n4266);
    let n4270: ZB = zb_or(n4268, n4269);
    let n4271: ZB = zb_or(n4267, n4270);
    let n4272: ZB = zb_and(n4134, n4264);
    let n4273: ZB = zb_not(n4272);
    let n4274: ZB = zb_and(n4271, n4272);
    let n4275: ZB = zb_and(n4271, n4273);
    let n4276: ZB = zb_or(n4274, n4275);
    let n4277: ZB = zb_and(n3675, n4272);
    let n4278: ZB = zb_not(n4277);
    let n4279: ZB = zb_and(n4276, n4277);
    let n4280: ZB = zb_and(n4276, n4278);
    let n4281: ZB = zb_or(n4262, n4279);
    let n4282: ZB = zb_or(n4248, n4281);
    let n4283: ZB = zb_or(n4234, n4282);
    let n4284: ZB = zb_and(n3837, n4069);
    let n4285: ZB = zb_or(n4217, n4280);
    let n4286: ZB = zsel_b(n3762, n4069, n4284);
    let n4287: ZB = zb_or(n4215, n4283);
    let n4288: ZB = zb_or(n4149, n4285);
    let n4289: ZB = zsel_b(n3686, n4069, n4286);
    let n4290: ZB = zb_or(n4147, n4287);
    let n4291: ZB = zb_or(n4076, n4288);
    let n4292: ZB = zsel_b(n3591, n4069, n4289);
    let n4293: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n3572);
    let n4294: ZB = zn_gt(n4293, n3576);
    let n4295: ZB = zb_and(n4292, n4294);
    let n4296: ZB = zb_or(n4067, n4290);
    let n4297: ZB = zsel_b(n4067, n3846, n4069);
    let n4298: ZB = zb_or(n4074, n4291);
    let n4299: ZB = zsel_b(n4072, n4069, n4295);
    let n4300: ZB = zb_or(n3844, n4296);
    let n4301: ZB = zsel_b(n3844, n3567, n4297);
    let n4302: ZB = zb_or(n3851, n4298);
    let n4303: ZB = zsel_b(n3849, n3846, n4299);
    let n4304: ZB = zb_or(n3581, n4302);
    let n4305: ZB = zsel_b(n3579, n3567, n4303);
    let n4306: ZB = zn_gt(n3563, zn_splat(P8::from_raw(8388608i32)));
    let n4307: ZB = zn_le(n3563, zn_splat(P8::from_raw(8388608i32)));
    let n4308: ZB = zb_and(n4300, n4306);
    let n4309: ZB = zb_and(n4300, n4307);
    let n4310: ZB = zb_or(n4308, n4309);
    let n4311: ZB = zb_and(n4304, n4306);
    let n4312: ZB = zb_or(n4310, n4311);
    let n4313: ZB = zsel_b(n4310, n4301, n4305);
    let n4314: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3568);
    let n4315: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3569);
    let n4316: ZB = zn_tile_flag_at(g.cache, g.cart, n4314, n4315, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n4317: ZB = zb_not(n4316);
    let n4318: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n4314);
    let n4319: ZB = zn_le(zn_splat(P8::from_raw(1572864i32)), n4314);
    let n4320: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n4315);
    let n4321: ZB = zb_and(n4318, n4320);
    let n4322: ZB = zb_not(n4321);
    let n4323: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3573);
    let n4324: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n4323);
    let n4325: ZB = zb_and(n4321, n4324);
    let n4326: ZB = zb_not(n4325);
    let n4327: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3585);
    let n4328: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n4327);
    let n4329: ZB = zb_and(n4325, n4328);
    let n4330: ZB = zb_not(n4329);
    let n4331: ZB = zb_or(n4316, n4329);
    let n4332: ZB = zb_and(n4312, n4317);
    let n4333: ZB = zb_and(n4312, n4316);
    let n4334: ZB = zb_or(n4332, n4333);
    let n4335: ZB = zb_and(n4317, n4334);
    let n4336: ZB = zb_and(n4316, n4334);
    let n4337: ZB = zb_and(n4318, n4335);
    let n4338: ZB = zb_and(n4319, n4335);
    let n4339: ZB = zb_or(n4337, n4338);
    let n4340: ZB = zb_and(n4321, n4339);
    let n4341: ZB = zb_and(n4322, n4339);
    let n4342: ZB = zb_or(n4340, n4341);
    let n4343: ZB = zb_and(n4325, n4342);
    let n4344: ZB = zb_and(n4326, n4342);
    let n4345: ZB = zb_or(n4343, n4344);
    let n4346: ZB = zb_and(n4329, n4345);
    let n4347: ZB = zb_and(n4330, n4345);
    let n4348: ZB = zb_or(n4346, n4347);
    let n4349: ZB = zb_or(n4336, n4348);
    let n4350: ZB = zb_not(n4331);
    let n4351: ZN = zsel_n(n4331, n244, r_c256);
    let n4352: ZN = zsel_n(n4331, zn_splat(P8::from_raw(393216i32)), n248);
    let n4353: ZB = zb_and(n4331, n4349);
    let n4354: ZB = zb_and(n4349, n4350);
    let n4355: ZB = zb_and(n242, n4353);
    let n4356: ZB = zb_and(n243, n4353);
    let n4357: ZB = zb_or(n4355, n4356);
    let n4358: ZB = zb_and(n245, n4354);
    let n4359: ZB = zb_and(n246, n4354);
    let n4360: ZB = zb_or(n4358, n4359);
    let n4361: ZB = zb_or(n4357, n4360);
    let n4362: ZB = zn_gt(n3564, r_c310);
    let n4363: ZB = zn_le(n3564, r_c310);
    let n4364: ZB = zn_gt(n3565, r_c311);
    let n4365: ZB = zn_le(n3565, r_c311);
    let n4366: ZN = zsel_n(n4350, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n4367: ZN = zn_abs(n3564);
    let n4368: ZB = zn_gt(n4367, zn_splat(P8::from_raw(65536i32)));
    let n4369: ZB = zn_le(n4367, zn_splat(P8::from_raw(65536i32)));
    let n4370: ZB = zn_gt(n3564, zn_splat(P8::from_raw(0i32)));
    let n4371: ZB = zn_lt(n3564, zn_splat(P8::from_raw(0i32)));
    let n4372: ZB = zn_gt(n3564, zn_splat(P8::from_raw(65536i32)));
    let n4373: ZB = zn_le(n3564, zn_splat(P8::from_raw(65536i32)));
    let n4374: ZN = zn_sub(n3564, zn_splat(P8::from_raw(9830i32)));
    let n4375: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n4374);
    let n4376: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n3564);
    let n4377: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n4376);
    let n4378: ZB = zn_gt(n3564, zn_splat(P8::from_raw(-65536i32)));
    let n4379: ZB = zn_le(n3564, zn_splat(P8::from_raw(-65536i32)));
    let n4380: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n4374);
    let n4381: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n4376);
    let n4382: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n4374);
    let n4383: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n4376);
    let n4384: ZN = zsel_n(n4378, n4380, n4381);
    let n4385: ZN = zsel_n(n4370, n4382, n4383);
    let n4386: ZN = zsel_n(n4372, n4375, n4377);
    let n4387: ZN = zsel_n(n4371, n4384, n4385);
    let n4388: ZN = zsel_n(n4370, n4386, n4387);
    let n4389: ZN = zn_sub(n3564, n4366);
    let n4390: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n4389);
    let n4391: ZN = zn_add(n3564, n4366);
    let n4392: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n4391);
    let n4393: ZN = zsel_n(n4370, n4390, n4392);
    let n4394: ZN = zsel_n(n4368, n4388, n4393);
    let n4395: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4394);
    let n4396: ZB = zb_not(n4395);
    let n4397: ZB = zn_lt(n4394, zn_splat(P8::from_raw(0i32)));
    let n4398: ZB = zsel_b(n4396, n4397, r_c312);
    let n4399: ZN = zn_abs(n3565);
    let n4400: ZB = zn_le(n4399, zn_splat(P8::from_raw(9830i32)));
    let n4401: ZB = zn_gt(n4399, zn_splat(P8::from_raw(9830i32)));
    let n4402: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3569);
    let n4403: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n4402);
    let n4404: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3585);
    let n4405: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n4404);
    let n4406: ZB = zn_gt(n3565, zn_splat(P8::from_raw(131072i32)));
    let n4407: ZB = zn_le(n3565, zn_splat(P8::from_raw(131072i32)));
    let n4408: ZB = zn_gt(n4352, zn_splat(P8::from_raw(0i32)));
    let n4409: ZB = zn_le(n4352, zn_splat(P8::from_raw(0i32)));
    let n4410: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n3568);
    let n4411: ZB = zn_tile_flag_at(g.cache, g.cart, n4410, n4402, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n4412: ZB = zb_not(n4411);
    let n4413: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n4410);
    let n4414: ZB = zn_le(zn_splat(P8::from_raw(1572864i32)), n4410);
    let n4415: ZB = zb_and(n4403, n4413);
    let n4416: ZB = zb_not(n4415);
    let n4417: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n3573);
    let n4418: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n4417);
    let n4419: ZB = zb_and(n4415, n4418);
    let n4420: ZB = zb_not(n4419);
    let n4421: ZB = zb_and(n4405, n4419);
    let n4422: ZB = zb_not(n4421);
    let n4423: ZB = zb_or(n4411, n4421);
    let n4424: ZB = zb_not(n4423);
    let n4425: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n3568);
    let n4426: ZB = zn_tile_flag_at(g.cache, g.cart, n4425, n4402, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n4427: ZB = zb_not(n4426);
    let n4428: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n4425);
    let n4429: ZB = zn_le(zn_splat(P8::from_raw(1572864i32)), n4425);
    let n4430: ZB = zb_and(n4403, n4428);
    let n4431: ZB = zb_not(n4430);
    let n4432: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n3573);
    let n4433: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n4432);
    let n4434: ZB = zb_and(n4430, n4433);
    let n4435: ZB = zb_not(n4434);
    let n4436: ZB = zb_and(n4405, n4434);
    let n4437: ZB = zb_not(n4436);
    let n4438: ZB = zb_or(n4426, n4436);
    let n4439: ZB = zb_not(n4438);
    let n4440: ZN = zsel_n(n4438, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n4441: ZN = zsel_n(n4423, zn_splat(P8::from_raw(-65536i32)), n4440);
    let n4442: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4441);
    let n4443: ZB = zb_not(n4442);
    let n4444: ZB = zn_gt(n4351, zn_splat(P8::from_raw(0i32)));
    let n4445: ZB = zn_le(n4351, zn_splat(P8::from_raw(0i32)));
    let n4446: ZB = zb_not(n4398);
    let n4447: ZN = zsel_n(n4398, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n4448: ZB = zn_gt(n4447, zn_splat(P8::from_raw(0i32)));
    let n4449: ZB = zn_le(n4447, zn_splat(P8::from_raw(0i32)));
    let n4450: ZB = zn_lt(n4447, zn_splat(P8::from_raw(0i32)));
    let n4451: ZB = zn_ge(n4447, zn_splat(P8::from_raw(0i32)));
    let n4452: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4447);
    let n4453: ZB = zb_not(n4452);
    let n4454: ZB = zb_and(n2793, n4361);
    let n4455: ZB = zb_and(n2794, n4361);
    let n4456: ZB = zb_and(n4362, n4454);
    let n4457: ZB = zb_and(n4363, n4454);
    let n4458: ZB = zb_or(n4456, n4457);
    let n4459: ZB = zb_and(n4364, n4458);
    let n4460: ZB = zb_and(n4365, n4458);
    let n4461: ZB = zb_or(n4459, n4460);
    let n4462: ZB = zb_and(n4350, n4455);
    let n4463: ZB = zb_and(n4331, n4455);
    let n4464: ZB = zb_or(n4462, n4463);
    let n4465: ZB = zb_and(n4368, n4464);
    let n4466: ZB = zb_and(n4369, n4464);
    let n4467: ZB = zb_and(n4370, n4465);
    let n4468: ZB = zb_and(n3650, n4465);
    let n4469: ZB = zb_and(n4371, n4468);
    let n4470: ZB = zb_and(n3675, n4468);
    let n4471: ZB = zb_and(n4372, n4467);
    let n4472: ZB = zb_and(n4373, n4467);
    let n4473: ZB = zb_and(n4378, n4469);
    let n4474: ZB = zb_and(n4379, n4469);
    let n4475: ZB = zb_and(n3650, n4470);
    let n4476: ZB = zb_or(n4473, n4474);
    let n4477: ZB = zb_or(n4471, n4472);
    let n4478: ZB = zb_or(n4475, n4476);
    let n4479: ZB = zb_or(n4477, n4478);
    let n4480: ZB = zb_and(n4370, n4466);
    let n4481: ZB = zb_and(n3650, n4466);
    let n4482: ZB = zb_or(n4480, n4481);
    let n4483: ZB = zb_or(n4479, n4482);
    let n4484: ZB = zb_and(n4396, n4483);
    let n4485: ZB = zb_and(n4395, n4483);
    let n4486: ZB = zb_or(n4484, n4485);
    let n4487: ZB = zb_and(n4400, n4486);
    let n4488: ZB = zb_and(n4401, n4486);
    let n4489: ZB = zb_or(n4487, n4488);
    let n4490: ZB = zb_and(n4350, n4489);
    let n4491: ZB = zb_and(n4331, n4489);
    let n4492: ZB = zb_and(n4406, n4490);
    let n4493: ZB = zb_and(n4407, n4490);
    let n4494: ZB = zb_or(n4492, n4493);
    let n4495: ZB = zb_or(n4491, n4494);
    let n4496: ZB = zb_and(n4444, n4495);
    let n4497: ZB = zb_and(n4445, n4495);
    let n4498: ZB = zb_or(n4496, n4497);
    let n4499: ZB = zb_or(n4461, n4498);
    let n4500: ZB = zn_lt(n3563, zn_splat(P8::from_raw(-262144i32)));
    let n4501: ZB = zn_ge(n3563, zn_splat(P8::from_raw(-262144i32)));
    let n4502: ZB = zb_and(n4499, n4500);
    let n4503: ZB = zb_and(n4499, n4501);
    let n4504: ZB = zb_or(n4502, n4503);
    let n4507: ZN = zsel_n(n4306, n2398, n2397);
    let n4508: ZN = zsel_n(n4310, n4507, n2397);
    let n4510: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n4389);
    let n4511: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n4391);
    let n4512: ZN = zsel_n(n4378, n4510, n4511);
    let n4513: ZN = zsel_n(n4368, n4388, n4512);
    let n4514: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4513);
    let n4515: ZB = zb_not(n4514);
    let n4516: ZB = zn_lt(n4513, zn_splat(P8::from_raw(0i32)));
    let n4517: ZB = zsel_b(n4515, n4516, r_c312);
    let n4518: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n3568);
    let n4519: ZB = zn_tile_flag_at(g.cache, g.cart, n4518, n4402, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n4520: ZB = zb_not(n4519);
    let n4521: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n4518);
    let n4522: ZB = zn_le(zn_splat(P8::from_raw(1572864i32)), n4518);
    let n4523: ZB = zb_and(n4403, n4521);
    let n4524: ZB = zb_not(n4523);
    let n4525: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n3573);
    let n4526: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n4525);
    let n4527: ZB = zb_and(n4523, n4526);
    let n4528: ZB = zb_not(n4527);
    let n4529: ZB = zb_and(n4405, n4527);
    let n4530: ZB = zb_not(n4529);
    let n4531: ZB = zb_or(n4519, n4529);
    let n4532: ZB = zb_not(n4531);
    let n4533: ZN = zsel_n(n4531, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n4534: ZB = zn_gt(n3565, n4533);
    let n4535: ZB = zn_le(n3565, n4533);
    let n4536: ZB = zb_and(n4378, n4466);
    let n4537: ZB = zb_and(n4379, n4466);
    let n4538: ZB = zb_or(n4536, n4537);
    let n4539: ZB = zb_or(n4479, n4538);
    let n4540: ZB = zb_and(n4515, n4539);
    let n4541: ZB = zb_and(n4514, n4539);
    let n4542: ZB = zb_or(n4540, n4541);
    let n4543: ZB = zb_and(n4400, n4542);
    let n4544: ZB = zb_and(n4401, n4542);
    let n4545: ZB = zb_or(n4543, n4544);
    let n4546: ZB = zb_and(n4520, n4545);
    let n4547: ZB = zb_and(n4519, n4545);
    let n4548: ZB = zb_or(n4546, n4547);
    let n4549: ZB = zb_and(n4520, n4548);
    let n4550: ZB = zb_and(n4519, n4548);
    let n4551: ZB = zb_and(n4521, n4549);
    let n4552: ZB = zb_and(n4522, n4549);
    let n4553: ZB = zb_or(n4551, n4552);
    let n4554: ZB = zb_and(n4523, n4553);
    let n4555: ZB = zb_and(n4524, n4553);
    let n4556: ZB = zb_or(n4554, n4555);
    let n4557: ZB = zb_and(n4527, n4556);
    let n4558: ZB = zb_and(n4528, n4556);
    let n4559: ZB = zb_or(n4557, n4558);
    let n4560: ZB = zb_and(n4529, n4559);
    let n4561: ZB = zb_and(n4530, n4559);
    let n4562: ZB = zb_or(n4560, n4561);
    let n4563: ZB = zb_or(n4550, n4562);
    let n4564: ZB = zb_and(n4531, n4563);
    let n4565: ZB = zb_and(n4532, n4563);
    let n4566: ZB = zb_or(n4564, n4565);
    let n4567: ZB = zb_and(n4531, n4566);
    let n4568: ZB = zb_and(n4532, n4566);
    let n4569: ZB = zb_or(n4567, n4568);
    let n4570: ZB = zb_and(n4350, n4569);
    let n4571: ZB = zb_and(n4331, n4569);
    let n4572: ZB = zb_and(n4534, n4570);
    let n4573: ZB = zb_and(n4535, n4570);
    let n4574: ZB = zb_or(n4572, n4573);
    let n4575: ZB = zb_or(n4571, n4574);
    let n4576: ZB = zb_and(n4444, n4575);
    let n4577: ZB = zb_and(n4445, n4575);
    let n4578: ZB = zb_or(n4576, n4577);
    let n4579: ZB = zb_or(n4461, n4578);
    let n4580: ZB = zb_and(n4500, n4579);
    let n4581: ZB = zb_and(n4501, n4579);
    let n4582: ZB = zb_or(n4580, n4581);
    let n4585: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n4389);
    let n4586: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n4391);
    let n4587: ZN = zsel_n(n4372, n4585, n4586);
    let n4588: ZN = zsel_n(n4368, n4388, n4587);
    let n4589: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4588);
    let n4590: ZB = zb_not(n4589);
    let n4591: ZB = zn_lt(n4588, zn_splat(P8::from_raw(0i32)));
    let n4592: ZB = zsel_b(n4590, n4591, r_c312);
    let n4593: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3568);
    let n4594: ZB = zn_tile_flag_at(g.cache, g.cart, n4593, n4402, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n4595: ZB = zb_not(n4594);
    let n4596: ZB = zn_gt(zn_splat(P8::from_raw(1572864i32)), n4593);
    let n4597: ZB = zn_le(zn_splat(P8::from_raw(1572864i32)), n4593);
    let n4598: ZB = zb_and(n4403, n4596);
    let n4599: ZB = zb_not(n4598);
    let n4600: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3573);
    let n4601: ZB = zn_lt(zn_splat(P8::from_raw(524288i32)), n4600);
    let n4602: ZB = zb_and(n4598, n4601);
    let n4603: ZB = zb_not(n4602);
    let n4604: ZB = zb_and(n4405, n4602);
    let n4605: ZB = zb_not(n4604);
    let n4606: ZB = zb_or(n4594, n4604);
    let n4607: ZB = zb_not(n4606);
    let n4608: ZN = zsel_n(n4606, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n4609: ZB = zn_gt(n3565, n4608);
    let n4610: ZB = zn_le(n3565, n4608);
    let n4611: ZB = zb_and(n4372, n4466);
    let n4612: ZB = zb_and(n4373, n4466);
    let n4613: ZB = zb_or(n4611, n4612);
    let n4614: ZB = zb_or(n4479, n4613);
    let n4615: ZB = zb_and(n4590, n4614);
    let n4616: ZB = zb_and(n4589, n4614);
    let n4617: ZB = zb_or(n4615, n4616);
    let n4618: ZB = zb_and(n4400, n4617);
    let n4619: ZB = zb_and(n4401, n4617);
    let n4620: ZB = zb_or(n4618, n4619);
    let n4621: ZB = zb_and(n4595, n4620);
    let n4622: ZB = zb_and(n4594, n4620);
    let n4623: ZB = zb_or(n4621, n4622);
    let n4624: ZB = zb_and(n4595, n4623);
    let n4625: ZB = zb_and(n4594, n4623);
    let n4626: ZB = zb_and(n4596, n4624);
    let n4627: ZB = zb_and(n4597, n4624);
    let n4628: ZB = zb_or(n4626, n4627);
    let n4629: ZB = zb_and(n4598, n4628);
    let n4630: ZB = zb_and(n4599, n4628);
    let n4631: ZB = zb_or(n4629, n4630);
    let n4632: ZB = zb_and(n4602, n4631);
    let n4633: ZB = zb_and(n4603, n4631);
    let n4634: ZB = zb_or(n4632, n4633);
    let n4635: ZB = zb_and(n4604, n4634);
    let n4636: ZB = zb_and(n4605, n4634);
    let n4637: ZB = zb_or(n4635, n4636);
    let n4638: ZB = zb_or(n4625, n4637);
    let n4639: ZB = zb_and(n4606, n4638);
    let n4640: ZB = zb_and(n4607, n4638);
    let n4641: ZB = zb_or(n4639, n4640);
    let n4642: ZB = zb_and(n4606, n4641);
    let n4643: ZB = zb_and(n4607, n4641);
    let n4644: ZB = zb_or(n4642, n4643);
    let n4645: ZB = zb_and(n4350, n4644);
    let n4646: ZB = zb_and(n4331, n4644);
    let n4647: ZB = zb_and(n4609, n4645);
    let n4648: ZB = zb_and(n4610, n4645);
    let n4649: ZB = zb_or(n4647, n4648);
    let n4650: ZB = zb_or(n4646, n4649);
    let n4651: ZB = zb_and(n4444, n4650);
    let n4652: ZB = zb_and(n4445, n4650);
    let n4653: ZB = zb_or(n4651, n4652);
    let n4654: ZB = zb_or(n4461, n4653);
    let n4655: ZB = zb_and(n4500, n4654);
    let n4656: ZB = zb_and(n4501, n4654);
    let n4657: ZB = zb_or(n4655, n4656);
    let n4660: ZB = zb_and(n240, n4495);
    let n4661: ZB = zb_and(r_c266, n4495);
    let n4662: ZB = zb_and(n4408, n4660);
    let n4663: ZB = zb_and(n4409, n4660);
    let n4664: ZB = zb_and(n4412, n4663);
    let n4665: ZB = zb_and(n4411, n4663);
    let n4666: ZB = zb_or(n4664, n4665);
    let n4667: ZB = zb_and(n4412, n4666);
    let n4668: ZB = zb_and(n4411, n4666);
    let n4669: ZB = zb_and(n4413, n4667);
    let n4670: ZB = zb_and(n4414, n4667);
    let n4671: ZB = zb_or(n4669, n4670);
    let n4672: ZB = zb_and(n4415, n4671);
    let n4673: ZB = zb_and(n4416, n4671);
    let n4674: ZB = zb_or(n4672, n4673);
    let n4675: ZB = zb_and(n4419, n4674);
    let n4676: ZB = zb_and(n4420, n4674);
    let n4677: ZB = zb_or(n4675, n4676);
    let n4678: ZB = zb_and(n4421, n4677);
    let n4679: ZB = zb_and(n4422, n4677);
    let n4680: ZB = zb_or(n4678, n4679);
    let n4681: ZB = zb_or(n4668, n4680);
    let n4682: ZB = zb_and(n4423, n4681);
    let n4683: ZB = zb_and(n4424, n4681);
    let n4684: ZB = zb_and(n4427, n4683);
    let n4685: ZB = zb_and(n4426, n4683);
    let n4686: ZB = zb_or(n4684, n4685);
    let n4687: ZB = zb_and(n4427, n4686);
    let n4688: ZB = zb_and(n4426, n4686);
    let n4689: ZB = zb_and(n4428, n4687);
    let n4690: ZB = zb_and(n4429, n4687);
    let n4691: ZB = zb_or(n4689, n4690);
    let n4692: ZB = zb_and(n4430, n4691);
    let n4693: ZB = zb_and(n4431, n4691);
    let n4694: ZB = zb_or(n4692, n4693);
    let n4695: ZB = zb_and(n4434, n4694);
    let n4696: ZB = zb_and(n4435, n4694);
    let n4697: ZB = zb_or(n4695, n4696);
    let n4698: ZB = zb_and(n4436, n4697);
    let n4699: ZB = zb_and(n4437, n4697);
    let n4700: ZB = zb_or(n4698, n4699);
    let n4701: ZB = zb_or(n4688, n4700);
    let n4702: ZB = zb_and(n4438, n4701);
    let n4703: ZB = zb_and(n4439, n4701);
    let n4704: ZB = zb_or(n4702, n4703);
    let n4705: ZB = zb_or(n4682, n4704);
    let n4706: ZB = zb_and(n4443, n4705);
    let n4707: ZB = zb_and(n4442, n4705);
    let n4708: ZB = zb_or(n4706, n4707);
    let n4709: ZB = zb_or(n4662, n4708);
    let n4710: ZB = zb_or(n4661, n4709);
    let n4711: ZB = zb_and(n4444, n4710);
    let n4712: ZB = zb_and(n4445, n4710);
    let n4713: ZB = zb_or(n4711, n4712);
    let n4714: ZB = zb_or(n4461, n4713);
    let n4715: ZB = zb_and(n4500, n4714);
    let n4716: ZB = zb_and(n4501, n4714);
    let n4717: ZB = zb_or(n4715, n4716);
    let n4720: ZB = zb_and(n240, n4575);
    let n4721: ZB = zb_and(r_c266, n4575);
    let n4722: ZB = zb_and(n4408, n4720);
    let n4723: ZB = zb_and(n4409, n4720);
    let n4724: ZB = zb_and(n4412, n4723);
    let n4725: ZB = zb_and(n4411, n4723);
    let n4726: ZB = zb_or(n4724, n4725);
    let n4727: ZB = zb_and(n4412, n4726);
    let n4728: ZB = zb_and(n4411, n4726);
    let n4729: ZB = zb_and(n4413, n4727);
    let n4730: ZB = zb_and(n4414, n4727);
    let n4731: ZB = zb_or(n4729, n4730);
    let n4732: ZB = zb_and(n4415, n4731);
    let n4733: ZB = zb_and(n4416, n4731);
    let n4734: ZB = zb_or(n4732, n4733);
    let n4735: ZB = zb_and(n4419, n4734);
    let n4736: ZB = zb_and(n4420, n4734);
    let n4737: ZB = zb_or(n4735, n4736);
    let n4738: ZB = zb_and(n4421, n4737);
    let n4739: ZB = zb_and(n4422, n4737);
    let n4740: ZB = zb_or(n4738, n4739);
    let n4741: ZB = zb_or(n4728, n4740);
    let n4742: ZB = zb_and(n4423, n4741);
    let n4743: ZB = zb_and(n4424, n4741);
    let n4744: ZB = zb_and(n4427, n4743);
    let n4745: ZB = zb_and(n4426, n4743);
    let n4746: ZB = zb_or(n4744, n4745);
    let n4747: ZB = zb_and(n4427, n4746);
    let n4748: ZB = zb_and(n4426, n4746);
    let n4749: ZB = zb_and(n4428, n4747);
    let n4750: ZB = zb_and(n4429, n4747);
    let n4751: ZB = zb_or(n4749, n4750);
    let n4752: ZB = zb_and(n4430, n4751);
    let n4753: ZB = zb_and(n4431, n4751);
    let n4754: ZB = zb_or(n4752, n4753);
    let n4755: ZB = zb_and(n4434, n4754);
    let n4756: ZB = zb_and(n4435, n4754);
    let n4757: ZB = zb_or(n4755, n4756);
    let n4758: ZB = zb_and(n4436, n4757);
    let n4759: ZB = zb_and(n4437, n4757);
    let n4760: ZB = zb_or(n4758, n4759);
    let n4761: ZB = zb_or(n4748, n4760);
    let n4762: ZB = zb_and(n4438, n4761);
    let n4763: ZB = zb_and(n4439, n4761);
    let n4764: ZB = zb_or(n4762, n4763);
    let n4765: ZB = zb_or(n4742, n4764);
    let n4766: ZB = zb_and(n4443, n4765);
    let n4767: ZB = zb_and(n4442, n4765);
    let n4768: ZB = zb_or(n4766, n4767);
    let n4769: ZB = zb_or(n4722, n4768);
    let n4770: ZB = zb_or(n4721, n4769);
    let n4771: ZB = zb_and(n4444, n4770);
    let n4772: ZB = zb_and(n4445, n4770);
    let n4773: ZB = zb_or(n4771, n4772);
    let n4774: ZB = zb_or(n4461, n4773);
    let n4775: ZB = zb_and(n4500, n4774);
    let n4776: ZB = zb_and(n4501, n4774);
    let n4777: ZB = zb_or(n4775, n4776);
    let n4780: ZB = zb_and(n240, n4650);
    let n4781: ZB = zb_and(r_c266, n4650);
    let n4782: ZB = zb_and(n4408, n4780);
    let n4783: ZB = zb_and(n4409, n4780);
    let n4784: ZB = zb_and(n4412, n4783);
    let n4785: ZB = zb_and(n4411, n4783);
    let n4786: ZB = zb_or(n4784, n4785);
    let n4787: ZB = zb_and(n4412, n4786);
    let n4788: ZB = zb_and(n4411, n4786);
    let n4789: ZB = zb_and(n4413, n4787);
    let n4790: ZB = zb_and(n4414, n4787);
    let n4791: ZB = zb_or(n4789, n4790);
    let n4792: ZB = zb_and(n4415, n4791);
    let n4793: ZB = zb_and(n4416, n4791);
    let n4794: ZB = zb_or(n4792, n4793);
    let n4795: ZB = zb_and(n4419, n4794);
    let n4796: ZB = zb_and(n4420, n4794);
    let n4797: ZB = zb_or(n4795, n4796);
    let n4798: ZB = zb_and(n4421, n4797);
    let n4799: ZB = zb_and(n4422, n4797);
    let n4800: ZB = zb_or(n4798, n4799);
    let n4801: ZB = zb_or(n4788, n4800);
    let n4802: ZB = zb_and(n4423, n4801);
    let n4803: ZB = zb_and(n4424, n4801);
    let n4804: ZB = zb_and(n4427, n4803);
    let n4805: ZB = zb_and(n4426, n4803);
    let n4806: ZB = zb_or(n4804, n4805);
    let n4807: ZB = zb_and(n4427, n4806);
    let n4808: ZB = zb_and(n4426, n4806);
    let n4809: ZB = zb_and(n4428, n4807);
    let n4810: ZB = zb_and(n4429, n4807);
    let n4811: ZB = zb_or(n4809, n4810);
    let n4812: ZB = zb_and(n4430, n4811);
    let n4813: ZB = zb_and(n4431, n4811);
    let n4814: ZB = zb_or(n4812, n4813);
    let n4815: ZB = zb_and(n4434, n4814);
    let n4816: ZB = zb_and(n4435, n4814);
    let n4817: ZB = zb_or(n4815, n4816);
    let n4818: ZB = zb_and(n4436, n4817);
    let n4819: ZB = zb_and(n4437, n4817);
    let n4820: ZB = zb_or(n4818, n4819);
    let n4821: ZB = zb_or(n4808, n4820);
    let n4822: ZB = zb_and(n4438, n4821);
    let n4823: ZB = zb_and(n4439, n4821);
    let n4824: ZB = zb_or(n4822, n4823);
    let n4825: ZB = zb_or(n4802, n4824);
    let n4826: ZB = zb_and(n4443, n4825);
    let n4827: ZB = zb_and(n4442, n4825);
    let n4828: ZB = zb_or(n4826, n4827);
    let n4829: ZB = zb_or(n4782, n4828);
    let n4830: ZB = zb_or(n4781, n4829);
    let n4831: ZB = zb_and(n4444, n4830);
    let n4832: ZB = zb_and(n4445, n4830);
    let n4833: ZB = zb_or(n4831, n4832);
    let n4834: ZB = zb_or(n4461, n4833);
    let n4835: ZB = zb_and(n4500, n4834);
    let n4836: ZB = zb_and(n4501, n4834);
    let n4837: ZB = zb_or(n4835, n4836);
    let n4840: ZB = zb_and(n241, n4444);
    let n4841: ZB = zb_not(n4840);
    let n4842: ZN = zsel_n(n4840, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4843: ZB = zb_or(r_c41, n4840);
    let n4844: ZN = zsel_n(n2793, r_c20, n4842);
    let n4845: ZB = zsel_b(n2793, r_c41, n4843);
    let n4846: ZB = zb_and(n4498, n4840);
    let n4847: ZB = zb_and(n4498, n4841);
    let n4848: ZB = zb_and(n4398, n4846);
    let n4849: ZB = zb_and(n4446, n4846);
    let n4850: ZB = zb_or(n4848, n4849);
    let n4851: ZB = zb_and(n4448, n4850);
    let n4852: ZB = zb_and(n4449, n4850);
    let n4853: ZB = zb_and(n4450, n4852);
    let n4854: ZB = zb_and(n4451, n4852);
    let n4855: ZB = zb_or(n4853, n4854);
    let n4856: ZB = zb_or(n4851, n4855);
    let n4857: ZB = zb_and(n4453, n4856);
    let n4858: ZB = zb_and(n4452, n4856);
    let n4859: ZB = zb_or(n4857, n4858);
    let n4860: ZB = zb_or(n4847, n4859);
    let n4861: ZB = zb_or(n4461, n4860);
    let n4862: ZB = zb_and(n4500, n4861);
    let n4863: ZB = zb_and(n4501, n4861);
    let n4864: ZB = zb_or(n4862, n4863);
    let n4865: ZB = zb_and(n4501, n4864);
    let n4866: ZB = zn_gt(n4844, zn_splat(P8::from_raw(0i32)));
    let n4867: ZB = zn_le(n4844, zn_splat(P8::from_raw(0i32)));
    let n4868: ZB = zb_and(n4865, n4866);
    let n4869: ZB = zb_and(n4865, n4867);
    let n4870: ZB = zb_or(n4868, n4869);
    let n4871: ZB = zb_and(n4578, n4840);
    let n4872: ZB = zb_and(n4578, n4841);
    let n4873: ZB = zb_or(n4871, n4872);
    let n4874: ZB = zb_or(n4461, n4873);
    let n4875: ZB = zb_and(n4500, n4874);
    let n4876: ZB = zb_and(n4501, n4874);
    let n4877: ZB = zb_or(n4875, n4876);
    let n4878: ZB = zb_and(n4501, n4877);
    let n4879: ZB = zb_and(n4866, n4878);
    let n4880: ZB = zb_and(n4867, n4878);
    let n4881: ZB = zb_or(n4879, n4880);
    let n4882: ZB = zb_and(n4653, n4840);
    let n4883: ZB = zb_and(n4653, n4841);
    let n4884: ZB = zb_or(n4882, n4883);
    let n4885: ZB = zb_or(n4461, n4884);
    let n4886: ZB = zb_and(n4500, n4885);
    let n4887: ZB = zb_and(n4501, n4885);
    let n4888: ZB = zb_or(n4886, n4887);
    let n4889: ZB = zb_and(n4501, n4888);
    let n4890: ZB = zb_and(n4866, n4889);
    let n4891: ZB = zb_and(n4867, n4889);
    let n4892: ZB = zb_or(n4890, n4891);
    let n4893: ZB = zb_or(n4846, n4847);
    let n4894: ZB = zb_or(n4461, n4893);
    let n4895: ZB = zb_and(n4500, n4894);
    let n4896: ZB = zb_and(n4501, n4894);
    let n4897: ZB = zb_or(n4895, n4896);
    let n4898: ZB = zb_and(n4501, n4897);
    let n4899: ZB = zb_and(n4866, n4898);
    let n4900: ZB = zb_and(n4867, n4898);
    let n4901: ZB = zb_or(n4899, n4900);
    let n4902: ZB = zb_and(n4713, n4840);
    let n4903: ZB = zb_and(n4713, n4841);
    let n4904: ZB = zb_and(n4398, n4902);
    let n4905: ZB = zb_and(n4446, n4902);
    let n4906: ZB = zb_or(n4904, n4905);
    let n4907: ZB = zb_and(n4448, n4906);
    let n4908: ZB = zb_and(n4449, n4906);
    let n4909: ZB = zb_and(n4450, n4908);
    let n4910: ZB = zb_and(n4451, n4908);
    let n4911: ZB = zb_or(n4909, n4910);
    let n4912: ZB = zb_or(n4907, n4911);
    let n4913: ZB = zb_and(n4453, n4912);
    let n4914: ZB = zb_and(n4452, n4912);
    let n4915: ZB = zb_or(n4913, n4914);
    let n4916: ZB = zb_or(n4903, n4915);
    let n4917: ZB = zb_or(n4461, n4916);
    let n4918: ZB = zb_and(n4500, n4917);
    let n4919: ZB = zb_and(n4501, n4917);
    let n4920: ZB = zb_or(n4918, n4919);
    let n4921: ZB = zb_and(n4501, n4920);
    let n4922: ZB = zb_and(n4866, n4921);
    let n4923: ZB = zb_and(n4867, n4921);
    let n4924: ZB = zb_or(n4922, n4923);
    let n4925: ZB = zb_and(n4773, n4840);
    let n4926: ZB = zb_and(n4773, n4841);
    let n4927: ZB = zb_or(n4925, n4926);
    let n4928: ZB = zb_or(n4461, n4927);
    let n4929: ZB = zb_and(n4500, n4928);
    let n4930: ZB = zb_and(n4501, n4928);
    let n4931: ZB = zb_or(n4929, n4930);
    let n4932: ZB = zb_and(n4501, n4931);
    let n4933: ZB = zb_and(n4866, n4932);
    let n4934: ZB = zb_and(n4867, n4932);
    let n4935: ZB = zb_or(n4933, n4934);
    let n4936: ZB = zb_and(n4833, n4840);
    let n4937: ZB = zb_and(n4833, n4841);
    let n4938: ZB = zb_or(n4936, n4937);
    let n4939: ZB = zb_or(n4461, n4938);
    let n4940: ZB = zb_and(n4500, n4939);
    let n4941: ZB = zb_and(n4501, n4939);
    let n4942: ZB = zb_or(n4940, n4941);
    let n4943: ZB = zb_and(n4501, n4942);
    let n4944: ZB = zb_and(n4866, n4943);
    let n4945: ZB = zb_and(n4867, n4943);
    let n4946: ZB = zb_or(n4944, n4945);
    let n4947: ZB = zb_or(n4902, n4903);
    let n4948: ZB = zb_or(n4461, n4947);
    let n4949: ZB = zb_and(n4500, n4948);
    let n4950: ZB = zb_and(n4501, n4948);
    let n4951: ZB = zb_or(n4949, n4950);
    let n4952: ZB = zb_and(n4501, n4951);
    let n4953: ZB = zb_and(n4866, n4952);
    let n4954: ZB = zb_and(n4867, n4952);
    let n4955: ZB = zb_or(n4953, n4954);
    let n4958: ZB = zb_and(n1467, n1471);
    let n4959: ZB = zb_and(n1467, n2394);
    let n4960: ZB = zb_not(n4958);
    let n4961: ZB = zb_or(n4958, n4959);
    let n4962: ZB = zsel_b(n4958, n1337, n2342);
    let n4963: ZB = zb_and(n4304, n4307);
    let n4964: ZB = zb_and(n4317, n4963);
    let n4965: ZB = zb_and(n4316, n4963);
    let n4966: ZB = zb_or(n4964, n4965);
    let n4967: ZB = zb_and(n4317, n4966);
    let n4968: ZB = zb_and(n4316, n4966);
    let n4969: ZB = zb_and(n4318, n4967);
    let n4970: ZB = zb_and(n4319, n4967);
    let n4971: ZB = zb_or(n4969, n4970);
    let n4972: ZB = zb_and(n4321, n4971);
    let n4973: ZB = zb_and(n4322, n4971);
    let n4974: ZB = zb_or(n4972, n4973);
    let n4975: ZB = zb_and(n4325, n4974);
    let n4976: ZB = zb_and(n4326, n4974);
    let n4977: ZB = zb_or(n4975, n4976);
    let n4978: ZB = zb_and(n4329, n4977);
    let n4979: ZB = zb_and(n4330, n4977);
    let n4980: ZB = zb_or(n4978, n4979);
    let n4981: ZB = zb_or(n4968, n4980);
    let n4982: ZB = zb_and(n4331, n4981);
    let n4983: ZB = zb_and(n4350, n4981);
    let n4984: ZB = zb_and(n242, n4982);
    let n4985: ZB = zb_and(n243, n4982);
    let n4986: ZB = zb_or(n4984, n4985);
    let n4987: ZB = zb_and(n245, n4983);
    let n4988: ZB = zb_and(n246, n4983);
    let n4989: ZB = zb_or(n4987, n4988);
    let n4990: ZB = zb_or(n4986, n4989);
    let n4991: ZB = zb_and(n2793, n4990);
    let n4992: ZB = zb_and(n2794, n4990);
    let n4993: ZB = zb_and(n4362, n4991);
    let n4994: ZB = zb_and(n4363, n4991);
    let n4995: ZB = zb_or(n4993, n4994);
    let n4996: ZB = zb_and(n4364, n4995);
    let n4997: ZB = zb_and(n4365, n4995);
    let n4998: ZB = zb_or(n4996, n4997);
    let n4999: ZB = zb_and(n4350, n4992);
    let n5000: ZB = zb_and(n4331, n4992);
    let n5001: ZB = zb_or(n4999, n5000);
    let n5002: ZB = zb_and(n4368, n5001);
    let n5003: ZB = zb_and(n4369, n5001);
    let n5004: ZB = zb_and(n4370, n5002);
    let n5005: ZB = zb_and(n3650, n5002);
    let n5006: ZB = zb_and(n4371, n5005);
    let n5007: ZB = zb_and(n3675, n5005);
    let n5008: ZB = zb_and(n4372, n5004);
    let n5009: ZB = zb_and(n4373, n5004);
    let n5010: ZB = zb_and(n4378, n5006);
    let n5011: ZB = zb_and(n4379, n5006);
    let n5012: ZB = zb_and(n3650, n5007);
    let n5013: ZB = zb_or(n5010, n5011);
    let n5014: ZB = zb_or(n5008, n5009);
    let n5015: ZB = zb_or(n5012, n5013);
    let n5016: ZB = zb_or(n5014, n5015);
    let n5017: ZB = zb_and(n4370, n5003);
    let n5018: ZB = zb_and(n3650, n5003);
    let n5019: ZB = zb_or(n5017, n5018);
    let n5020: ZB = zb_or(n5016, n5019);
    let n5021: ZB = zb_and(n4396, n5020);
    let n5022: ZB = zb_and(n4395, n5020);
    let n5023: ZB = zb_or(n5021, n5022);
    let n5024: ZB = zb_and(n4400, n5023);
    let n5025: ZB = zb_and(n4401, n5023);
    let n5026: ZB = zb_or(n5024, n5025);
    let n5027: ZB = zb_and(n4350, n5026);
    let n5028: ZB = zb_and(n4331, n5026);
    let n5029: ZB = zb_and(n4406, n5027);
    let n5030: ZB = zb_and(n4407, n5027);
    let n5031: ZB = zb_or(n5029, n5030);
    let n5032: ZB = zb_or(n5028, n5031);
    let n5033: ZB = zb_and(n4444, n5032);
    let n5034: ZB = zb_and(n4445, n5032);
    let n5035: ZB = zb_or(n5033, n5034);
    let n5036: ZB = zb_or(n4998, n5035);
    let n5037: ZB = zb_and(n4500, n5036);
    let n5038: ZB = zb_and(n4501, n5036);
    let n5039: ZB = zb_or(n5037, n5038);
    let n5040: ZB = zb_and(n4500, n5039);
    let n5041: ZB = zb_and(n4500, n4504);
    let n5042: ZB = zb_not(n5040);
    let n5043: ZB = zb_or(n5040, n5041);
    let n5044: ZB = zsel_b(n5040, n4305, n4313);
    let n5045: ZB = zsel_b(n5043, n5042, n4960);
    let n5046: ZN = zsel_n(n5043, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n5047: ZN = zsel_n(n5043, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5048: ZN = zsel_n(n5043, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n5049: ZB = zb_or(n4961, n5043);
    let n5050: ZB = zsel_b(n5043, n5044, n4962);
    let n5052: ZN = zsel_n(n4958, r_c87, n2400);
    let n5053: ZN = zsel_n(n4958, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5054: ZN = zsel_n(n5040, r_c87, n4508);
    let n5055: ZN = zsel_n(n5040, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5056: ZN = zsel_n(n5043, n5054, n5052);
    let n5057: ZN = zsel_n(n5043, n5055, n5053);
    let n5059: ZB = zb_and(n1467, n1572);
    let n5060: ZB = zb_and(n1467, n2432);
    let n5061: ZB = zb_not(n5059);
    let n5062: ZB = zb_or(n5059, n5060);
    let n5063: ZB = zsel_b(n5059, n1337, n2342);
    let n5064: ZB = zb_and(n4378, n5003);
    let n5065: ZB = zb_and(n4379, n5003);
    let n5066: ZB = zb_or(n5064, n5065);
    let n5067: ZB = zb_or(n5016, n5066);
    let n5068: ZB = zb_and(n4515, n5067);
    let n5069: ZB = zb_and(n4514, n5067);
    let n5070: ZB = zb_or(n5068, n5069);
    let n5071: ZB = zb_and(n4400, n5070);
    let n5072: ZB = zb_and(n4401, n5070);
    let n5073: ZB = zb_or(n5071, n5072);
    let n5074: ZB = zb_and(n4520, n5073);
    let n5075: ZB = zb_and(n4519, n5073);
    let n5076: ZB = zb_or(n5074, n5075);
    let n5077: ZB = zb_and(n4520, n5076);
    let n5078: ZB = zb_and(n4519, n5076);
    let n5079: ZB = zb_and(n4521, n5077);
    let n5080: ZB = zb_and(n4522, n5077);
    let n5081: ZB = zb_or(n5079, n5080);
    let n5082: ZB = zb_and(n4523, n5081);
    let n5083: ZB = zb_and(n4524, n5081);
    let n5084: ZB = zb_or(n5082, n5083);
    let n5085: ZB = zb_and(n4527, n5084);
    let n5086: ZB = zb_and(n4528, n5084);
    let n5087: ZB = zb_or(n5085, n5086);
    let n5088: ZB = zb_and(n4529, n5087);
    let n5089: ZB = zb_and(n4530, n5087);
    let n5090: ZB = zb_or(n5088, n5089);
    let n5091: ZB = zb_or(n5078, n5090);
    let n5092: ZB = zb_and(n4531, n5091);
    let n5093: ZB = zb_and(n4532, n5091);
    let n5094: ZB = zb_or(n5092, n5093);
    let n5095: ZB = zb_and(n4531, n5094);
    let n5096: ZB = zb_and(n4532, n5094);
    let n5097: ZB = zb_or(n5095, n5096);
    let n5098: ZB = zb_and(n4350, n5097);
    let n5099: ZB = zb_and(n4331, n5097);
    let n5100: ZB = zb_and(n4534, n5098);
    let n5101: ZB = zb_and(n4535, n5098);
    let n5102: ZB = zb_or(n5100, n5101);
    let n5103: ZB = zb_or(n5099, n5102);
    let n5104: ZB = zb_and(n4444, n5103);
    let n5105: ZB = zb_and(n4445, n5103);
    let n5106: ZB = zb_or(n5104, n5105);
    let n5107: ZB = zb_or(n4998, n5106);
    let n5108: ZB = zb_and(n4500, n5107);
    let n5109: ZB = zb_and(n4501, n5107);
    let n5110: ZB = zb_or(n5108, n5109);
    let n5111: ZB = zb_and(n4500, n5110);
    let n5112: ZB = zb_and(n4500, n4582);
    let n5113: ZB = zb_not(n5111);
    let n5114: ZB = zb_or(n5111, n5112);
    let n5115: ZB = zsel_b(n5111, n4305, n4313);
    let n5116: ZB = zsel_b(n5114, n5113, n5061);
    let n5117: ZN = zsel_n(n5114, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n5118: ZN = zsel_n(n5114, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5119: ZN = zsel_n(n5114, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n5120: ZB = zb_or(n5062, n5114);
    let n5121: ZB = zsel_b(n5114, n5115, n5063);
    let n5123: ZN = zsel_n(n5059, r_c87, n2400);
    let n5124: ZN = zsel_n(n5059, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5125: ZN = zsel_n(n5111, r_c87, n4508);
    let n5126: ZN = zsel_n(n5111, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5127: ZN = zsel_n(n5114, n5125, n5123);
    let n5128: ZN = zsel_n(n5114, n5126, n5124);
    let n5130: ZB = zb_and(n1467, n1641);
    let n5131: ZB = zb_and(n1467, n2465);
    let n5132: ZB = zb_not(n5130);
    let n5133: ZB = zb_or(n5130, n5131);
    let n5134: ZB = zsel_b(n5130, n1337, n2342);
    let n5135: ZB = zb_and(n4372, n5003);
    let n5136: ZB = zb_and(n4373, n5003);
    let n5137: ZB = zb_or(n5135, n5136);
    let n5138: ZB = zb_or(n5016, n5137);
    let n5139: ZB = zb_and(n4590, n5138);
    let n5140: ZB = zb_and(n4589, n5138);
    let n5141: ZB = zb_or(n5139, n5140);
    let n5142: ZB = zb_and(n4400, n5141);
    let n5143: ZB = zb_and(n4401, n5141);
    let n5144: ZB = zb_or(n5142, n5143);
    let n5145: ZB = zb_and(n4595, n5144);
    let n5146: ZB = zb_and(n4594, n5144);
    let n5147: ZB = zb_or(n5145, n5146);
    let n5148: ZB = zb_and(n4595, n5147);
    let n5149: ZB = zb_and(n4594, n5147);
    let n5150: ZB = zb_and(n4596, n5148);
    let n5151: ZB = zb_and(n4597, n5148);
    let n5152: ZB = zb_or(n5150, n5151);
    let n5153: ZB = zb_and(n4598, n5152);
    let n5154: ZB = zb_and(n4599, n5152);
    let n5155: ZB = zb_or(n5153, n5154);
    let n5156: ZB = zb_and(n4602, n5155);
    let n5157: ZB = zb_and(n4603, n5155);
    let n5158: ZB = zb_or(n5156, n5157);
    let n5159: ZB = zb_and(n4604, n5158);
    let n5160: ZB = zb_and(n4605, n5158);
    let n5161: ZB = zb_or(n5159, n5160);
    let n5162: ZB = zb_or(n5149, n5161);
    let n5163: ZB = zb_and(n4606, n5162);
    let n5164: ZB = zb_and(n4607, n5162);
    let n5165: ZB = zb_or(n5163, n5164);
    let n5166: ZB = zb_and(n4606, n5165);
    let n5167: ZB = zb_and(n4607, n5165);
    let n5168: ZB = zb_or(n5166, n5167);
    let n5169: ZB = zb_and(n4350, n5168);
    let n5170: ZB = zb_and(n4331, n5168);
    let n5171: ZB = zb_and(n4609, n5169);
    let n5172: ZB = zb_and(n4610, n5169);
    let n5173: ZB = zb_or(n5171, n5172);
    let n5174: ZB = zb_or(n5170, n5173);
    let n5175: ZB = zb_and(n4444, n5174);
    let n5176: ZB = zb_and(n4445, n5174);
    let n5177: ZB = zb_or(n5175, n5176);
    let n5178: ZB = zb_or(n4998, n5177);
    let n5179: ZB = zb_and(n4500, n5178);
    let n5180: ZB = zb_and(n4501, n5178);
    let n5181: ZB = zb_or(n5179, n5180);
    let n5182: ZB = zb_and(n4500, n5181);
    let n5183: ZB = zb_and(n4500, n4657);
    let n5184: ZB = zb_not(n5182);
    let n5185: ZB = zb_or(n5182, n5183);
    let n5186: ZB = zsel_b(n5182, n4305, n4313);
    let n5187: ZB = zsel_b(n5185, n5184, n5132);
    let n5188: ZN = zsel_n(n5185, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n5189: ZN = zsel_n(n5185, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5190: ZN = zsel_n(n5185, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n5191: ZB = zb_or(n5133, n5185);
    let n5192: ZB = zsel_b(n5185, n5186, n5134);
    let n5194: ZN = zsel_n(n5130, r_c87, n2400);
    let n5195: ZN = zsel_n(n5130, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5196: ZN = zsel_n(n5182, r_c87, n4508);
    let n5197: ZN = zsel_n(n5182, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5198: ZN = zsel_n(n5185, n5196, n5194);
    let n5199: ZN = zsel_n(n5185, n5197, n5195);
    let n5201: ZB = zb_and(n1467, n1697);
    let n5202: ZB = zb_and(n1467, n2500);
    let n5203: ZB = zb_not(n5201);
    let n5204: ZB = zb_or(n5201, n5202);
    let n5205: ZB = zsel_b(n5201, n1337, n2342);
    let n5206: ZB = zb_and(n240, n5032);
    let n5207: ZB = zb_and(r_c266, n5032);
    let n5208: ZB = zb_and(n4408, n5206);
    let n5209: ZB = zb_and(n4409, n5206);
    let n5210: ZB = zb_and(n4412, n5209);
    let n5211: ZB = zb_and(n4411, n5209);
    let n5212: ZB = zb_or(n5210, n5211);
    let n5213: ZB = zb_and(n4412, n5212);
    let n5214: ZB = zb_and(n4411, n5212);
    let n5215: ZB = zb_and(n4413, n5213);
    let n5216: ZB = zb_and(n4414, n5213);
    let n5217: ZB = zb_or(n5215, n5216);
    let n5218: ZB = zb_and(n4415, n5217);
    let n5219: ZB = zb_and(n4416, n5217);
    let n5220: ZB = zb_or(n5218, n5219);
    let n5221: ZB = zb_and(n4419, n5220);
    let n5222: ZB = zb_and(n4420, n5220);
    let n5223: ZB = zb_or(n5221, n5222);
    let n5224: ZB = zb_and(n4421, n5223);
    let n5225: ZB = zb_and(n4422, n5223);
    let n5226: ZB = zb_or(n5224, n5225);
    let n5227: ZB = zb_or(n5214, n5226);
    let n5228: ZB = zb_and(n4423, n5227);
    let n5229: ZB = zb_and(n4424, n5227);
    let n5230: ZB = zb_and(n4427, n5229);
    let n5231: ZB = zb_and(n4426, n5229);
    let n5232: ZB = zb_or(n5230, n5231);
    let n5233: ZB = zb_and(n4427, n5232);
    let n5234: ZB = zb_and(n4426, n5232);
    let n5235: ZB = zb_and(n4428, n5233);
    let n5236: ZB = zb_and(n4429, n5233);
    let n5237: ZB = zb_or(n5235, n5236);
    let n5238: ZB = zb_and(n4430, n5237);
    let n5239: ZB = zb_and(n4431, n5237);
    let n5240: ZB = zb_or(n5238, n5239);
    let n5241: ZB = zb_and(n4434, n5240);
    let n5242: ZB = zb_and(n4435, n5240);
    let n5243: ZB = zb_or(n5241, n5242);
    let n5244: ZB = zb_and(n4436, n5243);
    let n5245: ZB = zb_and(n4437, n5243);
    let n5246: ZB = zb_or(n5244, n5245);
    let n5247: ZB = zb_or(n5234, n5246);
    let n5248: ZB = zb_and(n4438, n5247);
    let n5249: ZB = zb_and(n4439, n5247);
    let n5250: ZB = zb_or(n5248, n5249);
    let n5251: ZB = zb_or(n5228, n5250);
    let n5252: ZB = zb_and(n4443, n5251);
    let n5253: ZB = zb_and(n4442, n5251);
    let n5254: ZB = zb_or(n5252, n5253);
    let n5255: ZB = zb_or(n5208, n5254);
    let n5256: ZB = zb_or(n5207, n5255);
    let n5257: ZB = zb_and(n4444, n5256);
    let n5258: ZB = zb_and(n4445, n5256);
    let n5259: ZB = zb_or(n5257, n5258);
    let n5260: ZB = zb_or(n4998, n5259);
    let n5261: ZB = zb_and(n4500, n5260);
    let n5262: ZB = zb_and(n4501, n5260);
    let n5263: ZB = zb_or(n5261, n5262);
    let n5264: ZB = zb_and(n4500, n5263);
    let n5265: ZB = zb_and(n4500, n4717);
    let n5266: ZB = zb_not(n5264);
    let n5267: ZB = zb_or(n5264, n5265);
    let n5268: ZB = zsel_b(n5264, n4305, n4313);
    let n5269: ZB = zsel_b(n5267, n5266, n5203);
    let n5270: ZN = zsel_n(n5267, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n5271: ZN = zsel_n(n5267, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5272: ZN = zsel_n(n5267, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n5273: ZB = zb_or(n5204, n5267);
    let n5274: ZB = zsel_b(n5267, n5268, n5205);
    let n5276: ZN = zsel_n(n5201, r_c87, n2400);
    let n5277: ZN = zsel_n(n5201, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5278: ZN = zsel_n(n5264, r_c87, n4508);
    let n5279: ZN = zsel_n(n5264, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5280: ZN = zsel_n(n5267, n5278, n5276);
    let n5281: ZN = zsel_n(n5267, n5279, n5277);
    let n5283: ZB = zb_and(n1467, n1752);
    let n5284: ZB = zb_and(n1467, n2535);
    let n5285: ZB = zb_not(n5283);
    let n5286: ZB = zb_or(n5283, n5284);
    let n5287: ZB = zsel_b(n5283, n1337, n2342);
    let n5288: ZB = zb_and(n240, n5103);
    let n5289: ZB = zb_and(r_c266, n5103);
    let n5290: ZB = zb_and(n4408, n5288);
    let n5291: ZB = zb_and(n4409, n5288);
    let n5292: ZB = zb_and(n4412, n5291);
    let n5293: ZB = zb_and(n4411, n5291);
    let n5294: ZB = zb_or(n5292, n5293);
    let n5295: ZB = zb_and(n4412, n5294);
    let n5296: ZB = zb_and(n4411, n5294);
    let n5297: ZB = zb_and(n4413, n5295);
    let n5298: ZB = zb_and(n4414, n5295);
    let n5299: ZB = zb_or(n5297, n5298);
    let n5300: ZB = zb_and(n4415, n5299);
    let n5301: ZB = zb_and(n4416, n5299);
    let n5302: ZB = zb_or(n5300, n5301);
    let n5303: ZB = zb_and(n4419, n5302);
    let n5304: ZB = zb_and(n4420, n5302);
    let n5305: ZB = zb_or(n5303, n5304);
    let n5306: ZB = zb_and(n4421, n5305);
    let n5307: ZB = zb_and(n4422, n5305);
    let n5308: ZB = zb_or(n5306, n5307);
    let n5309: ZB = zb_or(n5296, n5308);
    let n5310: ZB = zb_and(n4423, n5309);
    let n5311: ZB = zb_and(n4424, n5309);
    let n5312: ZB = zb_and(n4427, n5311);
    let n5313: ZB = zb_and(n4426, n5311);
    let n5314: ZB = zb_or(n5312, n5313);
    let n5315: ZB = zb_and(n4427, n5314);
    let n5316: ZB = zb_and(n4426, n5314);
    let n5317: ZB = zb_and(n4428, n5315);
    let n5318: ZB = zb_and(n4429, n5315);
    let n5319: ZB = zb_or(n5317, n5318);
    let n5320: ZB = zb_and(n4430, n5319);
    let n5321: ZB = zb_and(n4431, n5319);
    let n5322: ZB = zb_or(n5320, n5321);
    let n5323: ZB = zb_and(n4434, n5322);
    let n5324: ZB = zb_and(n4435, n5322);
    let n5325: ZB = zb_or(n5323, n5324);
    let n5326: ZB = zb_and(n4436, n5325);
    let n5327: ZB = zb_and(n4437, n5325);
    let n5328: ZB = zb_or(n5326, n5327);
    let n5329: ZB = zb_or(n5316, n5328);
    let n5330: ZB = zb_and(n4438, n5329);
    let n5331: ZB = zb_and(n4439, n5329);
    let n5332: ZB = zb_or(n5330, n5331);
    let n5333: ZB = zb_or(n5310, n5332);
    let n5334: ZB = zb_and(n4443, n5333);
    let n5335: ZB = zb_and(n4442, n5333);
    let n5336: ZB = zb_or(n5334, n5335);
    let n5337: ZB = zb_or(n5290, n5336);
    let n5338: ZB = zb_or(n5289, n5337);
    let n5339: ZB = zb_and(n4444, n5338);
    let n5340: ZB = zb_and(n4445, n5338);
    let n5341: ZB = zb_or(n5339, n5340);
    let n5342: ZB = zb_or(n4998, n5341);
    let n5343: ZB = zb_and(n4500, n5342);
    let n5344: ZB = zb_and(n4501, n5342);
    let n5345: ZB = zb_or(n5343, n5344);
    let n5346: ZB = zb_and(n4500, n5345);
    let n5347: ZB = zb_and(n4500, n4777);
    let n5348: ZB = zb_not(n5346);
    let n5349: ZB = zb_or(n5346, n5347);
    let n5350: ZB = zsel_b(n5346, n4305, n4313);
    let n5351: ZB = zsel_b(n5349, n5348, n5285);
    let n5352: ZN = zsel_n(n5349, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n5353: ZN = zsel_n(n5349, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5354: ZN = zsel_n(n5349, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n5355: ZB = zb_or(n5286, n5349);
    let n5356: ZB = zsel_b(n5349, n5350, n5287);
    let n5358: ZN = zsel_n(n5283, r_c87, n2400);
    let n5359: ZN = zsel_n(n5283, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5360: ZN = zsel_n(n5346, r_c87, n4508);
    let n5361: ZN = zsel_n(n5346, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5362: ZN = zsel_n(n5349, n5360, n5358);
    let n5363: ZN = zsel_n(n5349, n5361, n5359);
    let n5365: ZB = zb_and(n1467, n1807);
    let n5366: ZB = zb_and(n1467, n2570);
    let n5367: ZB = zb_not(n5365);
    let n5368: ZB = zb_or(n5365, n5366);
    let n5369: ZB = zsel_b(n5365, n1337, n2342);
    let n5370: ZB = zb_and(n240, n5174);
    let n5371: ZB = zb_and(r_c266, n5174);
    let n5372: ZB = zb_and(n4408, n5370);
    let n5373: ZB = zb_and(n4409, n5370);
    let n5374: ZB = zb_and(n4412, n5373);
    let n5375: ZB = zb_and(n4411, n5373);
    let n5376: ZB = zb_or(n5374, n5375);
    let n5377: ZB = zb_and(n4412, n5376);
    let n5378: ZB = zb_and(n4411, n5376);
    let n5379: ZB = zb_and(n4413, n5377);
    let n5380: ZB = zb_and(n4414, n5377);
    let n5381: ZB = zb_or(n5379, n5380);
    let n5382: ZB = zb_and(n4415, n5381);
    let n5383: ZB = zb_and(n4416, n5381);
    let n5384: ZB = zb_or(n5382, n5383);
    let n5385: ZB = zb_and(n4419, n5384);
    let n5386: ZB = zb_and(n4420, n5384);
    let n5387: ZB = zb_or(n5385, n5386);
    let n5388: ZB = zb_and(n4421, n5387);
    let n5389: ZB = zb_and(n4422, n5387);
    let n5390: ZB = zb_or(n5388, n5389);
    let n5391: ZB = zb_or(n5378, n5390);
    let n5392: ZB = zb_and(n4423, n5391);
    let n5393: ZB = zb_and(n4424, n5391);
    let n5394: ZB = zb_and(n4427, n5393);
    let n5395: ZB = zb_and(n4426, n5393);
    let n5396: ZB = zb_or(n5394, n5395);
    let n5397: ZB = zb_and(n4427, n5396);
    let n5398: ZB = zb_and(n4426, n5396);
    let n5399: ZB = zb_and(n4428, n5397);
    let n5400: ZB = zb_and(n4429, n5397);
    let n5401: ZB = zb_or(n5399, n5400);
    let n5402: ZB = zb_and(n4430, n5401);
    let n5403: ZB = zb_and(n4431, n5401);
    let n5404: ZB = zb_or(n5402, n5403);
    let n5405: ZB = zb_and(n4434, n5404);
    let n5406: ZB = zb_and(n4435, n5404);
    let n5407: ZB = zb_or(n5405, n5406);
    let n5408: ZB = zb_and(n4436, n5407);
    let n5409: ZB = zb_and(n4437, n5407);
    let n5410: ZB = zb_or(n5408, n5409);
    let n5411: ZB = zb_or(n5398, n5410);
    let n5412: ZB = zb_and(n4438, n5411);
    let n5413: ZB = zb_and(n4439, n5411);
    let n5414: ZB = zb_or(n5412, n5413);
    let n5415: ZB = zb_or(n5392, n5414);
    let n5416: ZB = zb_and(n4443, n5415);
    let n5417: ZB = zb_and(n4442, n5415);
    let n5418: ZB = zb_or(n5416, n5417);
    let n5419: ZB = zb_or(n5372, n5418);
    let n5420: ZB = zb_or(n5371, n5419);
    let n5421: ZB = zb_and(n4444, n5420);
    let n5422: ZB = zb_and(n4445, n5420);
    let n5423: ZB = zb_or(n5421, n5422);
    let n5424: ZB = zb_or(n4998, n5423);
    let n5425: ZB = zb_and(n4500, n5424);
    let n5426: ZB = zb_and(n4501, n5424);
    let n5427: ZB = zb_or(n5425, n5426);
    let n5428: ZB = zb_and(n4500, n5427);
    let n5429: ZB = zb_and(n4500, n4837);
    let n5430: ZB = zb_not(n5428);
    let n5431: ZB = zb_or(n5428, n5429);
    let n5432: ZB = zsel_b(n5428, n4305, n4313);
    let n5433: ZB = zsel_b(n5431, n5430, n5367);
    let n5434: ZN = zsel_n(n5431, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n5435: ZN = zsel_n(n5431, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5436: ZN = zsel_n(n5431, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n5437: ZB = zb_or(n5368, n5431);
    let n5438: ZB = zsel_b(n5431, n5432, n5369);
    let n5440: ZN = zsel_n(n5365, r_c87, n2400);
    let n5441: ZN = zsel_n(n5365, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5442: ZN = zsel_n(n5428, r_c87, n4508);
    let n5443: ZN = zsel_n(n5428, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5444: ZN = zsel_n(n5431, n5442, n5440);
    let n5445: ZN = zsel_n(n5431, n5443, n5441);
    let n5447: ZB = zb_and(n1467, n1857);
    let n5448: ZB = zb_and(n1467, n2590);
    let n5449: ZB = zb_not(n5447);
    let n5450: ZB = zb_or(n5447, n5448);
    let n5451: ZB = zsel_b(n5447, n1337, n2342);
    let n5452: ZB = zb_and(n4840, n5035);
    let n5453: ZB = zb_and(n4841, n5035);
    let n5454: ZB = zb_and(n4398, n5452);
    let n5455: ZB = zb_and(n4446, n5452);
    let n5456: ZB = zb_or(n5454, n5455);
    let n5457: ZB = zb_and(n4448, n5456);
    let n5458: ZB = zb_and(n4449, n5456);
    let n5459: ZB = zb_and(n4450, n5458);
    let n5460: ZB = zb_and(n4451, n5458);
    let n5461: ZB = zb_or(n5459, n5460);
    let n5462: ZB = zb_or(n5457, n5461);
    let n5463: ZB = zb_and(n4453, n5462);
    let n5464: ZB = zb_and(n4452, n5462);
    let n5465: ZB = zb_or(n5463, n5464);
    let n5466: ZB = zb_or(n5453, n5465);
    let n5467: ZB = zb_or(n4998, n5466);
    let n5468: ZB = zb_and(n4500, n5467);
    let n5469: ZB = zb_and(n4501, n5467);
    let n5470: ZB = zb_or(n5468, n5469);
    let n5471: ZB = zb_and(n4500, n5470);
    let n5472: ZB = zb_and(n4500, n4864);
    let n5473: ZB = zb_not(n5471);
    let n5474: ZB = zb_or(n5471, n5472);
    let n5475: ZB = zsel_b(n5471, n4305, n4313);
    let n5476: ZN = zsel_n(n5474, n4844, n1844);
    let n5477: ZB = zsel_b(n5474, n5473, n5449);
    let n5478: ZN = zsel_n(n5474, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n5479: ZN = zsel_n(n5474, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5480: ZN = zsel_n(n5474, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n5481: ZB = zb_or(n5450, n5474);
    let n5482: ZB = zsel_b(n5474, n5475, n5451);
    let n5483: ZB = zn_gt(n5476, zn_splat(P8::from_raw(0i32)));
    let n5484: ZB = zn_le(n5476, zn_splat(P8::from_raw(0i32)));
    let n5485: ZB = zb_and(n5481, n5483);
    let n5486: ZB = zb_and(n5481, n5484);
    let n5487: ZB = zb_or(n5485, n5486);
    let n5488: ZN = zsel_n(n5447, r_c87, n2400);
    let n5489: ZN = zsel_n(n5447, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5490: ZN = zsel_n(n5471, r_c87, n4508);
    let n5491: ZN = zsel_n(n5471, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5492: ZN = zsel_n(n5474, n5490, n5488);
    let n5493: ZN = zsel_n(n5474, n5491, n5489);
    let n5495: ZB = zb_and(n1467, n1892);
    let n5496: ZB = zb_and(n1467, n2600);
    let n5497: ZB = zb_not(n5495);
    let n5498: ZB = zb_or(n5495, n5496);
    let n5499: ZB = zsel_b(n5495, n1337, n2342);
    let n5500: ZB = zb_and(n4840, n5106);
    let n5501: ZB = zb_and(n4841, n5106);
    let n5502: ZB = zb_or(n5500, n5501);
    let n5503: ZB = zb_or(n4998, n5502);
    let n5504: ZB = zb_and(n4500, n5503);
    let n5505: ZB = zb_and(n4501, n5503);
    let n5506: ZB = zb_or(n5504, n5505);
    let n5507: ZB = zb_and(n4500, n5506);
    let n5508: ZB = zb_and(n4500, n4877);
    let n5509: ZB = zb_not(n5507);
    let n5510: ZB = zb_or(n5507, n5508);
    let n5511: ZB = zsel_b(n5507, n4305, n4313);
    let n5512: ZN = zsel_n(n5510, n4844, n1844);
    let n5513: ZB = zsel_b(n5510, n5509, n5497);
    let n5514: ZN = zsel_n(n5510, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n5515: ZN = zsel_n(n5510, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5516: ZN = zsel_n(n5510, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n5517: ZB = zb_or(n5498, n5510);
    let n5518: ZB = zsel_b(n5510, n5511, n5499);
    let n5519: ZB = zn_gt(n5512, zn_splat(P8::from_raw(0i32)));
    let n5520: ZB = zn_le(n5512, zn_splat(P8::from_raw(0i32)));
    let n5521: ZB = zb_and(n5517, n5519);
    let n5522: ZB = zb_and(n5517, n5520);
    let n5523: ZB = zb_or(n5521, n5522);
    let n5524: ZN = zsel_n(n5495, r_c87, n2400);
    let n5525: ZN = zsel_n(n5495, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5526: ZN = zsel_n(n5507, r_c87, n4508);
    let n5527: ZN = zsel_n(n5507, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5528: ZN = zsel_n(n5510, n5526, n5524);
    let n5529: ZN = zsel_n(n5510, n5527, n5525);
    let n5531: ZB = zb_and(n1467, n1923);
    let n5532: ZB = zb_and(n1467, n2610);
    let n5533: ZB = zb_not(n5531);
    let n5534: ZB = zb_or(n5531, n5532);
    let n5535: ZB = zsel_b(n5531, n1337, n2342);
    let n5536: ZB = zb_and(n4840, n5177);
    let n5537: ZB = zb_and(n4841, n5177);
    let n5538: ZB = zb_or(n5536, n5537);
    let n5539: ZB = zb_or(n4998, n5538);
    let n5540: ZB = zb_and(n4500, n5539);
    let n5541: ZB = zb_and(n4501, n5539);
    let n5542: ZB = zb_or(n5540, n5541);
    let n5543: ZB = zb_and(n4500, n5542);
    let n5544: ZB = zb_and(n4500, n4888);
    let n5545: ZB = zb_not(n5543);
    let n5546: ZB = zb_or(n5543, n5544);
    let n5547: ZB = zsel_b(n5543, n4305, n4313);
    let n5548: ZN = zsel_n(n5546, n4844, n1844);
    let n5549: ZB = zsel_b(n5546, n5545, n5533);
    let n5550: ZN = zsel_n(n5546, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n5551: ZN = zsel_n(n5546, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5552: ZN = zsel_n(n5546, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n5553: ZB = zb_or(n5534, n5546);
    let n5554: ZB = zsel_b(n5546, n5547, n5535);
    let n5555: ZB = zn_gt(n5548, zn_splat(P8::from_raw(0i32)));
    let n5556: ZB = zn_le(n5548, zn_splat(P8::from_raw(0i32)));
    let n5557: ZB = zb_and(n5553, n5555);
    let n5558: ZB = zb_and(n5553, n5556);
    let n5559: ZB = zb_or(n5557, n5558);
    let n5560: ZN = zsel_n(n5531, r_c87, n2400);
    let n5561: ZN = zsel_n(n5531, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5562: ZN = zsel_n(n5543, r_c87, n4508);
    let n5563: ZN = zsel_n(n5543, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5564: ZN = zsel_n(n5546, n5562, n5560);
    let n5565: ZN = zsel_n(n5546, n5563, n5561);
    let n5567: ZB = zb_and(n1467, n1955);
    let n5568: ZB = zb_and(n1467, n2618);
    let n5569: ZB = zb_not(n5567);
    let n5570: ZB = zb_or(n5567, n5568);
    let n5571: ZB = zsel_b(n5567, n1337, n2342);
    let n5572: ZB = zb_or(n5452, n5453);
    let n5573: ZB = zb_or(n4998, n5572);
    let n5574: ZB = zb_and(n4500, n5573);
    let n5575: ZB = zb_and(n4501, n5573);
    let n5576: ZB = zb_or(n5574, n5575);
    let n5577: ZB = zb_and(n4500, n5576);
    let n5578: ZB = zb_and(n4500, n4897);
    let n5579: ZB = zb_not(n5577);
    let n5580: ZB = zb_or(n5577, n5578);
    let n5581: ZB = zsel_b(n5577, n4305, n4313);
    let n5582: ZN = zsel_n(n5580, n4844, n1844);
    let n5583: ZB = zsel_b(n5580, n5579, n5569);
    let n5584: ZN = zsel_n(n5580, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n5585: ZN = zsel_n(n5580, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5586: ZN = zsel_n(n5580, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n5587: ZB = zb_or(n5570, n5580);
    let n5588: ZB = zsel_b(n5580, n5581, n5571);
    let n5589: ZB = zn_gt(n5582, zn_splat(P8::from_raw(0i32)));
    let n5590: ZB = zn_le(n5582, zn_splat(P8::from_raw(0i32)));
    let n5591: ZB = zb_and(n5587, n5589);
    let n5592: ZB = zb_and(n5587, n5590);
    let n5593: ZB = zb_or(n5591, n5592);
    let n5594: ZN = zsel_n(n5567, r_c87, n2400);
    let n5595: ZN = zsel_n(n5567, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5596: ZN = zsel_n(n5577, r_c87, n4508);
    let n5597: ZN = zsel_n(n5577, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5598: ZN = zsel_n(n5580, n5596, n5594);
    let n5599: ZN = zsel_n(n5580, n5597, n5595);
    let n5601: ZB = zb_and(n1467, n2009);
    let n5602: ZB = zb_and(n1467, n2640);
    let n5603: ZB = zb_not(n5601);
    let n5604: ZB = zb_or(n5601, n5602);
    let n5605: ZB = zsel_b(n5601, n1337, n2342);
    let n5606: ZB = zb_and(n4840, n5259);
    let n5607: ZB = zb_and(n4841, n5259);
    let n5608: ZB = zb_and(n4398, n5606);
    let n5609: ZB = zb_and(n4446, n5606);
    let n5610: ZB = zb_or(n5608, n5609);
    let n5611: ZB = zb_and(n4448, n5610);
    let n5612: ZB = zb_and(n4449, n5610);
    let n5613: ZB = zb_and(n4450, n5612);
    let n5614: ZB = zb_and(n4451, n5612);
    let n5615: ZB = zb_or(n5613, n5614);
    let n5616: ZB = zb_or(n5611, n5615);
    let n5617: ZB = zb_and(n4453, n5616);
    let n5618: ZB = zb_and(n4452, n5616);
    let n5619: ZB = zb_or(n5617, n5618);
    let n5620: ZB = zb_or(n5607, n5619);
    let n5621: ZB = zb_or(n4998, n5620);
    let n5622: ZB = zb_and(n4500, n5621);
    let n5623: ZB = zb_and(n4501, n5621);
    let n5624: ZB = zb_or(n5622, n5623);
    let n5625: ZB = zb_and(n4500, n5624);
    let n5626: ZB = zb_and(n4500, n4920);
    let n5627: ZB = zb_not(n5625);
    let n5628: ZB = zb_or(n5625, n5626);
    let n5629: ZB = zsel_b(n5625, n4305, n4313);
    let n5630: ZN = zsel_n(n5628, n4844, n1844);
    let n5631: ZB = zsel_b(n5628, n5627, n5603);
    let n5632: ZN = zsel_n(n5628, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n5633: ZN = zsel_n(n5628, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5634: ZN = zsel_n(n5628, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n5635: ZB = zb_or(n5604, n5628);
    let n5636: ZB = zsel_b(n5628, n5629, n5605);
    let n5637: ZB = zn_gt(n5630, zn_splat(P8::from_raw(0i32)));
    let n5638: ZB = zn_le(n5630, zn_splat(P8::from_raw(0i32)));
    let n5639: ZB = zb_and(n5635, n5637);
    let n5640: ZB = zb_and(n5635, n5638);
    let n5641: ZB = zb_or(n5639, n5640);
    let n5642: ZN = zsel_n(n5601, r_c87, n2400);
    let n5643: ZN = zsel_n(n5601, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5644: ZN = zsel_n(n5625, r_c87, n4508);
    let n5645: ZN = zsel_n(n5625, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5646: ZN = zsel_n(n5628, n5644, n5642);
    let n5647: ZN = zsel_n(n5628, n5645, n5643);
    let n5649: ZB = zb_and(n1467, n2039);
    let n5650: ZB = zb_and(n1467, n2650);
    let n5651: ZB = zb_not(n5649);
    let n5652: ZB = zb_or(n5649, n5650);
    let n5653: ZB = zsel_b(n5649, n1337, n2342);
    let n5654: ZB = zb_and(n4840, n5341);
    let n5655: ZB = zb_and(n4841, n5341);
    let n5656: ZB = zb_or(n5654, n5655);
    let n5657: ZB = zb_or(n4998, n5656);
    let n5658: ZB = zb_and(n4500, n5657);
    let n5659: ZB = zb_and(n4501, n5657);
    let n5660: ZB = zb_or(n5658, n5659);
    let n5661: ZB = zb_and(n4500, n5660);
    let n5662: ZB = zb_and(n4500, n4931);
    let n5663: ZB = zb_not(n5661);
    let n5664: ZB = zb_or(n5661, n5662);
    let n5665: ZB = zsel_b(n5661, n4305, n4313);
    let n5666: ZN = zsel_n(n5664, n4844, n1844);
    let n5667: ZB = zsel_b(n5664, n5663, n5651);
    let n5668: ZN = zsel_n(n5664, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n5669: ZN = zsel_n(n5664, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5670: ZN = zsel_n(n5664, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n5671: ZB = zb_or(n5652, n5664);
    let n5672: ZB = zsel_b(n5664, n5665, n5653);
    let n5673: ZB = zn_gt(n5666, zn_splat(P8::from_raw(0i32)));
    let n5674: ZB = zn_le(n5666, zn_splat(P8::from_raw(0i32)));
    let n5675: ZB = zb_and(n5671, n5673);
    let n5676: ZB = zb_and(n5671, n5674);
    let n5677: ZB = zb_or(n5675, n5676);
    let n5678: ZN = zsel_n(n5649, r_c87, n2400);
    let n5679: ZN = zsel_n(n5649, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5680: ZN = zsel_n(n5661, r_c87, n4508);
    let n5681: ZN = zsel_n(n5661, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5682: ZN = zsel_n(n5664, n5680, n5678);
    let n5683: ZN = zsel_n(n5664, n5681, n5679);
    let n5685: ZB = zb_and(n1467, n2069);
    let n5686: ZB = zb_and(n1467, n2660);
    let n5687: ZB = zb_not(n5685);
    let n5688: ZB = zb_or(n5685, n5686);
    let n5689: ZB = zsel_b(n5685, n1337, n2342);
    let n5690: ZB = zb_and(n4840, n5423);
    let n5691: ZB = zb_and(n4841, n5423);
    let n5692: ZB = zb_or(n5690, n5691);
    let n5693: ZB = zb_or(n4998, n5692);
    let n5694: ZB = zb_and(n4500, n5693);
    let n5695: ZB = zb_and(n4501, n5693);
    let n5696: ZB = zb_or(n5694, n5695);
    let n5697: ZB = zb_and(n4500, n5696);
    let n5698: ZB = zb_and(n4500, n4942);
    let n5699: ZB = zb_not(n5697);
    let n5700: ZB = zb_or(n5697, n5698);
    let n5701: ZB = zsel_b(n5697, n4305, n4313);
    let n5702: ZN = zsel_n(n5700, n4844, n1844);
    let n5703: ZB = zsel_b(n5700, n5699, n5687);
    let n5704: ZN = zsel_n(n5700, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n5705: ZN = zsel_n(n5700, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5706: ZN = zsel_n(n5700, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n5707: ZB = zb_or(n5688, n5700);
    let n5708: ZB = zsel_b(n5700, n5701, n5689);
    let n5709: ZB = zn_gt(n5702, zn_splat(P8::from_raw(0i32)));
    let n5710: ZB = zn_le(n5702, zn_splat(P8::from_raw(0i32)));
    let n5711: ZB = zb_and(n5707, n5709);
    let n5712: ZB = zb_and(n5707, n5710);
    let n5713: ZB = zb_or(n5711, n5712);
    let n5714: ZN = zsel_n(n5685, r_c87, n2400);
    let n5715: ZN = zsel_n(n5685, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5716: ZN = zsel_n(n5697, r_c87, n4508);
    let n5717: ZN = zsel_n(n5697, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5718: ZN = zsel_n(n5700, n5716, n5714);
    let n5719: ZN = zsel_n(n5700, n5717, n5715);
    let n5721: ZB = zb_and(n1467, n2097);
    let n5722: ZB = zb_and(n1467, n2668);
    let n5723: ZB = zb_not(n5721);
    let n5724: ZB = zb_or(n5721, n5722);
    let n5725: ZB = zsel_b(n5721, n1337, n2342);
    let n5726: ZB = zb_or(n5606, n5607);
    let n5727: ZB = zb_or(n4998, n5726);
    let n5728: ZB = zb_and(n4500, n5727);
    let n5729: ZB = zb_and(n4501, n5727);
    let n5730: ZB = zb_or(n5728, n5729);
    let n5731: ZB = zb_and(n4500, n5730);
    let n5732: ZB = zb_and(n4500, n4951);
    let n5733: ZB = zb_not(n5731);
    let n5734: ZB = zb_or(n5731, n5732);
    let n5735: ZB = zsel_b(n5731, n4305, n4313);
    let n5736: ZN = zsel_n(n5734, n4844, n1844);
    let n5737: ZB = zsel_b(n5734, n5733, n5723);
    let n5738: ZN = zsel_n(n5734, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(196608i32)));
    let n5739: ZN = zsel_n(n5734, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5740: ZN = zsel_n(n5734, zn_splat(P8::from_raw(8388608i32)), zn_splat(P8::from_raw(8126464i32)));
    let n5741: ZB = zb_or(n5724, n5734);
    let n5742: ZB = zsel_b(n5734, n5735, n5725);
    let n5743: ZB = zn_gt(n5736, zn_splat(P8::from_raw(0i32)));
    let n5744: ZB = zn_le(n5736, zn_splat(P8::from_raw(0i32)));
    let n5745: ZB = zb_and(n5741, n5743);
    let n5746: ZB = zb_and(n5741, n5744);
    let n5747: ZB = zb_or(n5745, n5746);
    let n5748: ZN = zsel_n(n5721, r_c87, n2400);
    let n5749: ZN = zsel_n(n5721, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5750: ZN = zsel_n(n5731, r_c87, n4508);
    let n5751: ZN = zsel_n(n5731, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5752: ZN = zsel_n(n5734, n5750, n5748);
    let n5753: ZN = zsel_n(n5734, n5751, n5749);
    let n5755: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n5756: ZN = zn_sub(n2685, zn_splat(P8::from_raw(32768i32)));
    let n5757: ZN = zn_sub(n5756, n2686);
    let n5758: ZN = zn_sub(n2713, zn_splat(P8::from_raw(32768i32)));
    let n5759: ZN = zn_sub(n5758, n2714);
    let n5760: ZN = zn_sub(r_c255, zn_splat(P8::from_raw(65536i32)));
    let n5761: ZB = zb_and(r_c265, n1493);
    let n5762: ZB = zb_and(r_c266, n1493);
    let n5763: ZN = zsel_n(n3121, zn_splat(P8::from_raw(0i32)), n5757);
    let n5764: ZN = zsel_n(n2710, n5757, n5763);
    let n5765: ZN = zsel_n(n3081, zn_splat(P8::from_raw(0i32)), n5764);
    let n5766: ZN = zsel_n(n2708, n5757, n5765);
    let n5767: ZN = zsel_n(n3041, zn_splat(P8::from_raw(0i32)), n5766);
    let n5768: ZN = zsel_n(n2706, n5757, n5767);
    let n5769: ZN = zsel_n(n3001, zn_splat(P8::from_raw(0i32)), n5768);
    let n5770: ZN = zsel_n(n2704, n5757, n5769);
    let n5771: ZN = zsel_n(n2961, zn_splat(P8::from_raw(0i32)), n5770);
    let n5772: ZN = zsel_n(n2702, n5757, n5771);
    let n5773: ZN = zsel_n(n2921, zn_splat(P8::from_raw(0i32)), n5772);
    let n5774: ZN = zsel_n(n2700, n5757, n5773);
    let n5775: ZN = zsel_n(n2881, zn_splat(P8::from_raw(0i32)), n5774);
    let n5776: ZN = zsel_n(n2698, n5757, n5775);
    let n5777: ZN = zsel_n(n2841, zn_splat(P8::from_raw(0i32)), n5776);
    let n5778: ZN = zsel_n(n3497, zn_splat(P8::from_raw(0i32)), n5759);
    let n5779: ZN = zsel_n(n2784, n5759, n5778);
    let n5780: ZN = zsel_n(n3459, zn_splat(P8::from_raw(0i32)), n5779);
    let n5781: ZN = zsel_n(n2775, n5759, n5780);
    let n5782: ZN = zsel_n(n3421, zn_splat(P8::from_raw(0i32)), n5781);
    let n5783: ZN = zsel_n(n2766, n5759, n5782);
    let n5784: ZN = zsel_n(n3383, zn_splat(P8::from_raw(0i32)), n5783);
    let n5785: ZN = zsel_n(n2757, n5759, n5784);
    let n5786: ZN = zsel_n(n3345, zn_splat(P8::from_raw(0i32)), n5785);
    let n5787: ZN = zsel_n(n2748, n5759, n5786);
    let n5788: ZN = zsel_n(n3307, zn_splat(P8::from_raw(0i32)), n5787);
    let n5789: ZN = zsel_n(n2739, n5759, n5788);
    let n5790: ZN = zsel_n(n3269, zn_splat(P8::from_raw(0i32)), n5789);
    let n5791: ZN = zsel_n(n2730, n5759, n5790);
    let n5792: ZN = zsel_n(n3231, zn_splat(P8::from_raw(0i32)), n5791);
    let n5793: ZN = zsel_n(n2682, n5777, r_c318);
    let n5794: ZN = zsel_n(n2682, n5792, r_c319);
    let n5795: ZN = zn_sub(n3564, r_c308);
    let n5796: ZN = zn_max(r_c310, n5795);
    let n5797: ZN = zn_add(r_c308, n3564);
    let n5798: ZN = zn_min(r_c310, n5797);
    let n5799: ZN = zsel_n(n4362, n5796, n5798);
    let n5800: ZN = zn_sub(n3565, r_c309);
    let n5801: ZN = zn_max(r_c311, n5800);
    let n5802: ZN = zn_add(r_c309, n3565);
    let n5803: ZN = zn_min(r_c311, n5802);
    let n5804: ZN = zsel_n(n4364, n5801, n5803);
    let n5805: ZN = zsel_n(n4400, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n5806: ZN = zn_sub(n3565, n5805);
    let n5807: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n5806);
    let n5808: ZN = zn_add(n3565, n5805);
    let n5809: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n5808);
    let n5810: ZN = zsel_n(n4406, n5807, n5809);
    let n5811: ZN = zsel_n(n4350, n5810, n3565);
    let n5812: ZN = zn_neg(n4441);
    let n5813: ZN = zn_mul(n5812, zn_splat(P8::from_raw(131072i32)));
    let n5814: ZN = zsel_n(n4443, n5813, n4394);
    let n5815: ZN = zsel_n(n4443, zn_splat(P8::from_raw(-131072i32)), n5811);
    let n5816: ZN = zsel_n(n4408, zn_splat(P8::from_raw(0i32)), n4352);
    let n5817: ZN = zsel_n(n4408, n4394, n5814);
    let n5818: ZN = zsel_n(n4408, zn_splat(P8::from_raw(-131072i32)), n5815);
    let n5819: ZN = zn_sub(n4351, zn_splat(P8::from_raw(65536i32)));
    let n5820: ZN = zsel_n(n4450, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n5821: ZN = zsel_n(n4448, zn_splat(P8::from_raw(131072i32)), n5820);
    let n5822: ZN = zsel_n(n4453, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n5823: ZN = zsel_n(n2793, n5760, r_c255);
    let n5824: ZB = zsel_b(n2793, r_c312, n4398);
    let n5825: ZN = zsel_n(n2793, n5799, n4394);
    let n5826: ZN = zsel_n(n2793, n5804, n5811);
    let n5827: ZB = zb_and(n4501, n5039);
    let n5828: ZN = zsel_n(n1493, n5755, r_c20);
    let n5829: ZN = zsel_n(n1493, r_c253, n249);
    let n5830: ZN = zsel_n(n1493, r_c255, n5823);
    let n5831: ZN = zsel_n(n1493, r_c256, n4351);
    let n5832: ZN = zsel_n(n1493, r_c258, n4352);
    let n5833: ZN = zsel_n(n1493, r_c272, n3562);
    let n5834: ZN = zsel_n(n1493, r_c273, n3563);
    let n5835: ZB = zsel_b(n1493, r_c312, n5824);
    let n5836: ZN = zsel_n(n1493, r_c318, n5793);
    let n5837: ZN = zsel_n(n1493, r_c319, n5794);
    let n5838: ZN = zsel_n(n1493, r_c320, n5825);
    let n5839: ZN = zsel_n(n1493, r_c321, n5826);
    let n5840: ZB = zb_or(n1493, n5827);
    let n5841: ZB = zb_or(n1493, n4305);
    let n5842: ZB = zn_gt(n5828, zn_splat(P8::from_raw(0i32)));
    let n5843: ZB = zn_le(n5828, zn_splat(P8::from_raw(0i32)));
    let n5844: ZB = zb_and(n5840, n5842);
    let n5845: ZB = zb_and(n5840, n5843);
    let n5846: ZB = zn_lt(n5833, zn_splat(P8::from_raw(-65536i32)));
    let n5847: ZB = zn_ge(n5833, zn_splat(P8::from_raw(-65536i32)));
    let n5848: ZB = zb_and(n5845, n5847);
    let n5849: ZB = zb_and(n5845, n5846);
    let n5850: ZB = zn_gt(n5833, zn_splat(P8::from_raw(7929856i32)));
    let n5851: ZB = zb_or(n5848, n5849);
    let n5852: ZB = zb_or(n5846, n5850);
    let n5853: ZB = zb_not(n5852);
    let n5854: ZB = zb_and(n5851, n5852);
    let n5855: ZB = zb_and(n5851, n5853);
    let n5856: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n5833);
    let n5857: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n5856);
    let n5858: ZN = zsel_n(n5852, n5857, n5833);
    let n5859: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n5838);
    let n5860: ZB = zb_or(n5854, n5855);
    let n5861: ZN = zsel_n(n5842, n5833, n5858);
    let n5862: ZN = zsel_n(n5842, n5838, n5859);
    let n5863: ZB = zb_or(n5844, n5860);
    let n5865: ZN = zn_max(n4533, n5806);
    let n5866: ZN = zn_min(n4533, n5808);
    let n5867: ZN = zsel_n(n4534, n5865, n5866);
    let n5868: ZN = zsel_n(n4350, n5867, n3565);
    let n5869: ZN = zsel_n(n4443, n5813, n4513);
    let n5870: ZN = zsel_n(n4443, zn_splat(P8::from_raw(-131072i32)), n5868);
    let n5871: ZN = zsel_n(n4408, n4513, n5869);
    let n5872: ZN = zsel_n(n4408, zn_splat(P8::from_raw(-131072i32)), n5870);
    let n5873: ZB = zsel_b(n2793, r_c312, n4517);
    let n5874: ZN = zsel_n(n2793, n5799, n4513);
    let n5875: ZN = zsel_n(n2793, n5804, n5868);
    let n5876: ZB = zb_and(n4501, n5110);
    let n5877: ZB = zsel_b(n1493, r_c312, n5873);
    let n5878: ZN = zsel_n(n1493, r_c320, n5874);
    let n5879: ZN = zsel_n(n1493, r_c321, n5875);
    let n5880: ZB = zb_or(n1493, n5876);
    let n5881: ZB = zb_and(n5842, n5880);
    let n5882: ZB = zb_and(n5843, n5880);
    let n5883: ZB = zb_and(n5847, n5882);
    let n5884: ZB = zb_and(n5846, n5882);
    let n5885: ZB = zb_or(n5883, n5884);
    let n5886: ZB = zb_and(n5852, n5885);
    let n5887: ZB = zb_and(n5853, n5885);
    let n5888: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n5878);
    let n5889: ZB = zb_or(n5886, n5887);
    let n5890: ZN = zsel_n(n5842, n5878, n5888);
    let n5891: ZB = zb_or(n5881, n5889);
    let n5892: ZN = zn_max(n4608, n5806);
    let n5893: ZN = zn_min(n4608, n5808);
    let n5894: ZN = zsel_n(n4609, n5892, n5893);
    let n5895: ZN = zsel_n(n4350, n5894, n3565);
    let n5896: ZN = zsel_n(n4443, n5813, n4588);
    let n5897: ZN = zsel_n(n4443, zn_splat(P8::from_raw(-131072i32)), n5895);
    let n5898: ZN = zsel_n(n4408, n4588, n5896);
    let n5899: ZN = zsel_n(n4408, zn_splat(P8::from_raw(-131072i32)), n5897);
    let n5900: ZB = zsel_b(n2793, r_c312, n4592);
    let n5901: ZN = zsel_n(n2793, n5799, n4588);
    let n5902: ZN = zsel_n(n2793, n5804, n5895);
    let n5903: ZB = zb_and(n4501, n5181);
    let n5904: ZB = zsel_b(n1493, r_c312, n5900);
    let n5905: ZN = zsel_n(n1493, r_c320, n5901);
    let n5906: ZN = zsel_n(n1493, r_c321, n5902);
    let n5907: ZB = zb_or(n1493, n5903);
    let n5908: ZB = zb_and(n5842, n5907);
    let n5909: ZB = zb_and(n5843, n5907);
    let n5910: ZB = zb_and(n5847, n5909);
    let n5911: ZB = zb_and(n5846, n5909);
    let n5912: ZB = zb_or(n5910, n5911);
    let n5913: ZB = zb_and(n5852, n5912);
    let n5914: ZB = zb_and(n5853, n5912);
    let n5915: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n5905);
    let n5916: ZB = zb_or(n5913, n5914);
    let n5917: ZN = zsel_n(n5842, n5905, n5915);
    let n5918: ZB = zb_or(n5908, n5916);
    let n5919: ZB = zb_or(r_c266, n111);
    let n5920: ZN = zsel_n(n240, n5816, n4352);
    let n5921: ZN = zsel_n(n240, n5817, n4394);
    let n5922: ZN = zsel_n(n240, n5818, n5811);
    let n5923: ZN = zsel_n(n2793, n4352, n5920);
    let n5924: ZN = zsel_n(n2793, n5799, n5921);
    let n5925: ZN = zsel_n(n2793, n5804, n5922);
    let n5926: ZB = zb_and(n4501, n5263);
    let n5927: ZN = zsel_n(n1493, r_c258, n5923);
    let n5928: ZN = zsel_n(n1493, r_c320, n5924);
    let n5929: ZN = zsel_n(n1493, r_c321, n5925);
    let n5930: ZB = zb_or(n1493, n5926);
    let n5931: ZB = zb_and(n5842, n5930);
    let n5932: ZB = zb_and(n5843, n5930);
    let n5933: ZB = zb_and(n5847, n5932);
    let n5934: ZB = zb_and(n5846, n5932);
    let n5935: ZB = zb_or(n5933, n5934);
    let n5936: ZB = zb_and(n5852, n5935);
    let n5937: ZB = zb_and(n5853, n5935);
    let n5938: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n5928);
    let n5939: ZB = zb_or(n5936, n5937);
    let n5940: ZN = zsel_n(n5842, n5928, n5938);
    let n5941: ZB = zb_or(n5931, n5939);
    let n5942: ZN = zsel_n(n240, n5871, n4513);
    let n5943: ZN = zsel_n(n240, n5872, n5868);
    let n5944: ZN = zsel_n(n2793, n5799, n5942);
    let n5945: ZN = zsel_n(n2793, n5804, n5943);
    let n5946: ZB = zb_and(n4501, n5345);
    let n5947: ZN = zsel_n(n1493, r_c320, n5944);
    let n5948: ZN = zsel_n(n1493, r_c321, n5945);
    let n5949: ZB = zb_or(n1493, n5946);
    let n5950: ZB = zb_and(n5842, n5949);
    let n5951: ZB = zb_and(n5843, n5949);
    let n5952: ZB = zb_and(n5847, n5951);
    let n5953: ZB = zb_and(n5846, n5951);
    let n5954: ZB = zb_or(n5952, n5953);
    let n5955: ZB = zb_and(n5852, n5954);
    let n5956: ZB = zb_and(n5853, n5954);
    let n5957: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n5947);
    let n5958: ZB = zb_or(n5955, n5956);
    let n5959: ZN = zsel_n(n5842, n5947, n5957);
    let n5960: ZB = zb_or(n5950, n5958);
    let n5961: ZN = zsel_n(n240, n5898, n4588);
    let n5962: ZN = zsel_n(n240, n5899, n5895);
    let n5963: ZN = zsel_n(n2793, n5799, n5961);
    let n5964: ZN = zsel_n(n2793, n5804, n5962);
    let n5965: ZB = zb_and(n4501, n5427);
    let n5966: ZN = zsel_n(n1493, r_c320, n5963);
    let n5967: ZN = zsel_n(n1493, r_c321, n5964);
    let n5968: ZB = zb_or(n1493, n5965);
    let n5969: ZB = zb_and(n5842, n5968);
    let n5970: ZB = zb_and(n5843, n5968);
    let n5971: ZB = zb_and(n5847, n5970);
    let n5972: ZB = zb_and(n5846, n5970);
    let n5973: ZB = zb_or(n5971, n5972);
    let n5974: ZB = zb_and(n5852, n5973);
    let n5975: ZB = zb_and(n5853, n5973);
    let n5976: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n5966);
    let n5977: ZB = zb_or(n5974, n5975);
    let n5978: ZN = zsel_n(n5842, n5966, n5976);
    let n5979: ZB = zb_or(n5969, n5977);
    let n5980: ZB = zb_or(r_c265, n111);
    let n5981: ZN = zsel_n(n4840, zn_splat(P8::from_raw(655360i32)), n249);
    let n5982: ZN = zsel_n(n4840, zn_splat(P8::from_raw(262144i32)), r_c255);
    let n5983: ZN = zsel_n(n4840, n5819, n4351);
    let n5984: ZN = zsel_n(n4840, zn_splat(P8::from_raw(98304i32)), r_c308);
    let n5985: ZN = zsel_n(n4840, n5822, r_c309);
    let n5986: ZN = zsel_n(n4840, n5821, r_c310);
    let n5987: ZN = zsel_n(n4840, zn_splat(P8::from_raw(0i32)), r_c311);
    let n5988: ZN = zsel_n(n4840, n4447, n4394);
    let n5989: ZN = zsel_n(n4840, zn_splat(P8::from_raw(0i32)), n5811);
    let n5990: ZN = zsel_n(n2793, n249, n5981);
    let n5991: ZN = zsel_n(n2793, n5760, n5982);
    let n5992: ZN = zsel_n(n2793, n4351, n5983);
    let n5993: ZN = zsel_n(n2793, r_c308, n5984);
    let n5994: ZN = zsel_n(n2793, r_c309, n5985);
    let n5995: ZN = zsel_n(n2793, r_c310, n5986);
    let n5996: ZN = zsel_n(n2793, r_c311, n5987);
    let n5997: ZN = zsel_n(n2793, n5799, n5988);
    let n5998: ZN = zsel_n(n2793, n5804, n5989);
    let n5999: ZB = zb_and(n4501, n5470);
    let n6000: ZN = zsel_n(n1493, n5755, n4844);
    let n6001: ZB = zsel_b(n1493, r_c41, n4845);
    let n6002: ZN = zsel_n(n1493, r_c253, n5990);
    let n6003: ZN = zsel_n(n1493, r_c255, n5991);
    let n6004: ZN = zsel_n(n1493, r_c256, n5992);
    let n6005: ZN = zsel_n(n1493, r_c308, n5993);
    let n6006: ZN = zsel_n(n1493, r_c309, n5994);
    let n6007: ZN = zsel_n(n1493, r_c310, n5995);
    let n6008: ZN = zsel_n(n1493, r_c311, n5996);
    let n6009: ZN = zsel_n(n1493, r_c320, n5997);
    let n6010: ZN = zsel_n(n1493, r_c321, n5998);
    let n6011: ZB = zb_or(n1493, n5999);
    let n6012: ZB = zn_gt(n6000, zn_splat(P8::from_raw(0i32)));
    let n6013: ZB = zn_le(n6000, zn_splat(P8::from_raw(0i32)));
    let n6014: ZB = zb_and(n6011, n6012);
    let n6015: ZB = zb_and(n6011, n6013);
    let n6016: ZB = zb_and(n5847, n6015);
    let n6017: ZB = zb_and(n5846, n6015);
    let n6018: ZB = zb_or(n6016, n6017);
    let n6019: ZB = zb_and(n5852, n6018);
    let n6020: ZB = zb_and(n5853, n6018);
    let n6021: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n6009);
    let n6022: ZB = zb_or(n6019, n6020);
    let n6023: ZN = zsel_n(n6012, n5833, n5858);
    let n6024: ZN = zsel_n(n6012, n6009, n6021);
    let n6025: ZB = zb_or(n6014, n6022);
    let n6026: ZN = zsel_n(n4840, zn_splat(P8::from_raw(69510i32)), r_c309);
    let n6027: ZN = zsel_n(n4840, zn_splat(P8::from_raw(-131072i32)), r_c310);
    let n6028: ZN = zsel_n(n4840, zn_splat(P8::from_raw(-327680i32)), n4513);
    let n6029: ZN = zsel_n(n4840, zn_splat(P8::from_raw(0i32)), n5868);
    let n6030: ZN = zsel_n(n2793, r_c309, n6026);
    let n6031: ZN = zsel_n(n2793, r_c310, n6027);
    let n6032: ZN = zsel_n(n2793, n5799, n6028);
    let n6033: ZN = zsel_n(n2793, n5804, n6029);
    let n6034: ZB = zb_and(n4501, n5506);
    let n6035: ZN = zsel_n(n1493, r_c309, n6030);
    let n6036: ZN = zsel_n(n1493, r_c310, n6031);
    let n6037: ZN = zsel_n(n1493, r_c320, n6032);
    let n6038: ZN = zsel_n(n1493, r_c321, n6033);
    let n6039: ZB = zb_or(n1493, n6034);
    let n6040: ZB = zb_and(n6012, n6039);
    let n6041: ZB = zb_and(n6013, n6039);
    let n6042: ZB = zb_and(n5847, n6041);
    let n6043: ZB = zb_and(n5846, n6041);
    let n6044: ZB = zb_or(n6042, n6043);
    let n6045: ZB = zb_and(n5852, n6044);
    let n6046: ZB = zb_and(n5853, n6044);
    let n6047: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n6037);
    let n6048: ZB = zb_or(n6045, n6046);
    let n6049: ZN = zsel_n(n6012, n6037, n6047);
    let n6050: ZB = zb_or(n6040, n6048);
    let n6051: ZN = zsel_n(n4840, zn_splat(P8::from_raw(131072i32)), r_c310);
    let n6052: ZN = zsel_n(n4840, zn_splat(P8::from_raw(327680i32)), n4588);
    let n6053: ZN = zsel_n(n4840, zn_splat(P8::from_raw(0i32)), n5895);
    let n6054: ZN = zsel_n(n2793, r_c310, n6051);
    let n6055: ZN = zsel_n(n2793, n5799, n6052);
    let n6056: ZN = zsel_n(n2793, n5804, n6053);
    let n6057: ZB = zb_and(n4501, n5542);
    let n6058: ZN = zsel_n(n1493, r_c310, n6054);
    let n6059: ZN = zsel_n(n1493, r_c320, n6055);
    let n6060: ZN = zsel_n(n1493, r_c321, n6056);
    let n6061: ZB = zb_or(n1493, n6057);
    let n6062: ZB = zb_and(n6012, n6061);
    let n6063: ZB = zb_and(n6013, n6061);
    let n6064: ZB = zb_and(n5847, n6063);
    let n6065: ZB = zb_and(n5846, n6063);
    let n6066: ZB = zb_or(n6064, n6065);
    let n6067: ZB = zb_and(n5852, n6066);
    let n6068: ZB = zb_and(n5853, n6066);
    let n6069: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n6059);
    let n6070: ZB = zb_or(n6067, n6068);
    let n6071: ZN = zsel_n(n6012, n6059, n6069);
    let n6072: ZB = zb_or(n6062, n6070);
    let n6073: ZN = zsel_n(n4840, zn_splat(P8::from_raw(69510i32)), r_c308);
    let n6074: ZN = zsel_n(n4840, zn_splat(P8::from_raw(98304i32)), r_c309);
    let n6075: ZN = zsel_n(n4840, zn_splat(P8::from_raw(0i32)), r_c310);
    let n6076: ZN = zsel_n(n4840, zn_splat(P8::from_raw(-98304i32)), r_c311);
    let n6077: ZN = zsel_n(n4840, zn_splat(P8::from_raw(0i32)), n4394);
    let n6078: ZN = zsel_n(n4840, zn_splat(P8::from_raw(-327680i32)), n5811);
    let n6079: ZN = zsel_n(n2793, r_c308, n6073);
    let n6080: ZN = zsel_n(n2793, r_c309, n6074);
    let n6081: ZN = zsel_n(n2793, r_c310, n6075);
    let n6082: ZN = zsel_n(n2793, r_c311, n6076);
    let n6083: ZN = zsel_n(n2793, n5799, n6077);
    let n6084: ZN = zsel_n(n2793, n5804, n6078);
    let n6085: ZB = zb_and(n4501, n5576);
    let n6086: ZN = zsel_n(n1493, r_c308, n6079);
    let n6087: ZN = zsel_n(n1493, r_c309, n6080);
    let n6088: ZN = zsel_n(n1493, r_c310, n6081);
    let n6089: ZN = zsel_n(n1493, r_c311, n6082);
    let n6090: ZN = zsel_n(n1493, r_c320, n6083);
    let n6091: ZN = zsel_n(n1493, r_c321, n6084);
    let n6092: ZB = zb_or(n1493, n6085);
    let n6093: ZB = zb_and(n6012, n6092);
    let n6094: ZB = zb_and(n6013, n6092);
    let n6095: ZB = zb_and(n5847, n6094);
    let n6096: ZB = zb_and(n5846, n6094);
    let n6097: ZB = zb_or(n6095, n6096);
    let n6098: ZB = zb_and(n5852, n6097);
    let n6099: ZB = zb_and(n5853, n6097);
    let n6100: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n6090);
    let n6101: ZB = zb_or(n6098, n6099);
    let n6102: ZN = zsel_n(n6012, n6090, n6100);
    let n6103: ZB = zb_or(n6093, n6101);
    let n6104: ZN = zsel_n(n4840, zn_splat(P8::from_raw(-231700i32)), n4513);
    let n6105: ZN = zsel_n(n4840, zn_splat(P8::from_raw(-231700i32)), n5868);
    let n6106: ZN = zsel_n(n2793, n5799, n6104);
    let n6107: ZN = zsel_n(n2793, n5804, n6105);
    let n6108: ZN = zsel_n(n1493, r_c320, n6106);
    let n6109: ZN = zsel_n(n1493, r_c321, n6107);
    let n6110: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n6108);
    let n6111: ZN = zsel_n(n6012, n6108, n6110);
    let n6112: ZN = zsel_n(n4840, zn_splat(P8::from_raw(231700i32)), n4588);
    let n6113: ZN = zsel_n(n4840, zn_splat(P8::from_raw(-231700i32)), n5895);
    let n6114: ZN = zsel_n(n2793, n5799, n6112);
    let n6115: ZN = zsel_n(n2793, n5804, n6113);
    let n6116: ZN = zsel_n(n1493, r_c320, n6114);
    let n6117: ZN = zsel_n(n1493, r_c321, n6115);
    let n6118: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n6116);
    let n6119: ZN = zsel_n(n6012, n6116, n6118);
    let n6120: ZN = zsel_n(n4840, zn_splat(P8::from_raw(131072i32)), r_c311);
    let n6121: ZN = zsel_n(n4840, zn_splat(P8::from_raw(327680i32)), n5811);
    let n6122: ZN = zsel_n(n2793, r_c311, n6120);
    let n6123: ZN = zsel_n(n2793, n5804, n6121);
    let n6124: ZN = zsel_n(n1493, r_c311, n6122);
    let n6125: ZN = zsel_n(n1493, r_c321, n6123);
    let n6126: ZN = zsel_n(n4840, zn_splat(P8::from_raw(231700i32)), n5868);
    let n6127: ZN = zsel_n(n2793, n5804, n6126);
    let n6128: ZN = zsel_n(n1493, r_c321, n6127);
    let n6129: ZN = zsel_n(n4840, zn_splat(P8::from_raw(231700i32)), n5895);
    let n6130: ZN = zsel_n(n2793, n5804, n6129);
    let n6131: ZN = zsel_n(n1493, r_c321, n6130);
    let n6132: ZN = zsel_n(n4840, n4447, n5921);
    let n6133: ZN = zsel_n(n4840, zn_splat(P8::from_raw(0i32)), n5922);
    let n6134: ZN = zsel_n(n2793, n5799, n6132);
    let n6135: ZN = zsel_n(n2793, n5804, n6133);
    let n6136: ZB = zb_and(n4501, n5624);
    let n6137: ZN = zsel_n(n1493, r_c320, n6134);
    let n6138: ZN = zsel_n(n1493, r_c321, n6135);
    let n6139: ZB = zb_or(n1493, n6136);
    let n6140: ZB = zb_and(n6012, n6139);
    let n6141: ZB = zb_and(n6013, n6139);
    let n6142: ZB = zb_and(n5847, n6141);
    let n6143: ZB = zb_and(n5846, n6141);
    let n6144: ZB = zb_or(n6142, n6143);
    let n6145: ZB = zb_and(n5852, n6144);
    let n6146: ZB = zb_and(n5853, n6144);
    let n6147: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n6137);
    let n6148: ZB = zb_or(n6145, n6146);
    let n6149: ZN = zsel_n(n6012, n6137, n6147);
    let n6150: ZB = zb_or(n6140, n6148);
    let n6151: ZN = zsel_n(n4840, zn_splat(P8::from_raw(-327680i32)), n5942);
    let n6152: ZN = zsel_n(n4840, zn_splat(P8::from_raw(0i32)), n5943);
    let n6153: ZN = zsel_n(n2793, n5799, n6151);
    let n6154: ZN = zsel_n(n2793, n5804, n6152);
    let n6155: ZB = zb_and(n4501, n5660);
    let n6156: ZN = zsel_n(n1493, r_c320, n6153);
    let n6157: ZN = zsel_n(n1493, r_c321, n6154);
    let n6158: ZB = zb_or(n1493, n6155);
    let n6159: ZB = zb_and(n6012, n6158);
    let n6160: ZB = zb_and(n6013, n6158);
    let n6161: ZB = zb_and(n5847, n6160);
    let n6162: ZB = zb_and(n5846, n6160);
    let n6163: ZB = zb_or(n6161, n6162);
    let n6164: ZB = zb_and(n5852, n6163);
    let n6165: ZB = zb_and(n5853, n6163);
    let n6166: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n6156);
    let n6167: ZB = zb_or(n6164, n6165);
    let n6168: ZN = zsel_n(n6012, n6156, n6166);
    let n6169: ZB = zb_or(n6159, n6167);
    let n6170: ZN = zsel_n(n4840, zn_splat(P8::from_raw(327680i32)), n5961);
    let n6171: ZN = zsel_n(n4840, zn_splat(P8::from_raw(0i32)), n5962);
    let n6172: ZN = zsel_n(n2793, n5799, n6170);
    let n6173: ZN = zsel_n(n2793, n5804, n6171);
    let n6174: ZB = zb_and(n4501, n5696);
    let n6175: ZN = zsel_n(n1493, r_c320, n6172);
    let n6176: ZN = zsel_n(n1493, r_c321, n6173);
    let n6177: ZB = zb_or(n1493, n6174);
    let n6178: ZB = zb_and(n6012, n6177);
    let n6179: ZB = zb_and(n6013, n6177);
    let n6180: ZB = zb_and(n5847, n6179);
    let n6181: ZB = zb_and(n5846, n6179);
    let n6182: ZB = zb_or(n6180, n6181);
    let n6183: ZB = zb_and(n5852, n6182);
    let n6184: ZB = zb_and(n5853, n6182);
    let n6185: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n6175);
    let n6186: ZB = zb_or(n6183, n6184);
    let n6187: ZN = zsel_n(n6012, n6175, n6185);
    let n6188: ZB = zb_or(n6178, n6186);
    let n6189: ZN = zsel_n(n4840, zn_splat(P8::from_raw(0i32)), n5921);
    let n6190: ZN = zsel_n(n4840, zn_splat(P8::from_raw(-327680i32)), n5922);
    let n6191: ZN = zsel_n(n2793, n5799, n6189);
    let n6192: ZN = zsel_n(n2793, n5804, n6190);
    let n6193: ZB = zb_and(n4501, n5730);
    let n6194: ZN = zsel_n(n1493, r_c320, n6191);
    let n6195: ZN = zsel_n(n1493, r_c321, n6192);
    let n6196: ZB = zb_or(n1493, n6193);
    let n6197: ZB = zb_and(n6012, n6196);
    let n6198: ZB = zb_and(n6013, n6196);
    let n6199: ZB = zb_and(n5847, n6198);
    let n6200: ZB = zb_and(n5846, n6198);
    let n6201: ZB = zb_or(n6199, n6200);
    let n6202: ZB = zb_and(n5852, n6201);
    let n6203: ZB = zb_and(n5853, n6201);
    let n6204: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n6194);
    let n6205: ZB = zb_or(n6202, n6203);
    let n6206: ZN = zsel_n(n6012, n6194, n6204);
    let n6207: ZB = zb_or(n6197, n6205);
    let n6208: ZN = zsel_n(n4840, zn_splat(P8::from_raw(-231700i32)), n5942);
    let n6209: ZN = zsel_n(n4840, zn_splat(P8::from_raw(-231700i32)), n5943);
    let n6210: ZN = zsel_n(n2793, n5799, n6208);
    let n6211: ZN = zsel_n(n2793, n5804, n6209);
    let n6212: ZN = zsel_n(n1493, r_c320, n6210);
    let n6213: ZN = zsel_n(n1493, r_c321, n6211);
    let n6214: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n6212);
    let n6215: ZN = zsel_n(n6012, n6212, n6214);
    let n6216: ZN = zsel_n(n4840, zn_splat(P8::from_raw(231700i32)), n5961);
    let n6217: ZN = zsel_n(n4840, zn_splat(P8::from_raw(-231700i32)), n5962);
    let n6218: ZN = zsel_n(n2793, n5799, n6216);
    let n6219: ZN = zsel_n(n2793, n5804, n6217);
    let n6220: ZN = zsel_n(n1493, r_c320, n6218);
    let n6221: ZN = zsel_n(n1493, r_c321, n6219);
    let n6222: ZN = zsel_n(n5852, zn_splat(P8::from_raw(0i32)), n6220);
    let n6223: ZN = zsel_n(n6012, n6220, n6222);
    let n6224: ZN = zsel_n(n4840, zn_splat(P8::from_raw(327680i32)), n5922);
    let n6225: ZN = zsel_n(n2793, n5804, n6224);
    let n6226: ZN = zsel_n(n1493, r_c321, n6225);
    let n6227: ZN = zsel_n(n4840, zn_splat(P8::from_raw(231700i32)), n5943);
    let n6228: ZN = zsel_n(n2793, n5804, n6227);
    let n6229: ZN = zsel_n(n1493, r_c321, n6228);
    let n6230: ZN = zsel_n(n4840, zn_splat(P8::from_raw(231700i32)), n5962);
    let n6231: ZN = zsel_n(n2793, n5804, n6230);
    let n6232: ZN = zsel_n(n1493, r_c321, n6231);
    let n6235: ZW = zw_bits_n(r_c39);
    let n6236: ZW = zw_mix1(zw_splat(11400714819323198485u64), n6235, 39u64);
    let n6237: ZW = zw_mix2(zw_splat(11562461410679940143u64), n6235, 39u64);
    let n6238: ZW = zw_bits_n(n90);
    let n6239: ZW = zw_mix1(n6236, n6238, 84u64);
    let n6240: ZW = zw_mix2(n6237, n6238, 84u64);
    let n6241: ZW = zw_bits_n(n145);
    let n6242: ZW = zw_mix1(n6239, n6241, 85u64);
    let n6243: ZW = zw_mix2(n6240, n6241, 85u64);
    let n6244: ZW = zw_bits_n(n144);
    let n6245: ZW = zw_mix1(n6242, n6244, 86u64);
    let n6246: ZW = zw_mix2(n6243, n6244, 86u64);
    let n6247: ZW = zw_bits_n(r_c87);
    let n6248: ZW = zw_mix1(n6245, n6247, 87u64);
    let n6249: ZW = zw_mix2(n6246, n6247, 87u64);
    let n6250: ZW = zw_bits_n(n688);
    let n6251: ZW = zw_mix1(n6248, n6250, 256u64);
    let n6252: ZW = zw_mix2(n6249, n6250, 256u64);
    let n6253: ZW = zw_bits_n(n463);
    let n6254: ZW = zw_mix1(n6251, n6253, 280u64);
    let n6255: ZW = zw_mix2(n6252, n6253, 280u64);
    let n6256: ZW = zw_bits_n(n689);
    let n6257: ZW = zw_mix1(n6254, n6256, 281u64);
    let n6258: ZW = zw_mix2(n6255, n6256, 281u64);
    let n6259: ZW = zw_bits_n(r_c20);
    let n6260: ZW = zw_mix1(n6257, n6259, 20u64);
    let n6261: ZW = zw_mix2(n6258, n6259, 20u64);
    let n6262: ZW = zw_bits_b(r_c41);
    let n6263: ZW = zw_mix1(n6260, n6262, 41u64);
    let n6264: ZW = zw_mix2(n6261, n6262, 41u64);
    let n6265: ZW = zw_bits_n(n249);
    let n6266: ZW = zw_mix1(n6263, n6265, 236u64);
    let n6267: ZW = zw_mix2(n6264, n6265, 236u64);
    let n6268: u64 = P8::from_raw(-65536i32).as_raw_u32() as u64;
    let n6269: ZW = zw_mix1(n6266, zw_splat(n6268), 238u64);
    let n6270: ZW = zw_mix2(n6267, zw_splat(n6268), 238u64);
    let n6271: ZW = zw_bits_n(n1358);
    let n6272: ZW = zw_mix1(n6269, n6271, 241u64);
    let n6273: ZW = zw_mix2(n6270, n6271, 241u64);
    let n6274: u64 = false as u64;
    let n6275: ZW = zw_mix1(n6272, zw_splat(n6274), 248u64);
    let n6276: ZW = zw_mix2(n6273, zw_splat(n6274), 248u64);
    let n6277: ZW = zw_mix1(n6275, zw_splat(n6274), 249u64);
    let n6278: ZW = zw_mix2(n6276, zw_splat(n6274), 249u64);
    let n6279: ZW = zw_bits_n(n1510);
    let n6280: ZW = zw_mix1(n6277, n6279, 255u64);
    let n6281: ZW = zw_mix2(n6278, n6279, 255u64);
    let n6282: ZW = zw_bits_n(r_c308);
    let n6283: ZW = zw_mix1(n6280, n6282, 270u64);
    let n6284: ZW = zw_mix2(n6281, n6282, 270u64);
    let n6285: ZW = zw_bits_n(r_c309);
    let n6286: ZW = zw_mix1(n6283, n6285, 271u64);
    let n6287: ZW = zw_mix2(n6284, n6285, 271u64);
    let n6288: ZW = zw_bits_n(r_c310);
    let n6289: ZW = zw_mix1(n6286, n6288, 272u64);
    let n6290: ZW = zw_mix2(n6287, n6288, 272u64);
    let n6291: ZW = zw_bits_n(r_c311);
    let n6292: ZW = zw_mix1(n6289, n6291, 273u64);
    let n6293: ZW = zw_mix2(n6290, n6291, 273u64);
    let n6294: ZW = zw_bits_b(n1416);
    let n6295: ZW = zw_mix1(n6292, n6294, 274u64);
    let n6296: ZW = zw_mix2(n6293, n6294, 274u64);
    let n6297: ZW = zw_bits_n(n1511);
    let n6298: ZW = zw_mix1(n6295, n6297, 282u64);
    let n6299: ZW = zw_mix2(n6296, n6297, 282u64);
    let n6300: ZW = zw_bits_n(n1430);
    let n6301: ZW = zw_mix1(n6298, n6300, 283u64);
    let n6302: ZW = zw_mix2(n6299, n6300, 283u64);
    let n6303: ZW = zw_bits_b(n1537);
    let n6304: ZW = zw_mix1(n6292, n6303, 274u64);
    let n6305: ZW = zw_mix2(n6293, n6303, 274u64);
    let n6306: ZW = zw_bits_n(n1592);
    let n6307: ZW = zw_mix1(n6304, n6306, 282u64);
    let n6308: ZW = zw_mix2(n6305, n6306, 282u64);
    let n6309: ZW = zw_bits_n(n1561);
    let n6310: ZW = zw_mix1(n6307, n6309, 283u64);
    let n6311: ZW = zw_mix2(n6308, n6309, 283u64);
    let n6312: ZW = zw_bits_b(n1606);
    let n6313: ZW = zw_mix1(n6292, n6312, 274u64);
    let n6314: ZW = zw_mix2(n6293, n6312, 274u64);
    let n6315: ZW = zw_bits_n(n1661);
    let n6316: ZW = zw_mix1(n6313, n6315, 282u64);
    let n6317: ZW = zw_mix2(n6314, n6315, 282u64);
    let n6318: ZW = zw_bits_n(n1630);
    let n6319: ZW = zw_mix1(n6316, n6318, 283u64);
    let n6320: ZW = zw_mix2(n6317, n6318, 283u64);
    let n6321: ZW = zw_bits_n(n1688);
    let n6322: ZW = zw_mix1(n6269, n6321, 241u64);
    let n6323: ZW = zw_mix2(n6270, n6321, 241u64);
    let n6324: ZW = zw_mix1(n6322, zw_splat(n6274), 248u64);
    let n6325: ZW = zw_mix2(n6323, zw_splat(n6274), 248u64);
    let n6326: u64 = true as u64;
    let n6327: ZW = zw_mix1(n6324, zw_splat(n6326), 249u64);
    let n6328: ZW = zw_mix2(n6325, zw_splat(n6326), 249u64);
    let n6329: ZW = zw_mix1(n6327, n6279, 255u64);
    let n6330: ZW = zw_mix2(n6328, n6279, 255u64);
    let n6331: ZW = zw_mix1(n6329, n6282, 270u64);
    let n6332: ZW = zw_mix2(n6330, n6282, 270u64);
    let n6333: ZW = zw_mix1(n6331, n6285, 271u64);
    let n6334: ZW = zw_mix2(n6332, n6285, 271u64);
    let n6335: ZW = zw_mix1(n6333, n6288, 272u64);
    let n6336: ZW = zw_mix2(n6334, n6288, 272u64);
    let n6337: ZW = zw_mix1(n6335, n6291, 273u64);
    let n6338: ZW = zw_mix2(n6336, n6291, 273u64);
    let n6339: ZW = zw_mix1(n6337, n6294, 274u64);
    let n6340: ZW = zw_mix2(n6338, n6294, 274u64);
    let n6341: ZW = zw_bits_n(n1717);
    let n6342: ZW = zw_mix1(n6339, n6341, 282u64);
    let n6343: ZW = zw_mix2(n6340, n6341, 282u64);
    let n6344: ZW = zw_bits_n(n1690);
    let n6345: ZW = zw_mix1(n6342, n6344, 283u64);
    let n6346: ZW = zw_mix2(n6343, n6344, 283u64);
    let n6347: ZW = zw_mix1(n6337, n6303, 274u64);
    let n6348: ZW = zw_mix2(n6338, n6303, 274u64);
    let n6349: ZW = zw_bits_n(n1772);
    let n6350: ZW = zw_mix1(n6347, n6349, 282u64);
    let n6351: ZW = zw_mix2(n6348, n6349, 282u64);
    let n6352: ZW = zw_bits_n(n1745);
    let n6353: ZW = zw_mix1(n6350, n6352, 283u64);
    let n6354: ZW = zw_mix2(n6351, n6352, 283u64);
    let n6355: ZW = zw_mix1(n6337, n6312, 274u64);
    let n6356: ZW = zw_mix2(n6338, n6312, 274u64);
    let n6357: ZW = zw_bits_n(n1827);
    let n6358: ZW = zw_mix1(n6355, n6357, 282u64);
    let n6359: ZW = zw_mix2(n6356, n6357, 282u64);
    let n6360: ZW = zw_bits_n(n1800);
    let n6361: ZW = zw_mix1(n6358, n6360, 283u64);
    let n6362: ZW = zw_mix2(n6359, n6360, 283u64);
    let n6363: ZW = zw_bits_n(n1844);
    let n6364: ZW = zw_mix1(n6257, n6363, 20u64);
    let n6365: ZW = zw_mix2(n6258, n6363, 20u64);
    let n6366: ZW = zw_bits_b(n1845);
    let n6367: ZW = zw_mix1(n6364, n6366, 41u64);
    let n6368: ZW = zw_mix2(n6365, n6366, 41u64);
    let n6369: ZW = zw_bits_n(n1846);
    let n6370: ZW = zw_mix1(n6367, n6369, 236u64);
    let n6371: ZW = zw_mix2(n6368, n6369, 236u64);
    let n6372: ZW = zw_bits_n(n1847);
    let n6373: ZW = zw_mix1(n6370, n6372, 238u64);
    let n6374: ZW = zw_mix2(n6371, n6372, 238u64);
    let n6375: ZW = zw_mix1(n6373, n6271, 241u64);
    let n6376: ZW = zw_mix2(n6374, n6271, 241u64);
    let n6377: ZW = zw_mix1(n6375, zw_splat(n6326), 248u64);
    let n6378: ZW = zw_mix2(n6376, zw_splat(n6326), 248u64);
    let n6379: ZW = zw_mix1(n6377, zw_splat(n6274), 249u64);
    let n6380: ZW = zw_mix2(n6378, zw_splat(n6274), 249u64);
    let n6381: ZW = zw_bits_n(n1880);
    let n6382: ZW = zw_mix1(n6379, n6381, 255u64);
    let n6383: ZW = zw_mix2(n6380, n6381, 255u64);
    let n6384: ZW = zw_bits_n(n1848);
    let n6385: ZW = zw_mix1(n6382, n6384, 270u64);
    let n6386: ZW = zw_mix2(n6383, n6384, 270u64);
    let n6387: ZW = zw_bits_n(n1849);
    let n6388: ZW = zw_mix1(n6385, n6387, 271u64);
    let n6389: ZW = zw_mix2(n6386, n6387, 271u64);
    let n6390: ZW = zw_bits_n(n1850);
    let n6391: ZW = zw_mix1(n6388, n6390, 272u64);
    let n6392: ZW = zw_mix2(n6389, n6390, 272u64);
    let n6393: ZW = zw_bits_n(n1851);
    let n6394: ZW = zw_mix1(n6391, n6393, 273u64);
    let n6395: ZW = zw_mix2(n6392, n6393, 273u64);
    let n6396: ZW = zw_mix1(n6394, n6294, 274u64);
    let n6397: ZW = zw_mix2(n6395, n6294, 274u64);
    let n6398: ZW = zw_bits_n(n1881);
    let n6399: ZW = zw_mix1(n6396, n6398, 282u64);
    let n6400: ZW = zw_mix2(n6397, n6398, 282u64);
    let n6401: ZW = zw_bits_n(n1853);
    let n6402: ZW = zw_mix1(n6399, n6401, 283u64);
    let n6403: ZW = zw_mix2(n6400, n6401, 283u64);
    let n6404: ZW = zw_bits_n(n1885);
    let n6405: ZW = zw_mix1(n6385, n6404, 271u64);
    let n6406: ZW = zw_mix2(n6386, n6404, 271u64);
    let n6407: ZW = zw_bits_n(n1886);
    let n6408: ZW = zw_mix1(n6405, n6407, 272u64);
    let n6409: ZW = zw_mix2(n6406, n6407, 272u64);
    let n6410: ZW = zw_mix1(n6408, n6393, 273u64);
    let n6411: ZW = zw_mix2(n6409, n6393, 273u64);
    let n6412: ZW = zw_mix1(n6410, n6303, 274u64);
    let n6413: ZW = zw_mix2(n6411, n6303, 274u64);
    let n6414: ZW = zw_bits_n(n1913);
    let n6415: ZW = zw_mix1(n6412, n6414, 282u64);
    let n6416: ZW = zw_mix2(n6413, n6414, 282u64);
    let n6417: ZW = zw_bits_n(n1888);
    let n6418: ZW = zw_mix1(n6415, n6417, 283u64);
    let n6419: ZW = zw_mix2(n6416, n6417, 283u64);
    let n6420: ZW = zw_bits_n(n1917);
    let n6421: ZW = zw_mix1(n6405, n6420, 272u64);
    let n6422: ZW = zw_mix2(n6406, n6420, 272u64);
    let n6423: ZW = zw_mix1(n6421, n6393, 273u64);
    let n6424: ZW = zw_mix2(n6422, n6393, 273u64);
    let n6425: ZW = zw_mix1(n6423, n6312, 274u64);
    let n6426: ZW = zw_mix2(n6424, n6312, 274u64);
    let n6427: ZW = zw_bits_n(n1944);
    let n6428: ZW = zw_mix1(n6425, n6427, 282u64);
    let n6429: ZW = zw_mix2(n6426, n6427, 282u64);
    let n6430: ZW = zw_bits_n(n1919);
    let n6431: ZW = zw_mix1(n6428, n6430, 283u64);
    let n6432: ZW = zw_mix2(n6429, n6430, 283u64);
    let n6433: ZW = zw_bits_n(n1946);
    let n6434: ZW = zw_mix1(n6382, n6433, 270u64);
    let n6435: ZW = zw_mix2(n6383, n6433, 270u64);
    let n6436: ZW = zw_bits_n(n1947);
    let n6437: ZW = zw_mix1(n6434, n6436, 271u64);
    let n6438: ZW = zw_mix2(n6435, n6436, 271u64);
    let n6439: ZW = zw_bits_n(n1948);
    let n6440: ZW = zw_mix1(n6437, n6439, 272u64);
    let n6441: ZW = zw_mix2(n6438, n6439, 272u64);
    let n6442: ZW = zw_bits_n(n1949);
    let n6443: ZW = zw_mix1(n6440, n6442, 273u64);
    let n6444: ZW = zw_mix2(n6441, n6442, 273u64);
    let n6445: ZW = zw_mix1(n6443, n6294, 274u64);
    let n6446: ZW = zw_mix2(n6444, n6294, 274u64);
    let n6447: ZW = zw_bits_n(n1976);
    let n6448: ZW = zw_mix1(n6445, n6447, 282u64);
    let n6449: ZW = zw_mix2(n6446, n6447, 282u64);
    let n6450: ZW = zw_bits_n(n1951);
    let n6451: ZW = zw_mix1(n6448, n6450, 283u64);
    let n6452: ZW = zw_mix2(n6449, n6450, 283u64);
    let n6453: ZW = zw_mix1(n6434, n6404, 271u64);
    let n6454: ZW = zw_mix2(n6435, n6404, 271u64);
    let n6455: ZW = zw_mix1(n6453, n6407, 272u64);
    let n6456: ZW = zw_mix2(n6454, n6407, 272u64);
    let n6457: ZW = zw_mix1(n6455, n6442, 273u64);
    let n6458: ZW = zw_mix2(n6456, n6442, 273u64);
    let n6459: ZW = zw_mix1(n6457, n6303, 274u64);
    let n6460: ZW = zw_mix2(n6458, n6303, 274u64);
    let n6461: ZW = zw_bits_n(n1981);
    let n6462: ZW = zw_mix1(n6459, n6461, 282u64);
    let n6463: ZW = zw_mix2(n6460, n6461, 282u64);
    let n6464: ZW = zw_bits_n(n1979);
    let n6465: ZW = zw_mix1(n6462, n6464, 283u64);
    let n6466: ZW = zw_mix2(n6463, n6464, 283u64);
    let n6467: ZW = zw_mix1(n6453, n6420, 272u64);
    let n6468: ZW = zw_mix2(n6454, n6420, 272u64);
    let n6469: ZW = zw_mix1(n6467, n6442, 273u64);
    let n6470: ZW = zw_mix2(n6468, n6442, 273u64);
    let n6471: ZW = zw_mix1(n6469, n6312, 274u64);
    let n6472: ZW = zw_mix2(n6470, n6312, 274u64);
    let n6473: ZW = zw_bits_n(n1985);
    let n6474: ZW = zw_mix1(n6471, n6473, 282u64);
    let n6475: ZW = zw_mix2(n6472, n6473, 282u64);
    let n6476: ZW = zw_bits_n(n1983);
    let n6477: ZW = zw_mix1(n6474, n6476, 283u64);
    let n6478: ZW = zw_mix2(n6475, n6476, 283u64);
    let n6479: ZW = zw_bits_n(n1986);
    let n6480: ZW = zw_mix1(n6440, n6479, 273u64);
    let n6481: ZW = zw_mix2(n6441, n6479, 273u64);
    let n6482: ZW = zw_mix1(n6480, n6294, 274u64);
    let n6483: ZW = zw_mix2(n6481, n6294, 274u64);
    let n6484: ZW = zw_mix1(n6482, n6447, 282u64);
    let n6485: ZW = zw_mix2(n6483, n6447, 282u64);
    let n6486: ZW = zw_bits_n(n1987);
    let n6487: ZW = zw_mix1(n6484, n6486, 283u64);
    let n6488: ZW = zw_mix2(n6485, n6486, 283u64);
    let n6489: ZW = zw_mix1(n6455, n6479, 273u64);
    let n6490: ZW = zw_mix2(n6456, n6479, 273u64);
    let n6491: ZW = zw_mix1(n6489, n6303, 274u64);
    let n6492: ZW = zw_mix2(n6490, n6303, 274u64);
    let n6493: ZW = zw_mix1(n6491, n6461, 282u64);
    let n6494: ZW = zw_mix2(n6492, n6461, 282u64);
    let n6495: ZW = zw_bits_n(n1988);
    let n6496: ZW = zw_mix1(n6493, n6495, 283u64);
    let n6497: ZW = zw_mix2(n6494, n6495, 283u64);
    let n6498: ZW = zw_mix1(n6467, n6479, 273u64);
    let n6499: ZW = zw_mix2(n6468, n6479, 273u64);
    let n6500: ZW = zw_mix1(n6498, n6312, 274u64);
    let n6501: ZW = zw_mix2(n6499, n6312, 274u64);
    let n6502: ZW = zw_mix1(n6500, n6473, 282u64);
    let n6503: ZW = zw_mix2(n6501, n6473, 282u64);
    let n6504: ZW = zw_bits_n(n1989);
    let n6505: ZW = zw_mix1(n6502, n6504, 283u64);
    let n6506: ZW = zw_mix2(n6503, n6504, 283u64);
    let n6507: ZW = zw_mix1(n6373, n6321, 241u64);
    let n6508: ZW = zw_mix2(n6374, n6321, 241u64);
    let n6509: ZW = zw_mix1(n6507, zw_splat(n6326), 248u64);
    let n6510: ZW = zw_mix2(n6508, zw_splat(n6326), 248u64);
    let n6511: ZW = zw_mix1(n6509, zw_splat(n6326), 249u64);
    let n6512: ZW = zw_mix2(n6510, zw_splat(n6326), 249u64);
    let n6513: ZW = zw_mix1(n6511, n6381, 255u64);
    let n6514: ZW = zw_mix2(n6512, n6381, 255u64);
    let n6515: ZW = zw_mix1(n6513, n6384, 270u64);
    let n6516: ZW = zw_mix2(n6514, n6384, 270u64);
    let n6517: ZW = zw_mix1(n6515, n6387, 271u64);
    let n6518: ZW = zw_mix2(n6516, n6387, 271u64);
    let n6519: ZW = zw_mix1(n6517, n6390, 272u64);
    let n6520: ZW = zw_mix2(n6518, n6390, 272u64);
    let n6521: ZW = zw_mix1(n6519, n6393, 273u64);
    let n6522: ZW = zw_mix2(n6520, n6393, 273u64);
    let n6523: ZW = zw_mix1(n6521, n6294, 274u64);
    let n6524: ZW = zw_mix2(n6522, n6294, 274u64);
    let n6525: ZW = zw_bits_n(n2030);
    let n6526: ZW = zw_mix1(n6523, n6525, 282u64);
    let n6527: ZW = zw_mix2(n6524, n6525, 282u64);
    let n6528: ZW = zw_bits_n(n2005);
    let n6529: ZW = zw_mix1(n6526, n6528, 283u64);
    let n6530: ZW = zw_mix2(n6527, n6528, 283u64);
    let n6531: ZW = zw_mix1(n6515, n6404, 271u64);
    let n6532: ZW = zw_mix2(n6516, n6404, 271u64);
    let n6533: ZW = zw_mix1(n6531, n6407, 272u64);
    let n6534: ZW = zw_mix2(n6532, n6407, 272u64);
    let n6535: ZW = zw_mix1(n6533, n6393, 273u64);
    let n6536: ZW = zw_mix2(n6534, n6393, 273u64);
    let n6537: ZW = zw_mix1(n6535, n6303, 274u64);
    let n6538: ZW = zw_mix2(n6536, n6303, 274u64);
    let n6539: ZW = zw_bits_n(n2060);
    let n6540: ZW = zw_mix1(n6537, n6539, 282u64);
    let n6541: ZW = zw_mix2(n6538, n6539, 282u64);
    let n6542: ZW = zw_bits_n(n2035);
    let n6543: ZW = zw_mix1(n6540, n6542, 283u64);
    let n6544: ZW = zw_mix2(n6541, n6542, 283u64);
    let n6545: ZW = zw_mix1(n6531, n6420, 272u64);
    let n6546: ZW = zw_mix2(n6532, n6420, 272u64);
    let n6547: ZW = zw_mix1(n6545, n6393, 273u64);
    let n6548: ZW = zw_mix2(n6546, n6393, 273u64);
    let n6549: ZW = zw_mix1(n6547, n6312, 274u64);
    let n6550: ZW = zw_mix2(n6548, n6312, 274u64);
    let n6551: ZW = zw_bits_n(n2090);
    let n6552: ZW = zw_mix1(n6549, n6551, 282u64);
    let n6553: ZW = zw_mix2(n6550, n6551, 282u64);
    let n6554: ZW = zw_bits_n(n2065);
    let n6555: ZW = zw_mix1(n6552, n6554, 283u64);
    let n6556: ZW = zw_mix2(n6553, n6554, 283u64);
    let n6557: ZW = zw_mix1(n6513, n6433, 270u64);
    let n6558: ZW = zw_mix2(n6514, n6433, 270u64);
    let n6559: ZW = zw_mix1(n6557, n6436, 271u64);
    let n6560: ZW = zw_mix2(n6558, n6436, 271u64);
    let n6561: ZW = zw_mix1(n6559, n6439, 272u64);
    let n6562: ZW = zw_mix2(n6560, n6439, 272u64);
    let n6563: ZW = zw_mix1(n6561, n6442, 273u64);
    let n6564: ZW = zw_mix2(n6562, n6442, 273u64);
    let n6565: ZW = zw_mix1(n6563, n6294, 274u64);
    let n6566: ZW = zw_mix2(n6564, n6294, 274u64);
    let n6567: ZW = zw_bits_n(n2118);
    let n6568: ZW = zw_mix1(n6565, n6567, 282u64);
    let n6569: ZW = zw_mix2(n6566, n6567, 282u64);
    let n6570: ZW = zw_bits_n(n2093);
    let n6571: ZW = zw_mix1(n6568, n6570, 283u64);
    let n6572: ZW = zw_mix2(n6569, n6570, 283u64);
    let n6573: ZW = zw_mix1(n6557, n6404, 271u64);
    let n6574: ZW = zw_mix2(n6558, n6404, 271u64);
    let n6575: ZW = zw_mix1(n6573, n6407, 272u64);
    let n6576: ZW = zw_mix2(n6574, n6407, 272u64);
    let n6577: ZW = zw_mix1(n6575, n6442, 273u64);
    let n6578: ZW = zw_mix2(n6576, n6442, 273u64);
    let n6579: ZW = zw_mix1(n6577, n6303, 274u64);
    let n6580: ZW = zw_mix2(n6578, n6303, 274u64);
    let n6581: ZW = zw_bits_n(n2123);
    let n6582: ZW = zw_mix1(n6579, n6581, 282u64);
    let n6583: ZW = zw_mix2(n6580, n6581, 282u64);
    let n6584: ZW = zw_bits_n(n2121);
    let n6585: ZW = zw_mix1(n6582, n6584, 283u64);
    let n6586: ZW = zw_mix2(n6583, n6584, 283u64);
    let n6587: ZW = zw_mix1(n6573, n6420, 272u64);
    let n6588: ZW = zw_mix2(n6574, n6420, 272u64);
    let n6589: ZW = zw_mix1(n6587, n6442, 273u64);
    let n6590: ZW = zw_mix2(n6588, n6442, 273u64);
    let n6591: ZW = zw_mix1(n6589, n6312, 274u64);
    let n6592: ZW = zw_mix2(n6590, n6312, 274u64);
    let n6593: ZW = zw_bits_n(n2127);
    let n6594: ZW = zw_mix1(n6591, n6593, 282u64);
    let n6595: ZW = zw_mix2(n6592, n6593, 282u64);
    let n6596: ZW = zw_bits_n(n2125);
    let n6597: ZW = zw_mix1(n6594, n6596, 283u64);
    let n6598: ZW = zw_mix2(n6595, n6596, 283u64);
    let n6599: ZW = zw_mix1(n6561, n6479, 273u64);
    let n6600: ZW = zw_mix2(n6562, n6479, 273u64);
    let n6601: ZW = zw_mix1(n6599, n6294, 274u64);
    let n6602: ZW = zw_mix2(n6600, n6294, 274u64);
    let n6603: ZW = zw_mix1(n6601, n6567, 282u64);
    let n6604: ZW = zw_mix2(n6602, n6567, 282u64);
    let n6605: ZW = zw_bits_n(n2128);
    let n6606: ZW = zw_mix1(n6603, n6605, 283u64);
    let n6607: ZW = zw_mix2(n6604, n6605, 283u64);
    let n6608: ZW = zw_mix1(n6575, n6479, 273u64);
    let n6609: ZW = zw_mix2(n6576, n6479, 273u64);
    let n6610: ZW = zw_mix1(n6608, n6303, 274u64);
    let n6611: ZW = zw_mix2(n6609, n6303, 274u64);
    let n6612: ZW = zw_mix1(n6610, n6581, 282u64);
    let n6613: ZW = zw_mix2(n6611, n6581, 282u64);
    let n6614: ZW = zw_bits_n(n2129);
    let n6615: ZW = zw_mix1(n6612, n6614, 283u64);
    let n6616: ZW = zw_mix2(n6613, n6614, 283u64);
    let n6617: ZW = zw_mix1(n6587, n6479, 273u64);
    let n6618: ZW = zw_mix2(n6588, n6479, 273u64);
    let n6619: ZW = zw_mix1(n6617, n6312, 274u64);
    let n6620: ZW = zw_mix2(n6618, n6312, 274u64);
    let n6621: ZW = zw_mix1(n6619, n6593, 282u64);
    let n6622: ZW = zw_mix2(n6620, n6593, 282u64);
    let n6623: ZW = zw_bits_n(n2130);
    let n6624: ZW = zw_mix1(n6621, n6623, 283u64);
    let n6625: ZW = zw_mix2(n6622, n6623, 283u64);
    let n6626: ZW = zw_mix1(n6251, n6253, 310u64);
    let n6627: ZW = zw_mix2(n6252, n6253, 310u64);
    let n6628: ZW = zw_mix1(n6626, n6256, 311u64);
    let n6629: ZW = zw_mix2(n6627, n6256, 311u64);
    let n6630: ZW = zw_mix1(n6628, n6259, 20u64);
    let n6631: ZW = zw_mix2(n6629, n6259, 20u64);
    let n6632: ZW = zw_mix1(n6630, n6262, 41u64);
    let n6633: ZW = zw_mix2(n6631, n6262, 41u64);
    let n6634: ZW = zw_mix1(n6632, n6265, 236u64);
    let n6635: ZW = zw_mix2(n6633, n6265, 236u64);
    let n6636: ZW = zw_mix1(n6634, zw_splat(n6268), 238u64);
    let n6637: ZW = zw_mix2(n6635, zw_splat(n6268), 238u64);
    let n6638: ZW = zw_bits_n(n1357);
    let n6639: ZW = zw_mix1(n6636, n6638, 239u64);
    let n6640: ZW = zw_mix2(n6637, n6638, 239u64);
    let n6641: ZW = zw_mix1(n6639, n6271, 241u64);
    let n6642: ZW = zw_mix2(n6640, n6271, 241u64);
    let n6643: ZW = zw_mix1(n6641, zw_splat(n6274), 248u64);
    let n6644: ZW = zw_mix2(n6642, zw_splat(n6274), 248u64);
    let n6645: ZW = zw_mix1(n6643, zw_splat(n6274), 249u64);
    let n6646: ZW = zw_mix2(n6644, zw_splat(n6274), 249u64);
    let n6647: ZW = zw_mix1(n6645, n6279, 255u64);
    let n6648: ZW = zw_mix2(n6646, n6279, 255u64);
    let n6649: ZW = zw_mix1(n6647, n6282, 300u64);
    let n6650: ZW = zw_mix2(n6648, n6282, 300u64);
    let n6651: ZW = zw_mix1(n6649, n6285, 301u64);
    let n6652: ZW = zw_mix2(n6650, n6285, 301u64);
    let n6653: ZW = zw_mix1(n6651, n6288, 302u64);
    let n6654: ZW = zw_mix2(n6652, n6288, 302u64);
    let n6655: ZW = zw_mix1(n6653, n6291, 303u64);
    let n6656: ZW = zw_mix2(n6654, n6291, 303u64);
    let n6657: ZW = zw_mix1(n6655, n6294, 304u64);
    let n6658: ZW = zw_mix2(n6656, n6294, 304u64);
    let n6659: ZW = zw_mix1(n6657, n6297, 312u64);
    let n6660: ZW = zw_mix2(n6658, n6297, 312u64);
    let n6661: ZW = zw_mix1(n6659, n6300, 313u64);
    let n6662: ZW = zw_mix2(n6660, n6300, 313u64);
    let n6663: ZW = zw_mix1(n6655, n6303, 304u64);
    let n6664: ZW = zw_mix2(n6656, n6303, 304u64);
    let n6665: ZW = zw_mix1(n6663, n6306, 312u64);
    let n6666: ZW = zw_mix2(n6664, n6306, 312u64);
    let n6667: ZW = zw_mix1(n6665, n6309, 313u64);
    let n6668: ZW = zw_mix2(n6666, n6309, 313u64);
    let n6669: ZW = zw_mix1(n6655, n6312, 304u64);
    let n6670: ZW = zw_mix2(n6656, n6312, 304u64);
    let n6671: ZW = zw_mix1(n6669, n6315, 312u64);
    let n6672: ZW = zw_mix2(n6670, n6315, 312u64);
    let n6673: ZW = zw_mix1(n6671, n6318, 313u64);
    let n6674: ZW = zw_mix2(n6672, n6318, 313u64);
    let n6675: ZW = zw_mix1(n6639, n6321, 241u64);
    let n6676: ZW = zw_mix2(n6640, n6321, 241u64);
    let n6677: ZW = zw_mix1(n6675, zw_splat(n6274), 248u64);
    let n6678: ZW = zw_mix2(n6676, zw_splat(n6274), 248u64);
    let n6679: ZW = zw_mix1(n6677, zw_splat(n6326), 249u64);
    let n6680: ZW = zw_mix2(n6678, zw_splat(n6326), 249u64);
    let n6681: ZW = zw_mix1(n6679, n6279, 255u64);
    let n6682: ZW = zw_mix2(n6680, n6279, 255u64);
    let n6683: ZW = zw_mix1(n6681, n6282, 300u64);
    let n6684: ZW = zw_mix2(n6682, n6282, 300u64);
    let n6685: ZW = zw_mix1(n6683, n6285, 301u64);
    let n6686: ZW = zw_mix2(n6684, n6285, 301u64);
    let n6687: ZW = zw_mix1(n6685, n6288, 302u64);
    let n6688: ZW = zw_mix2(n6686, n6288, 302u64);
    let n6689: ZW = zw_mix1(n6687, n6291, 303u64);
    let n6690: ZW = zw_mix2(n6688, n6291, 303u64);
    let n6691: ZW = zw_mix1(n6689, n6294, 304u64);
    let n6692: ZW = zw_mix2(n6690, n6294, 304u64);
    let n6693: ZW = zw_mix1(n6691, n6341, 312u64);
    let n6694: ZW = zw_mix2(n6692, n6341, 312u64);
    let n6695: ZW = zw_mix1(n6693, n6344, 313u64);
    let n6696: ZW = zw_mix2(n6694, n6344, 313u64);
    let n6697: ZW = zw_mix1(n6689, n6303, 304u64);
    let n6698: ZW = zw_mix2(n6690, n6303, 304u64);
    let n6699: ZW = zw_mix1(n6697, n6349, 312u64);
    let n6700: ZW = zw_mix2(n6698, n6349, 312u64);
    let n6701: ZW = zw_mix1(n6699, n6352, 313u64);
    let n6702: ZW = zw_mix2(n6700, n6352, 313u64);
    let n6703: ZW = zw_mix1(n6689, n6312, 304u64);
    let n6704: ZW = zw_mix2(n6690, n6312, 304u64);
    let n6705: ZW = zw_mix1(n6703, n6357, 312u64);
    let n6706: ZW = zw_mix2(n6704, n6357, 312u64);
    let n6707: ZW = zw_mix1(n6705, n6360, 313u64);
    let n6708: ZW = zw_mix2(n6706, n6360, 313u64);
    let n6709: ZW = zw_mix1(n6628, n6363, 20u64);
    let n6710: ZW = zw_mix2(n6629, n6363, 20u64);
    let n6711: ZW = zw_mix1(n6709, n6366, 41u64);
    let n6712: ZW = zw_mix2(n6710, n6366, 41u64);
    let n6713: ZW = zw_mix1(n6711, n6369, 236u64);
    let n6714: ZW = zw_mix2(n6712, n6369, 236u64);
    let n6715: ZW = zw_mix1(n6713, n6372, 238u64);
    let n6716: ZW = zw_mix2(n6714, n6372, 238u64);
    let n6717: ZW = zw_bits_n(n2182);
    let n6718: ZW = zw_mix1(n6715, n6717, 239u64);
    let n6719: ZW = zw_mix2(n6716, n6717, 239u64);
    let n6720: ZW = zw_mix1(n6718, n6271, 241u64);
    let n6721: ZW = zw_mix2(n6719, n6271, 241u64);
    let n6722: ZW = zw_mix1(n6720, zw_splat(n6326), 248u64);
    let n6723: ZW = zw_mix2(n6721, zw_splat(n6326), 248u64);
    let n6724: ZW = zw_mix1(n6722, zw_splat(n6274), 249u64);
    let n6725: ZW = zw_mix2(n6723, zw_splat(n6274), 249u64);
    let n6726: ZW = zw_mix1(n6724, n6381, 255u64);
    let n6727: ZW = zw_mix2(n6725, n6381, 255u64);
    let n6728: ZW = zw_mix1(n6726, n6384, 300u64);
    let n6729: ZW = zw_mix2(n6727, n6384, 300u64);
    let n6730: ZW = zw_mix1(n6728, n6387, 301u64);
    let n6731: ZW = zw_mix2(n6729, n6387, 301u64);
    let n6732: ZW = zw_mix1(n6730, n6390, 302u64);
    let n6733: ZW = zw_mix2(n6731, n6390, 302u64);
    let n6734: ZW = zw_mix1(n6732, n6393, 303u64);
    let n6735: ZW = zw_mix2(n6733, n6393, 303u64);
    let n6736: ZW = zw_mix1(n6734, n6294, 304u64);
    let n6737: ZW = zw_mix2(n6735, n6294, 304u64);
    let n6738: ZW = zw_mix1(n6736, n6398, 312u64);
    let n6739: ZW = zw_mix2(n6737, n6398, 312u64);
    let n6740: ZW = zw_mix1(n6738, n6401, 313u64);
    let n6741: ZW = zw_mix2(n6739, n6401, 313u64);
    let n6742: ZW = zw_mix1(n6728, n6404, 301u64);
    let n6743: ZW = zw_mix2(n6729, n6404, 301u64);
    let n6744: ZW = zw_mix1(n6742, n6407, 302u64);
    let n6745: ZW = zw_mix2(n6743, n6407, 302u64);
    let n6746: ZW = zw_mix1(n6744, n6393, 303u64);
    let n6747: ZW = zw_mix2(n6745, n6393, 303u64);
    let n6748: ZW = zw_mix1(n6746, n6303, 304u64);
    let n6749: ZW = zw_mix2(n6747, n6303, 304u64);
    let n6750: ZW = zw_mix1(n6748, n6414, 312u64);
    let n6751: ZW = zw_mix2(n6749, n6414, 312u64);
    let n6752: ZW = zw_mix1(n6750, n6417, 313u64);
    let n6753: ZW = zw_mix2(n6751, n6417, 313u64);
    let n6754: ZW = zw_mix1(n6742, n6420, 302u64);
    let n6755: ZW = zw_mix2(n6743, n6420, 302u64);
    let n6756: ZW = zw_mix1(n6754, n6393, 303u64);
    let n6757: ZW = zw_mix2(n6755, n6393, 303u64);
    let n6758: ZW = zw_mix1(n6756, n6312, 304u64);
    let n6759: ZW = zw_mix2(n6757, n6312, 304u64);
    let n6760: ZW = zw_mix1(n6758, n6427, 312u64);
    let n6761: ZW = zw_mix2(n6759, n6427, 312u64);
    let n6762: ZW = zw_mix1(n6760, n6430, 313u64);
    let n6763: ZW = zw_mix2(n6761, n6430, 313u64);
    let n6764: ZW = zw_mix1(n6726, n6433, 300u64);
    let n6765: ZW = zw_mix2(n6727, n6433, 300u64);
    let n6766: ZW = zw_mix1(n6764, n6436, 301u64);
    let n6767: ZW = zw_mix2(n6765, n6436, 301u64);
    let n6768: ZW = zw_mix1(n6766, n6439, 302u64);
    let n6769: ZW = zw_mix2(n6767, n6439, 302u64);
    let n6770: ZW = zw_mix1(n6768, n6442, 303u64);
    let n6771: ZW = zw_mix2(n6769, n6442, 303u64);
    let n6772: ZW = zw_mix1(n6770, n6294, 304u64);
    let n6773: ZW = zw_mix2(n6771, n6294, 304u64);
    let n6774: ZW = zw_mix1(n6772, n6447, 312u64);
    let n6775: ZW = zw_mix2(n6773, n6447, 312u64);
    let n6776: ZW = zw_mix1(n6774, n6450, 313u64);
    let n6777: ZW = zw_mix2(n6775, n6450, 313u64);
    let n6778: ZW = zw_mix1(n6764, n6404, 301u64);
    let n6779: ZW = zw_mix2(n6765, n6404, 301u64);
    let n6780: ZW = zw_mix1(n6778, n6407, 302u64);
    let n6781: ZW = zw_mix2(n6779, n6407, 302u64);
    let n6782: ZW = zw_mix1(n6780, n6442, 303u64);
    let n6783: ZW = zw_mix2(n6781, n6442, 303u64);
    let n6784: ZW = zw_mix1(n6782, n6303, 304u64);
    let n6785: ZW = zw_mix2(n6783, n6303, 304u64);
    let n6786: ZW = zw_mix1(n6784, n6461, 312u64);
    let n6787: ZW = zw_mix2(n6785, n6461, 312u64);
    let n6788: ZW = zw_mix1(n6786, n6464, 313u64);
    let n6789: ZW = zw_mix2(n6787, n6464, 313u64);
    let n6790: ZW = zw_mix1(n6778, n6420, 302u64);
    let n6791: ZW = zw_mix2(n6779, n6420, 302u64);
    let n6792: ZW = zw_mix1(n6790, n6442, 303u64);
    let n6793: ZW = zw_mix2(n6791, n6442, 303u64);
    let n6794: ZW = zw_mix1(n6792, n6312, 304u64);
    let n6795: ZW = zw_mix2(n6793, n6312, 304u64);
    let n6796: ZW = zw_mix1(n6794, n6473, 312u64);
    let n6797: ZW = zw_mix2(n6795, n6473, 312u64);
    let n6798: ZW = zw_mix1(n6796, n6476, 313u64);
    let n6799: ZW = zw_mix2(n6797, n6476, 313u64);
    let n6800: ZW = zw_mix1(n6768, n6479, 303u64);
    let n6801: ZW = zw_mix2(n6769, n6479, 303u64);
    let n6802: ZW = zw_mix1(n6800, n6294, 304u64);
    let n6803: ZW = zw_mix2(n6801, n6294, 304u64);
    let n6804: ZW = zw_mix1(n6802, n6447, 312u64);
    let n6805: ZW = zw_mix2(n6803, n6447, 312u64);
    let n6806: ZW = zw_mix1(n6804, n6486, 313u64);
    let n6807: ZW = zw_mix2(n6805, n6486, 313u64);
    let n6808: ZW = zw_mix1(n6780, n6479, 303u64);
    let n6809: ZW = zw_mix2(n6781, n6479, 303u64);
    let n6810: ZW = zw_mix1(n6808, n6303, 304u64);
    let n6811: ZW = zw_mix2(n6809, n6303, 304u64);
    let n6812: ZW = zw_mix1(n6810, n6461, 312u64);
    let n6813: ZW = zw_mix2(n6811, n6461, 312u64);
    let n6814: ZW = zw_mix1(n6812, n6495, 313u64);
    let n6815: ZW = zw_mix2(n6813, n6495, 313u64);
    let n6816: ZW = zw_mix1(n6790, n6479, 303u64);
    let n6817: ZW = zw_mix2(n6791, n6479, 303u64);
    let n6818: ZW = zw_mix1(n6816, n6312, 304u64);
    let n6819: ZW = zw_mix2(n6817, n6312, 304u64);
    let n6820: ZW = zw_mix1(n6818, n6473, 312u64);
    let n6821: ZW = zw_mix2(n6819, n6473, 312u64);
    let n6822: ZW = zw_mix1(n6820, n6504, 313u64);
    let n6823: ZW = zw_mix2(n6821, n6504, 313u64);
    let n6824: ZW = zw_mix1(n6718, n6321, 241u64);
    let n6825: ZW = zw_mix2(n6719, n6321, 241u64);
    let n6826: ZW = zw_mix1(n6824, zw_splat(n6326), 248u64);
    let n6827: ZW = zw_mix2(n6825, zw_splat(n6326), 248u64);
    let n6828: ZW = zw_mix1(n6826, zw_splat(n6326), 249u64);
    let n6829: ZW = zw_mix2(n6827, zw_splat(n6326), 249u64);
    let n6830: ZW = zw_mix1(n6828, n6381, 255u64);
    let n6831: ZW = zw_mix2(n6829, n6381, 255u64);
    let n6832: ZW = zw_mix1(n6830, n6384, 300u64);
    let n6833: ZW = zw_mix2(n6831, n6384, 300u64);
    let n6834: ZW = zw_mix1(n6832, n6387, 301u64);
    let n6835: ZW = zw_mix2(n6833, n6387, 301u64);
    let n6836: ZW = zw_mix1(n6834, n6390, 302u64);
    let n6837: ZW = zw_mix2(n6835, n6390, 302u64);
    let n6838: ZW = zw_mix1(n6836, n6393, 303u64);
    let n6839: ZW = zw_mix2(n6837, n6393, 303u64);
    let n6840: ZW = zw_mix1(n6838, n6294, 304u64);
    let n6841: ZW = zw_mix2(n6839, n6294, 304u64);
    let n6842: ZW = zw_mix1(n6840, n6525, 312u64);
    let n6843: ZW = zw_mix2(n6841, n6525, 312u64);
    let n6844: ZW = zw_mix1(n6842, n6528, 313u64);
    let n6845: ZW = zw_mix2(n6843, n6528, 313u64);
    let n6846: ZW = zw_mix1(n6832, n6404, 301u64);
    let n6847: ZW = zw_mix2(n6833, n6404, 301u64);
    let n6848: ZW = zw_mix1(n6846, n6407, 302u64);
    let n6849: ZW = zw_mix2(n6847, n6407, 302u64);
    let n6850: ZW = zw_mix1(n6848, n6393, 303u64);
    let n6851: ZW = zw_mix2(n6849, n6393, 303u64);
    let n6852: ZW = zw_mix1(n6850, n6303, 304u64);
    let n6853: ZW = zw_mix2(n6851, n6303, 304u64);
    let n6854: ZW = zw_mix1(n6852, n6539, 312u64);
    let n6855: ZW = zw_mix2(n6853, n6539, 312u64);
    let n6856: ZW = zw_mix1(n6854, n6542, 313u64);
    let n6857: ZW = zw_mix2(n6855, n6542, 313u64);
    let n6858: ZW = zw_mix1(n6846, n6420, 302u64);
    let n6859: ZW = zw_mix2(n6847, n6420, 302u64);
    let n6860: ZW = zw_mix1(n6858, n6393, 303u64);
    let n6861: ZW = zw_mix2(n6859, n6393, 303u64);
    let n6862: ZW = zw_mix1(n6860, n6312, 304u64);
    let n6863: ZW = zw_mix2(n6861, n6312, 304u64);
    let n6864: ZW = zw_mix1(n6862, n6551, 312u64);
    let n6865: ZW = zw_mix2(n6863, n6551, 312u64);
    let n6866: ZW = zw_mix1(n6864, n6554, 313u64);
    let n6867: ZW = zw_mix2(n6865, n6554, 313u64);
    let n6868: ZW = zw_mix1(n6830, n6433, 300u64);
    let n6869: ZW = zw_mix2(n6831, n6433, 300u64);
    let n6870: ZW = zw_mix1(n6868, n6436, 301u64);
    let n6871: ZW = zw_mix2(n6869, n6436, 301u64);
    let n6872: ZW = zw_mix1(n6870, n6439, 302u64);
    let n6873: ZW = zw_mix2(n6871, n6439, 302u64);
    let n6874: ZW = zw_mix1(n6872, n6442, 303u64);
    let n6875: ZW = zw_mix2(n6873, n6442, 303u64);
    let n6876: ZW = zw_mix1(n6874, n6294, 304u64);
    let n6877: ZW = zw_mix2(n6875, n6294, 304u64);
    let n6878: ZW = zw_mix1(n6876, n6567, 312u64);
    let n6879: ZW = zw_mix2(n6877, n6567, 312u64);
    let n6880: ZW = zw_mix1(n6878, n6570, 313u64);
    let n6881: ZW = zw_mix2(n6879, n6570, 313u64);
    let n6882: ZW = zw_mix1(n6868, n6404, 301u64);
    let n6883: ZW = zw_mix2(n6869, n6404, 301u64);
    let n6884: ZW = zw_mix1(n6882, n6407, 302u64);
    let n6885: ZW = zw_mix2(n6883, n6407, 302u64);
    let n6886: ZW = zw_mix1(n6884, n6442, 303u64);
    let n6887: ZW = zw_mix2(n6885, n6442, 303u64);
    let n6888: ZW = zw_mix1(n6886, n6303, 304u64);
    let n6889: ZW = zw_mix2(n6887, n6303, 304u64);
    let n6890: ZW = zw_mix1(n6888, n6581, 312u64);
    let n6891: ZW = zw_mix2(n6889, n6581, 312u64);
    let n6892: ZW = zw_mix1(n6890, n6584, 313u64);
    let n6893: ZW = zw_mix2(n6891, n6584, 313u64);
    let n6894: ZW = zw_mix1(n6882, n6420, 302u64);
    let n6895: ZW = zw_mix2(n6883, n6420, 302u64);
    let n6896: ZW = zw_mix1(n6894, n6442, 303u64);
    let n6897: ZW = zw_mix2(n6895, n6442, 303u64);
    let n6898: ZW = zw_mix1(n6896, n6312, 304u64);
    let n6899: ZW = zw_mix2(n6897, n6312, 304u64);
    let n6900: ZW = zw_mix1(n6898, n6593, 312u64);
    let n6901: ZW = zw_mix2(n6899, n6593, 312u64);
    let n6902: ZW = zw_mix1(n6900, n6596, 313u64);
    let n6903: ZW = zw_mix2(n6901, n6596, 313u64);
    let n6904: ZW = zw_mix1(n6872, n6479, 303u64);
    let n6905: ZW = zw_mix2(n6873, n6479, 303u64);
    let n6906: ZW = zw_mix1(n6904, n6294, 304u64);
    let n6907: ZW = zw_mix2(n6905, n6294, 304u64);
    let n6908: ZW = zw_mix1(n6906, n6567, 312u64);
    let n6909: ZW = zw_mix2(n6907, n6567, 312u64);
    let n6910: ZW = zw_mix1(n6908, n6605, 313u64);
    let n6911: ZW = zw_mix2(n6909, n6605, 313u64);
    let n6912: ZW = zw_mix1(n6884, n6479, 303u64);
    let n6913: ZW = zw_mix2(n6885, n6479, 303u64);
    let n6914: ZW = zw_mix1(n6912, n6303, 304u64);
    let n6915: ZW = zw_mix2(n6913, n6303, 304u64);
    let n6916: ZW = zw_mix1(n6914, n6581, 312u64);
    let n6917: ZW = zw_mix2(n6915, n6581, 312u64);
    let n6918: ZW = zw_mix1(n6916, n6614, 313u64);
    let n6919: ZW = zw_mix2(n6917, n6614, 313u64);
    let n6920: ZW = zw_mix1(n6894, n6479, 303u64);
    let n6921: ZW = zw_mix2(n6895, n6479, 303u64);
    let n6922: ZW = zw_mix1(n6920, n6312, 304u64);
    let n6923: ZW = zw_mix2(n6921, n6312, 304u64);
    let n6924: ZW = zw_mix1(n6922, n6593, 312u64);
    let n6925: ZW = zw_mix2(n6923, n6593, 312u64);
    let n6926: ZW = zw_mix1(n6924, n6623, 313u64);
    let n6927: ZW = zw_mix2(n6925, n6623, 313u64);
    let n6928: ZW = zw_mix1(zw_splat(11400714819323198485u64), n6238, 84u64);
    let n6929: ZW = zw_mix2(zw_splat(11562461410679940143u64), n6238, 84u64);
    let n6930: ZW = zw_mix1(n6928, n6241, 85u64);
    let n6931: ZW = zw_mix2(n6929, n6241, 85u64);
    let n6932: ZW = zw_mix1(n6930, n6244, 86u64);
    let n6933: ZW = zw_mix2(n6931, n6244, 86u64);
    let n6934: ZW = zw_bits_n(n2400);
    let n6935: ZW = zw_mix1(n6932, n6934, 87u64);
    let n6936: ZW = zw_mix2(n6933, n6934, 87u64);
    let n6937: ZW = zw_mix1(n6935, n6259, 20u64);
    let n6938: ZW = zw_mix2(n6936, n6259, 20u64);
    let n6939: ZW = zw_mix1(n6937, n6262, 41u64);
    let n6940: ZW = zw_mix2(n6938, n6262, 41u64);
    let n6941: ZW = zw_mix1(n6935, n6363, 20u64);
    let n6942: ZW = zw_mix2(n6936, n6363, 20u64);
    let n6943: ZW = zw_mix1(n6941, n6366, 41u64);
    let n6944: ZW = zw_mix2(n6942, n6366, 41u64);
    let n6945: ZW = zw_bits_n(n4508);
    let n6946: ZW = zw_mix1(n6932, n6945, 87u64);
    let n6947: ZW = zw_mix2(n6933, n6945, 87u64);
    let n6948: ZW = zw_mix1(n6946, n6259, 20u64);
    let n6949: ZW = zw_mix2(n6947, n6259, 20u64);
    let n6950: ZW = zw_mix1(n6948, n6262, 41u64);
    let n6951: ZW = zw_mix2(n6949, n6262, 41u64);
    let n6952: ZW = zw_bits_n(n4844);
    let n6953: ZW = zw_mix1(n6946, n6952, 20u64);
    let n6954: ZW = zw_mix2(n6947, n6952, 20u64);
    let n6955: ZW = zw_bits_b(n4845);
    let n6956: ZW = zw_mix1(n6953, n6955, 41u64);
    let n6957: ZW = zw_mix2(n6954, n6955, 41u64);
    let n6958: ZW = zw_mix1(n6932, n6259, 20u64);
    let n6959: ZW = zw_mix2(n6933, n6259, 20u64);
    let n6960: ZW = zw_bits_b(n5045);
    let n6961: ZW = zw_mix1(n6958, n6960, 38u64);
    let n6962: ZW = zw_mix2(n6959, n6960, 38u64);
    let n6963: ZW = zw_bits_n(n5057);
    let n6964: ZW = zw_mix1(n6961, n6963, 39u64);
    let n6965: ZW = zw_mix2(n6962, n6963, 39u64);
    let n6966: ZW = zw_bits_n(n5056);
    let n6967: ZW = zw_mix1(n6964, n6966, 87u64);
    let n6968: ZW = zw_mix2(n6965, n6966, 87u64);
    let n6969: ZW = zw_bits_n(n5046);
    let n6970: ZW = zw_mix1(n6967, n6969, 234u64);
    let n6971: ZW = zw_mix2(n6968, n6969, 234u64);
    let n6972: ZW = zw_bits_n(n5047);
    let n6973: ZW = zw_mix1(n6970, n6972, 246u64);
    let n6974: ZW = zw_mix2(n6971, n6972, 246u64);
    let n6975: ZW = zw_bits_n(n5048);
    let n6976: ZW = zw_mix1(n6973, n6975, 250u64);
    let n6977: ZW = zw_mix2(n6974, n6975, 250u64);
    let n6978: ZW = zw_bits_b(n5116);
    let n6979: ZW = zw_mix1(n6958, n6978, 38u64);
    let n6980: ZW = zw_mix2(n6959, n6978, 38u64);
    let n6981: ZW = zw_bits_n(n5128);
    let n6982: ZW = zw_mix1(n6979, n6981, 39u64);
    let n6983: ZW = zw_mix2(n6980, n6981, 39u64);
    let n6984: ZW = zw_bits_n(n5127);
    let n6985: ZW = zw_mix1(n6982, n6984, 87u64);
    let n6986: ZW = zw_mix2(n6983, n6984, 87u64);
    let n6987: ZW = zw_bits_n(n5117);
    let n6988: ZW = zw_mix1(n6985, n6987, 234u64);
    let n6989: ZW = zw_mix2(n6986, n6987, 234u64);
    let n6990: ZW = zw_bits_n(n5118);
    let n6991: ZW = zw_mix1(n6988, n6990, 246u64);
    let n6992: ZW = zw_mix2(n6989, n6990, 246u64);
    let n6993: ZW = zw_bits_n(n5119);
    let n6994: ZW = zw_mix1(n6991, n6993, 250u64);
    let n6995: ZW = zw_mix2(n6992, n6993, 250u64);
    let n6996: ZW = zw_bits_b(n5187);
    let n6997: ZW = zw_mix1(n6958, n6996, 38u64);
    let n6998: ZW = zw_mix2(n6959, n6996, 38u64);
    let n6999: ZW = zw_bits_n(n5199);
    let n7000: ZW = zw_mix1(n6997, n6999, 39u64);
    let n7001: ZW = zw_mix2(n6998, n6999, 39u64);
    let n7002: ZW = zw_bits_n(n5198);
    let n7003: ZW = zw_mix1(n7000, n7002, 87u64);
    let n7004: ZW = zw_mix2(n7001, n7002, 87u64);
    let n7005: ZW = zw_bits_n(n5188);
    let n7006: ZW = zw_mix1(n7003, n7005, 234u64);
    let n7007: ZW = zw_mix2(n7004, n7005, 234u64);
    let n7008: ZW = zw_bits_n(n5189);
    let n7009: ZW = zw_mix1(n7006, n7008, 246u64);
    let n7010: ZW = zw_mix2(n7007, n7008, 246u64);
    let n7011: ZW = zw_bits_n(n5190);
    let n7012: ZW = zw_mix1(n7009, n7011, 250u64);
    let n7013: ZW = zw_mix2(n7010, n7011, 250u64);
    let n7014: ZW = zw_bits_b(n5269);
    let n7015: ZW = zw_mix1(n6958, n7014, 38u64);
    let n7016: ZW = zw_mix2(n6959, n7014, 38u64);
    let n7017: ZW = zw_bits_n(n5281);
    let n7018: ZW = zw_mix1(n7015, n7017, 39u64);
    let n7019: ZW = zw_mix2(n7016, n7017, 39u64);
    let n7020: ZW = zw_bits_n(n5280);
    let n7021: ZW = zw_mix1(n7018, n7020, 87u64);
    let n7022: ZW = zw_mix2(n7019, n7020, 87u64);
    let n7023: ZW = zw_bits_n(n5270);
    let n7024: ZW = zw_mix1(n7021, n7023, 234u64);
    let n7025: ZW = zw_mix2(n7022, n7023, 234u64);
    let n7026: ZW = zw_bits_n(n5271);
    let n7027: ZW = zw_mix1(n7024, n7026, 246u64);
    let n7028: ZW = zw_mix2(n7025, n7026, 246u64);
    let n7029: ZW = zw_bits_n(n5272);
    let n7030: ZW = zw_mix1(n7027, n7029, 250u64);
    let n7031: ZW = zw_mix2(n7028, n7029, 250u64);
    let n7032: ZW = zw_bits_b(n5351);
    let n7033: ZW = zw_mix1(n6958, n7032, 38u64);
    let n7034: ZW = zw_mix2(n6959, n7032, 38u64);
    let n7035: ZW = zw_bits_n(n5363);
    let n7036: ZW = zw_mix1(n7033, n7035, 39u64);
    let n7037: ZW = zw_mix2(n7034, n7035, 39u64);
    let n7038: ZW = zw_bits_n(n5362);
    let n7039: ZW = zw_mix1(n7036, n7038, 87u64);
    let n7040: ZW = zw_mix2(n7037, n7038, 87u64);
    let n7041: ZW = zw_bits_n(n5352);
    let n7042: ZW = zw_mix1(n7039, n7041, 234u64);
    let n7043: ZW = zw_mix2(n7040, n7041, 234u64);
    let n7044: ZW = zw_bits_n(n5353);
    let n7045: ZW = zw_mix1(n7042, n7044, 246u64);
    let n7046: ZW = zw_mix2(n7043, n7044, 246u64);
    let n7047: ZW = zw_bits_n(n5354);
    let n7048: ZW = zw_mix1(n7045, n7047, 250u64);
    let n7049: ZW = zw_mix2(n7046, n7047, 250u64);
    let n7050: ZW = zw_bits_b(n5433);
    let n7051: ZW = zw_mix1(n6958, n7050, 38u64);
    let n7052: ZW = zw_mix2(n6959, n7050, 38u64);
    let n7053: ZW = zw_bits_n(n5445);
    let n7054: ZW = zw_mix1(n7051, n7053, 39u64);
    let n7055: ZW = zw_mix2(n7052, n7053, 39u64);
    let n7056: ZW = zw_bits_n(n5444);
    let n7057: ZW = zw_mix1(n7054, n7056, 87u64);
    let n7058: ZW = zw_mix2(n7055, n7056, 87u64);
    let n7059: ZW = zw_bits_n(n5434);
    let n7060: ZW = zw_mix1(n7057, n7059, 234u64);
    let n7061: ZW = zw_mix2(n7058, n7059, 234u64);
    let n7062: ZW = zw_bits_n(n5435);
    let n7063: ZW = zw_mix1(n7060, n7062, 246u64);
    let n7064: ZW = zw_mix2(n7061, n7062, 246u64);
    let n7065: ZW = zw_bits_n(n5436);
    let n7066: ZW = zw_mix1(n7063, n7065, 250u64);
    let n7067: ZW = zw_mix2(n7064, n7065, 250u64);
    let n7068: ZW = zw_bits_n(n5476);
    let n7069: ZW = zw_mix1(n6932, n7068, 20u64);
    let n7070: ZW = zw_mix2(n6933, n7068, 20u64);
    let n7071: ZW = zw_bits_b(n5477);
    let n7072: ZW = zw_mix1(n7069, n7071, 38u64);
    let n7073: ZW = zw_mix2(n7070, n7071, 38u64);
    let n7074: ZW = zw_bits_n(n5493);
    let n7075: ZW = zw_mix1(n7072, n7074, 39u64);
    let n7076: ZW = zw_mix2(n7073, n7074, 39u64);
    let n7077: ZW = zw_bits_n(n5492);
    let n7078: ZW = zw_mix1(n7075, n7077, 87u64);
    let n7079: ZW = zw_mix2(n7076, n7077, 87u64);
    let n7080: ZW = zw_bits_n(n5478);
    let n7081: ZW = zw_mix1(n7078, n7080, 234u64);
    let n7082: ZW = zw_mix2(n7079, n7080, 234u64);
    let n7083: ZW = zw_bits_n(n5479);
    let n7084: ZW = zw_mix1(n7081, n7083, 246u64);
    let n7085: ZW = zw_mix2(n7082, n7083, 246u64);
    let n7086: ZW = zw_bits_n(n5480);
    let n7087: ZW = zw_mix1(n7084, n7086, 250u64);
    let n7088: ZW = zw_mix2(n7085, n7086, 250u64);
    let n7089: ZW = zw_bits_n(n5512);
    let n7090: ZW = zw_mix1(n6932, n7089, 20u64);
    let n7091: ZW = zw_mix2(n6933, n7089, 20u64);
    let n7092: ZW = zw_bits_b(n5513);
    let n7093: ZW = zw_mix1(n7090, n7092, 38u64);
    let n7094: ZW = zw_mix2(n7091, n7092, 38u64);
    let n7095: ZW = zw_bits_n(n5529);
    let n7096: ZW = zw_mix1(n7093, n7095, 39u64);
    let n7097: ZW = zw_mix2(n7094, n7095, 39u64);
    let n7098: ZW = zw_bits_n(n5528);
    let n7099: ZW = zw_mix1(n7096, n7098, 87u64);
    let n7100: ZW = zw_mix2(n7097, n7098, 87u64);
    let n7101: ZW = zw_bits_n(n5514);
    let n7102: ZW = zw_mix1(n7099, n7101, 234u64);
    let n7103: ZW = zw_mix2(n7100, n7101, 234u64);
    let n7104: ZW = zw_bits_n(n5515);
    let n7105: ZW = zw_mix1(n7102, n7104, 246u64);
    let n7106: ZW = zw_mix2(n7103, n7104, 246u64);
    let n7107: ZW = zw_bits_n(n5516);
    let n7108: ZW = zw_mix1(n7105, n7107, 250u64);
    let n7109: ZW = zw_mix2(n7106, n7107, 250u64);
    let n7110: ZW = zw_bits_n(n5548);
    let n7111: ZW = zw_mix1(n6932, n7110, 20u64);
    let n7112: ZW = zw_mix2(n6933, n7110, 20u64);
    let n7113: ZW = zw_bits_b(n5549);
    let n7114: ZW = zw_mix1(n7111, n7113, 38u64);
    let n7115: ZW = zw_mix2(n7112, n7113, 38u64);
    let n7116: ZW = zw_bits_n(n5565);
    let n7117: ZW = zw_mix1(n7114, n7116, 39u64);
    let n7118: ZW = zw_mix2(n7115, n7116, 39u64);
    let n7119: ZW = zw_bits_n(n5564);
    let n7120: ZW = zw_mix1(n7117, n7119, 87u64);
    let n7121: ZW = zw_mix2(n7118, n7119, 87u64);
    let n7122: ZW = zw_bits_n(n5550);
    let n7123: ZW = zw_mix1(n7120, n7122, 234u64);
    let n7124: ZW = zw_mix2(n7121, n7122, 234u64);
    let n7125: ZW = zw_bits_n(n5551);
    let n7126: ZW = zw_mix1(n7123, n7125, 246u64);
    let n7127: ZW = zw_mix2(n7124, n7125, 246u64);
    let n7128: ZW = zw_bits_n(n5552);
    let n7129: ZW = zw_mix1(n7126, n7128, 250u64);
    let n7130: ZW = zw_mix2(n7127, n7128, 250u64);
    let n7131: ZW = zw_bits_n(n5582);
    let n7132: ZW = zw_mix1(n6932, n7131, 20u64);
    let n7133: ZW = zw_mix2(n6933, n7131, 20u64);
    let n7134: ZW = zw_bits_b(n5583);
    let n7135: ZW = zw_mix1(n7132, n7134, 38u64);
    let n7136: ZW = zw_mix2(n7133, n7134, 38u64);
    let n7137: ZW = zw_bits_n(n5599);
    let n7138: ZW = zw_mix1(n7135, n7137, 39u64);
    let n7139: ZW = zw_mix2(n7136, n7137, 39u64);
    let n7140: ZW = zw_bits_n(n5598);
    let n7141: ZW = zw_mix1(n7138, n7140, 87u64);
    let n7142: ZW = zw_mix2(n7139, n7140, 87u64);
    let n7143: ZW = zw_bits_n(n5584);
    let n7144: ZW = zw_mix1(n7141, n7143, 234u64);
    let n7145: ZW = zw_mix2(n7142, n7143, 234u64);
    let n7146: ZW = zw_bits_n(n5585);
    let n7147: ZW = zw_mix1(n7144, n7146, 246u64);
    let n7148: ZW = zw_mix2(n7145, n7146, 246u64);
    let n7149: ZW = zw_bits_n(n5586);
    let n7150: ZW = zw_mix1(n7147, n7149, 250u64);
    let n7151: ZW = zw_mix2(n7148, n7149, 250u64);
    let n7152: ZW = zw_bits_n(n5630);
    let n7153: ZW = zw_mix1(n6932, n7152, 20u64);
    let n7154: ZW = zw_mix2(n6933, n7152, 20u64);
    let n7155: ZW = zw_bits_b(n5631);
    let n7156: ZW = zw_mix1(n7153, n7155, 38u64);
    let n7157: ZW = zw_mix2(n7154, n7155, 38u64);
    let n7158: ZW = zw_bits_n(n5647);
    let n7159: ZW = zw_mix1(n7156, n7158, 39u64);
    let n7160: ZW = zw_mix2(n7157, n7158, 39u64);
    let n7161: ZW = zw_bits_n(n5646);
    let n7162: ZW = zw_mix1(n7159, n7161, 87u64);
    let n7163: ZW = zw_mix2(n7160, n7161, 87u64);
    let n7164: ZW = zw_bits_n(n5632);
    let n7165: ZW = zw_mix1(n7162, n7164, 234u64);
    let n7166: ZW = zw_mix2(n7163, n7164, 234u64);
    let n7167: ZW = zw_bits_n(n5633);
    let n7168: ZW = zw_mix1(n7165, n7167, 246u64);
    let n7169: ZW = zw_mix2(n7166, n7167, 246u64);
    let n7170: ZW = zw_bits_n(n5634);
    let n7171: ZW = zw_mix1(n7168, n7170, 250u64);
    let n7172: ZW = zw_mix2(n7169, n7170, 250u64);
    let n7173: ZW = zw_bits_n(n5666);
    let n7174: ZW = zw_mix1(n6932, n7173, 20u64);
    let n7175: ZW = zw_mix2(n6933, n7173, 20u64);
    let n7176: ZW = zw_bits_b(n5667);
    let n7177: ZW = zw_mix1(n7174, n7176, 38u64);
    let n7178: ZW = zw_mix2(n7175, n7176, 38u64);
    let n7179: ZW = zw_bits_n(n5683);
    let n7180: ZW = zw_mix1(n7177, n7179, 39u64);
    let n7181: ZW = zw_mix2(n7178, n7179, 39u64);
    let n7182: ZW = zw_bits_n(n5682);
    let n7183: ZW = zw_mix1(n7180, n7182, 87u64);
    let n7184: ZW = zw_mix2(n7181, n7182, 87u64);
    let n7185: ZW = zw_bits_n(n5668);
    let n7186: ZW = zw_mix1(n7183, n7185, 234u64);
    let n7187: ZW = zw_mix2(n7184, n7185, 234u64);
    let n7188: ZW = zw_bits_n(n5669);
    let n7189: ZW = zw_mix1(n7186, n7188, 246u64);
    let n7190: ZW = zw_mix2(n7187, n7188, 246u64);
    let n7191: ZW = zw_bits_n(n5670);
    let n7192: ZW = zw_mix1(n7189, n7191, 250u64);
    let n7193: ZW = zw_mix2(n7190, n7191, 250u64);
    let n7194: ZW = zw_bits_n(n5702);
    let n7195: ZW = zw_mix1(n6932, n7194, 20u64);
    let n7196: ZW = zw_mix2(n6933, n7194, 20u64);
    let n7197: ZW = zw_bits_b(n5703);
    let n7198: ZW = zw_mix1(n7195, n7197, 38u64);
    let n7199: ZW = zw_mix2(n7196, n7197, 38u64);
    let n7200: ZW = zw_bits_n(n5719);
    let n7201: ZW = zw_mix1(n7198, n7200, 39u64);
    let n7202: ZW = zw_mix2(n7199, n7200, 39u64);
    let n7203: ZW = zw_bits_n(n5718);
    let n7204: ZW = zw_mix1(n7201, n7203, 87u64);
    let n7205: ZW = zw_mix2(n7202, n7203, 87u64);
    let n7206: ZW = zw_bits_n(n5704);
    let n7207: ZW = zw_mix1(n7204, n7206, 234u64);
    let n7208: ZW = zw_mix2(n7205, n7206, 234u64);
    let n7209: ZW = zw_bits_n(n5705);
    let n7210: ZW = zw_mix1(n7207, n7209, 246u64);
    let n7211: ZW = zw_mix2(n7208, n7209, 246u64);
    let n7212: ZW = zw_bits_n(n5706);
    let n7213: ZW = zw_mix1(n7210, n7212, 250u64);
    let n7214: ZW = zw_mix2(n7211, n7212, 250u64);
    let n7215: ZW = zw_bits_n(n5736);
    let n7216: ZW = zw_mix1(n6932, n7215, 20u64);
    let n7217: ZW = zw_mix2(n6933, n7215, 20u64);
    let n7218: ZW = zw_bits_b(n5737);
    let n7219: ZW = zw_mix1(n7216, n7218, 38u64);
    let n7220: ZW = zw_mix2(n7217, n7218, 38u64);
    let n7221: ZW = zw_bits_n(n5753);
    let n7222: ZW = zw_mix1(n7219, n7221, 39u64);
    let n7223: ZW = zw_mix2(n7220, n7221, 39u64);
    let n7224: ZW = zw_bits_n(n5752);
    let n7225: ZW = zw_mix1(n7222, n7224, 87u64);
    let n7226: ZW = zw_mix2(n7223, n7224, 87u64);
    let n7227: ZW = zw_bits_n(n5738);
    let n7228: ZW = zw_mix1(n7225, n7227, 234u64);
    let n7229: ZW = zw_mix2(n7226, n7227, 234u64);
    let n7230: ZW = zw_bits_n(n5739);
    let n7231: ZW = zw_mix1(n7228, n7230, 246u64);
    let n7232: ZW = zw_mix2(n7229, n7230, 246u64);
    let n7233: ZW = zw_bits_n(n5740);
    let n7234: ZW = zw_mix1(n7231, n7233, 250u64);
    let n7235: ZW = zw_mix2(n7232, n7233, 250u64);
    let n7236: ZW = zw_bits_n(n5834);
    let n7237: ZW = zw_mix1(n6248, n7236, 273u64);
    let n7238: ZW = zw_mix2(n6249, n7236, 273u64);
    let n7239: ZW = zw_bits_n(n5836);
    let n7240: ZW = zw_mix1(n7237, n7239, 318u64);
    let n7241: ZW = zw_mix2(n7238, n7239, 318u64);
    let n7242: ZW = zw_bits_n(n5837);
    let n7243: ZW = zw_mix1(n7240, n7242, 319u64);
    let n7244: ZW = zw_mix2(n7241, n7242, 319u64);
    let n7245: ZW = zw_bits_n(n5828);
    let n7246: ZW = zw_mix1(n7243, n7245, 20u64);
    let n7247: ZW = zw_mix2(n7244, n7245, 20u64);
    let n7248: ZW = zw_mix1(n7246, n6262, 41u64);
    let n7249: ZW = zw_mix2(n7247, n6262, 41u64);
    let n7250: ZW = zw_bits_n(n5829);
    let n7251: ZW = zw_mix1(n7248, n7250, 253u64);
    let n7252: ZW = zw_mix2(n7249, n7250, 253u64);
    let n7253: ZW = zw_bits_n(n5830);
    let n7254: ZW = zw_mix1(n7251, n7253, 255u64);
    let n7255: ZW = zw_mix2(n7252, n7253, 255u64);
    let n7256: ZW = zw_bits_n(n5831);
    let n7257: ZW = zw_mix1(n7254, n7256, 256u64);
    let n7258: ZW = zw_mix2(n7255, n7256, 256u64);
    let n7259: ZW = zw_bits_n(n5832);
    let n7260: ZW = zw_mix1(n7257, n7259, 258u64);
    let n7261: ZW = zw_mix2(n7258, n7259, 258u64);
    let n7262: ZW = zw_bits_b(n5761);
    let n7263: ZW = zw_mix1(n7260, n7262, 265u64);
    let n7264: ZW = zw_mix2(n7261, n7262, 265u64);
    let n7265: ZW = zw_bits_b(n5762);
    let n7266: ZW = zw_mix1(n7263, n7265, 266u64);
    let n7267: ZW = zw_mix2(n7264, n7265, 266u64);
    let n7268: ZW = zw_bits_n(n5861);
    let n7269: ZW = zw_mix1(n7266, n7268, 272u64);
    let n7270: ZW = zw_mix2(n7267, n7268, 272u64);
    let n7271: ZW = zw_mix1(n7269, n6282, 308u64);
    let n7272: ZW = zw_mix2(n7270, n6282, 308u64);
    let n7273: ZW = zw_mix1(n7271, n6285, 309u64);
    let n7274: ZW = zw_mix2(n7272, n6285, 309u64);
    let n7275: ZW = zw_mix1(n7273, n6288, 310u64);
    let n7276: ZW = zw_mix2(n7274, n6288, 310u64);
    let n7277: ZW = zw_mix1(n7275, n6291, 311u64);
    let n7278: ZW = zw_mix2(n7276, n6291, 311u64);
    let n7279: ZW = zw_bits_b(n5835);
    let n7280: ZW = zw_mix1(n7277, n7279, 312u64);
    let n7281: ZW = zw_mix2(n7278, n7279, 312u64);
    let n7282: ZW = zw_bits_n(n5862);
    let n7283: ZW = zw_mix1(n7280, n7282, 320u64);
    let n7284: ZW = zw_mix2(n7281, n7282, 320u64);
    let n7285: ZW = zw_bits_n(n5839);
    let n7286: ZW = zw_mix1(n7283, n7285, 321u64);
    let n7287: ZW = zw_mix2(n7284, n7285, 321u64);
    let n7288: ZW = zw_bits_b(n5877);
    let n7289: ZW = zw_mix1(n7277, n7288, 312u64);
    let n7290: ZW = zw_mix2(n7278, n7288, 312u64);
    let n7291: ZW = zw_bits_n(n5890);
    let n7292: ZW = zw_mix1(n7289, n7291, 320u64);
    let n7293: ZW = zw_mix2(n7290, n7291, 320u64);
    let n7294: ZW = zw_bits_n(n5879);
    let n7295: ZW = zw_mix1(n7292, n7294, 321u64);
    let n7296: ZW = zw_mix2(n7293, n7294, 321u64);
    let n7297: ZW = zw_bits_b(n5904);
    let n7298: ZW = zw_mix1(n7277, n7297, 312u64);
    let n7299: ZW = zw_mix2(n7278, n7297, 312u64);
    let n7300: ZW = zw_bits_n(n5917);
    let n7301: ZW = zw_mix1(n7298, n7300, 320u64);
    let n7302: ZW = zw_mix2(n7299, n7300, 320u64);
    let n7303: ZW = zw_bits_n(n5906);
    let n7304: ZW = zw_mix1(n7301, n7303, 321u64);
    let n7305: ZW = zw_mix2(n7302, n7303, 321u64);
    let n7306: ZW = zw_bits_n(n5927);
    let n7307: ZW = zw_mix1(n7257, n7306, 258u64);
    let n7308: ZW = zw_mix2(n7258, n7306, 258u64);
    let n7309: ZW = zw_mix1(n7307, n7262, 265u64);
    let n7310: ZW = zw_mix2(n7308, n7262, 265u64);
    let n7311: ZW = zw_bits_b(n5919);
    let n7312: ZW = zw_mix1(n7309, n7311, 266u64);
    let n7313: ZW = zw_mix2(n7310, n7311, 266u64);
    let n7314: ZW = zw_mix1(n7312, n7268, 272u64);
    let n7315: ZW = zw_mix2(n7313, n7268, 272u64);
    let n7316: ZW = zw_mix1(n7314, n6282, 308u64);
    let n7317: ZW = zw_mix2(n7315, n6282, 308u64);
    let n7318: ZW = zw_mix1(n7316, n6285, 309u64);
    let n7319: ZW = zw_mix2(n7317, n6285, 309u64);
    let n7320: ZW = zw_mix1(n7318, n6288, 310u64);
    let n7321: ZW = zw_mix2(n7319, n6288, 310u64);
    let n7322: ZW = zw_mix1(n7320, n6291, 311u64);
    let n7323: ZW = zw_mix2(n7321, n6291, 311u64);
    let n7324: ZW = zw_mix1(n7322, n7279, 312u64);
    let n7325: ZW = zw_mix2(n7323, n7279, 312u64);
    let n7326: ZW = zw_bits_n(n5940);
    let n7327: ZW = zw_mix1(n7324, n7326, 320u64);
    let n7328: ZW = zw_mix2(n7325, n7326, 320u64);
    let n7329: ZW = zw_bits_n(n5929);
    let n7330: ZW = zw_mix1(n7327, n7329, 321u64);
    let n7331: ZW = zw_mix2(n7328, n7329, 321u64);
    let n7332: ZW = zw_mix1(n7322, n7288, 312u64);
    let n7333: ZW = zw_mix2(n7323, n7288, 312u64);
    let n7334: ZW = zw_bits_n(n5959);
    let n7335: ZW = zw_mix1(n7332, n7334, 320u64);
    let n7336: ZW = zw_mix2(n7333, n7334, 320u64);
    let n7337: ZW = zw_bits_n(n5948);
    let n7338: ZW = zw_mix1(n7335, n7337, 321u64);
    let n7339: ZW = zw_mix2(n7336, n7337, 321u64);
    let n7340: ZW = zw_mix1(n7322, n7297, 312u64);
    let n7341: ZW = zw_mix2(n7323, n7297, 312u64);
    let n7342: ZW = zw_bits_n(n5978);
    let n7343: ZW = zw_mix1(n7340, n7342, 320u64);
    let n7344: ZW = zw_mix2(n7341, n7342, 320u64);
    let n7345: ZW = zw_bits_n(n5967);
    let n7346: ZW = zw_mix1(n7343, n7345, 321u64);
    let n7347: ZW = zw_mix2(n7344, n7345, 321u64);
    let n7348: ZW = zw_bits_n(n6000);
    let n7349: ZW = zw_mix1(n7243, n7348, 20u64);
    let n7350: ZW = zw_mix2(n7244, n7348, 20u64);
    let n7351: ZW = zw_bits_b(n6001);
    let n7352: ZW = zw_mix1(n7349, n7351, 41u64);
    let n7353: ZW = zw_mix2(n7350, n7351, 41u64);
    let n7354: ZW = zw_bits_n(n6002);
    let n7355: ZW = zw_mix1(n7352, n7354, 253u64);
    let n7356: ZW = zw_mix2(n7353, n7354, 253u64);
    let n7357: ZW = zw_bits_n(n6003);
    let n7358: ZW = zw_mix1(n7355, n7357, 255u64);
    let n7359: ZW = zw_mix2(n7356, n7357, 255u64);
    let n7360: ZW = zw_bits_n(n6004);
    let n7361: ZW = zw_mix1(n7358, n7360, 256u64);
    let n7362: ZW = zw_mix2(n7359, n7360, 256u64);
    let n7363: ZW = zw_mix1(n7361, n7259, 258u64);
    let n7364: ZW = zw_mix2(n7362, n7259, 258u64);
    let n7365: ZW = zw_bits_b(n5980);
    let n7366: ZW = zw_mix1(n7363, n7365, 265u64);
    let n7367: ZW = zw_mix2(n7364, n7365, 265u64);
    let n7368: ZW = zw_mix1(n7366, n7265, 266u64);
    let n7369: ZW = zw_mix2(n7367, n7265, 266u64);
    let n7370: ZW = zw_bits_n(n6023);
    let n7371: ZW = zw_mix1(n7368, n7370, 272u64);
    let n7372: ZW = zw_mix2(n7369, n7370, 272u64);
    let n7373: ZW = zw_bits_n(n6005);
    let n7374: ZW = zw_mix1(n7371, n7373, 308u64);
    let n7375: ZW = zw_mix2(n7372, n7373, 308u64);
    let n7376: ZW = zw_bits_n(n6006);
    let n7377: ZW = zw_mix1(n7374, n7376, 309u64);
    let n7378: ZW = zw_mix2(n7375, n7376, 309u64);
    let n7379: ZW = zw_bits_n(n6007);
    let n7380: ZW = zw_mix1(n7377, n7379, 310u64);
    let n7381: ZW = zw_mix2(n7378, n7379, 310u64);
    let n7382: ZW = zw_bits_n(n6008);
    let n7383: ZW = zw_mix1(n7380, n7382, 311u64);
    let n7384: ZW = zw_mix2(n7381, n7382, 311u64);
    let n7385: ZW = zw_mix1(n7383, n7279, 312u64);
    let n7386: ZW = zw_mix2(n7384, n7279, 312u64);
    let n7387: ZW = zw_bits_n(n6024);
    let n7388: ZW = zw_mix1(n7385, n7387, 320u64);
    let n7389: ZW = zw_mix2(n7386, n7387, 320u64);
    let n7390: ZW = zw_bits_n(n6010);
    let n7391: ZW = zw_mix1(n7388, n7390, 321u64);
    let n7392: ZW = zw_mix2(n7389, n7390, 321u64);
    let n7393: ZW = zw_bits_n(n6035);
    let n7394: ZW = zw_mix1(n7374, n7393, 309u64);
    let n7395: ZW = zw_mix2(n7375, n7393, 309u64);
    let n7396: ZW = zw_bits_n(n6036);
    let n7397: ZW = zw_mix1(n7394, n7396, 310u64);
    let n7398: ZW = zw_mix2(n7395, n7396, 310u64);
    let n7399: ZW = zw_mix1(n7397, n7382, 311u64);
    let n7400: ZW = zw_mix2(n7398, n7382, 311u64);
    let n7401: ZW = zw_mix1(n7399, n7288, 312u64);
    let n7402: ZW = zw_mix2(n7400, n7288, 312u64);
    let n7403: ZW = zw_bits_n(n6049);
    let n7404: ZW = zw_mix1(n7401, n7403, 320u64);
    let n7405: ZW = zw_mix2(n7402, n7403, 320u64);
    let n7406: ZW = zw_bits_n(n6038);
    let n7407: ZW = zw_mix1(n7404, n7406, 321u64);
    let n7408: ZW = zw_mix2(n7405, n7406, 321u64);
    let n7409: ZW = zw_bits_n(n6058);
    let n7410: ZW = zw_mix1(n7394, n7409, 310u64);
    let n7411: ZW = zw_mix2(n7395, n7409, 310u64);
    let n7412: ZW = zw_mix1(n7410, n7382, 311u64);
    let n7413: ZW = zw_mix2(n7411, n7382, 311u64);
    let n7414: ZW = zw_mix1(n7412, n7297, 312u64);
    let n7415: ZW = zw_mix2(n7413, n7297, 312u64);
    let n7416: ZW = zw_bits_n(n6071);
    let n7417: ZW = zw_mix1(n7414, n7416, 320u64);
    let n7418: ZW = zw_mix2(n7415, n7416, 320u64);
    let n7419: ZW = zw_bits_n(n6060);
    let n7420: ZW = zw_mix1(n7417, n7419, 321u64);
    let n7421: ZW = zw_mix2(n7418, n7419, 321u64);
    let n7422: ZW = zw_bits_n(n6086);
    let n7423: ZW = zw_mix1(n7371, n7422, 308u64);
    let n7424: ZW = zw_mix2(n7372, n7422, 308u64);
    let n7425: ZW = zw_bits_n(n6087);
    let n7426: ZW = zw_mix1(n7423, n7425, 309u64);
    let n7427: ZW = zw_mix2(n7424, n7425, 309u64);
    let n7428: ZW = zw_bits_n(n6088);
    let n7429: ZW = zw_mix1(n7426, n7428, 310u64);
    let n7430: ZW = zw_mix2(n7427, n7428, 310u64);
    let n7431: ZW = zw_bits_n(n6089);
    let n7432: ZW = zw_mix1(n7429, n7431, 311u64);
    let n7433: ZW = zw_mix2(n7430, n7431, 311u64);
    let n7434: ZW = zw_mix1(n7432, n7279, 312u64);
    let n7435: ZW = zw_mix2(n7433, n7279, 312u64);
    let n7436: ZW = zw_bits_n(n6102);
    let n7437: ZW = zw_mix1(n7434, n7436, 320u64);
    let n7438: ZW = zw_mix2(n7435, n7436, 320u64);
    let n7439: ZW = zw_bits_n(n6091);
    let n7440: ZW = zw_mix1(n7437, n7439, 321u64);
    let n7441: ZW = zw_mix2(n7438, n7439, 321u64);
    let n7442: ZW = zw_mix1(n7423, n7393, 309u64);
    let n7443: ZW = zw_mix2(n7424, n7393, 309u64);
    let n7444: ZW = zw_mix1(n7442, n7396, 310u64);
    let n7445: ZW = zw_mix2(n7443, n7396, 310u64);
    let n7446: ZW = zw_mix1(n7444, n7431, 311u64);
    let n7447: ZW = zw_mix2(n7445, n7431, 311u64);
    let n7448: ZW = zw_mix1(n7446, n7288, 312u64);
    let n7449: ZW = zw_mix2(n7447, n7288, 312u64);
    let n7450: ZW = zw_bits_n(n6111);
    let n7451: ZW = zw_mix1(n7448, n7450, 320u64);
    let n7452: ZW = zw_mix2(n7449, n7450, 320u64);
    let n7453: ZW = zw_bits_n(n6109);
    let n7454: ZW = zw_mix1(n7451, n7453, 321u64);
    let n7455: ZW = zw_mix2(n7452, n7453, 321u64);
    let n7456: ZW = zw_mix1(n7442, n7409, 310u64);
    let n7457: ZW = zw_mix2(n7443, n7409, 310u64);
    let n7458: ZW = zw_mix1(n7456, n7431, 311u64);
    let n7459: ZW = zw_mix2(n7457, n7431, 311u64);
    let n7460: ZW = zw_mix1(n7458, n7297, 312u64);
    let n7461: ZW = zw_mix2(n7459, n7297, 312u64);
    let n7462: ZW = zw_bits_n(n6119);
    let n7463: ZW = zw_mix1(n7460, n7462, 320u64);
    let n7464: ZW = zw_mix2(n7461, n7462, 320u64);
    let n7465: ZW = zw_bits_n(n6117);
    let n7466: ZW = zw_mix1(n7463, n7465, 321u64);
    let n7467: ZW = zw_mix2(n7464, n7465, 321u64);
    let n7468: ZW = zw_bits_n(n6124);
    let n7469: ZW = zw_mix1(n7429, n7468, 311u64);
    let n7470: ZW = zw_mix2(n7430, n7468, 311u64);
    let n7471: ZW = zw_mix1(n7469, n7279, 312u64);
    let n7472: ZW = zw_mix2(n7470, n7279, 312u64);
    let n7473: ZW = zw_mix1(n7471, n7436, 320u64);
    let n7474: ZW = zw_mix2(n7472, n7436, 320u64);
    let n7475: ZW = zw_bits_n(n6125);
    let n7476: ZW = zw_mix1(n7473, n7475, 321u64);
    let n7477: ZW = zw_mix2(n7474, n7475, 321u64);
    let n7478: ZW = zw_mix1(n7444, n7468, 311u64);
    let n7479: ZW = zw_mix2(n7445, n7468, 311u64);
    let n7480: ZW = zw_mix1(n7478, n7288, 312u64);
    let n7481: ZW = zw_mix2(n7479, n7288, 312u64);
    let n7482: ZW = zw_mix1(n7480, n7450, 320u64);
    let n7483: ZW = zw_mix2(n7481, n7450, 320u64);
    let n7484: ZW = zw_bits_n(n6128);
    let n7485: ZW = zw_mix1(n7482, n7484, 321u64);
    let n7486: ZW = zw_mix2(n7483, n7484, 321u64);
    let n7487: ZW = zw_mix1(n7456, n7468, 311u64);
    let n7488: ZW = zw_mix2(n7457, n7468, 311u64);
    let n7489: ZW = zw_mix1(n7487, n7297, 312u64);
    let n7490: ZW = zw_mix2(n7488, n7297, 312u64);
    let n7491: ZW = zw_mix1(n7489, n7462, 320u64);
    let n7492: ZW = zw_mix2(n7490, n7462, 320u64);
    let n7493: ZW = zw_bits_n(n6131);
    let n7494: ZW = zw_mix1(n7491, n7493, 321u64);
    let n7495: ZW = zw_mix2(n7492, n7493, 321u64);
    let n7496: ZW = zw_mix1(n7361, n7306, 258u64);
    let n7497: ZW = zw_mix2(n7362, n7306, 258u64);
    let n7498: ZW = zw_mix1(n7496, n7365, 265u64);
    let n7499: ZW = zw_mix2(n7497, n7365, 265u64);
    let n7500: ZW = zw_mix1(n7498, n7311, 266u64);
    let n7501: ZW = zw_mix2(n7499, n7311, 266u64);
    let n7502: ZW = zw_mix1(n7500, n7370, 272u64);
    let n7503: ZW = zw_mix2(n7501, n7370, 272u64);
    let n7504: ZW = zw_mix1(n7502, n7373, 308u64);
    let n7505: ZW = zw_mix2(n7503, n7373, 308u64);
    let n7506: ZW = zw_mix1(n7504, n7376, 309u64);
    let n7507: ZW = zw_mix2(n7505, n7376, 309u64);
    let n7508: ZW = zw_mix1(n7506, n7379, 310u64);
    let n7509: ZW = zw_mix2(n7507, n7379, 310u64);
    let n7510: ZW = zw_mix1(n7508, n7382, 311u64);
    let n7511: ZW = zw_mix2(n7509, n7382, 311u64);
    let n7512: ZW = zw_mix1(n7510, n7279, 312u64);
    let n7513: ZW = zw_mix2(n7511, n7279, 312u64);
    let n7514: ZW = zw_bits_n(n6149);
    let n7515: ZW = zw_mix1(n7512, n7514, 320u64);
    let n7516: ZW = zw_mix2(n7513, n7514, 320u64);
    let n7517: ZW = zw_bits_n(n6138);
    let n7518: ZW = zw_mix1(n7515, n7517, 321u64);
    let n7519: ZW = zw_mix2(n7516, n7517, 321u64);
    let n7520: ZW = zw_mix1(n7504, n7393, 309u64);
    let n7521: ZW = zw_mix2(n7505, n7393, 309u64);
    let n7522: ZW = zw_mix1(n7520, n7396, 310u64);
    let n7523: ZW = zw_mix2(n7521, n7396, 310u64);
    let n7524: ZW = zw_mix1(n7522, n7382, 311u64);
    let n7525: ZW = zw_mix2(n7523, n7382, 311u64);
    let n7526: ZW = zw_mix1(n7524, n7288, 312u64);
    let n7527: ZW = zw_mix2(n7525, n7288, 312u64);
    let n7528: ZW = zw_bits_n(n6168);
    let n7529: ZW = zw_mix1(n7526, n7528, 320u64);
    let n7530: ZW = zw_mix2(n7527, n7528, 320u64);
    let n7531: ZW = zw_bits_n(n6157);
    let n7532: ZW = zw_mix1(n7529, n7531, 321u64);
    let n7533: ZW = zw_mix2(n7530, n7531, 321u64);
    let n7534: ZW = zw_mix1(n7520, n7409, 310u64);
    let n7535: ZW = zw_mix2(n7521, n7409, 310u64);
    let n7536: ZW = zw_mix1(n7534, n7382, 311u64);
    let n7537: ZW = zw_mix2(n7535, n7382, 311u64);
    let n7538: ZW = zw_mix1(n7536, n7297, 312u64);
    let n7539: ZW = zw_mix2(n7537, n7297, 312u64);
    let n7540: ZW = zw_bits_n(n6187);
    let n7541: ZW = zw_mix1(n7538, n7540, 320u64);
    let n7542: ZW = zw_mix2(n7539, n7540, 320u64);
    let n7543: ZW = zw_bits_n(n6176);
    let n7544: ZW = zw_mix1(n7541, n7543, 321u64);
    let n7545: ZW = zw_mix2(n7542, n7543, 321u64);
    let n7546: ZW = zw_mix1(n7502, n7422, 308u64);
    let n7547: ZW = zw_mix2(n7503, n7422, 308u64);
    let n7548: ZW = zw_mix1(n7546, n7425, 309u64);
    let n7549: ZW = zw_mix2(n7547, n7425, 309u64);
    let n7550: ZW = zw_mix1(n7548, n7428, 310u64);
    let n7551: ZW = zw_mix2(n7549, n7428, 310u64);
    let n7552: ZW = zw_mix1(n7550, n7431, 311u64);
    let n7553: ZW = zw_mix2(n7551, n7431, 311u64);
    let n7554: ZW = zw_mix1(n7552, n7279, 312u64);
    let n7555: ZW = zw_mix2(n7553, n7279, 312u64);
    let n7556: ZW = zw_bits_n(n6206);
    let n7557: ZW = zw_mix1(n7554, n7556, 320u64);
    let n7558: ZW = zw_mix2(n7555, n7556, 320u64);
    let n7559: ZW = zw_bits_n(n6195);
    let n7560: ZW = zw_mix1(n7557, n7559, 321u64);
    let n7561: ZW = zw_mix2(n7558, n7559, 321u64);
    let n7562: ZW = zw_mix1(n7546, n7393, 309u64);
    let n7563: ZW = zw_mix2(n7547, n7393, 309u64);
    let n7564: ZW = zw_mix1(n7562, n7396, 310u64);
    let n7565: ZW = zw_mix2(n7563, n7396, 310u64);
    let n7566: ZW = zw_mix1(n7564, n7431, 311u64);
    let n7567: ZW = zw_mix2(n7565, n7431, 311u64);
    let n7568: ZW = zw_mix1(n7566, n7288, 312u64);
    let n7569: ZW = zw_mix2(n7567, n7288, 312u64);
    let n7570: ZW = zw_bits_n(n6215);
    let n7571: ZW = zw_mix1(n7568, n7570, 320u64);
    let n7572: ZW = zw_mix2(n7569, n7570, 320u64);
    let n7573: ZW = zw_bits_n(n6213);
    let n7574: ZW = zw_mix1(n7571, n7573, 321u64);
    let n7575: ZW = zw_mix2(n7572, n7573, 321u64);
    let n7576: ZW = zw_mix1(n7562, n7409, 310u64);
    let n7577: ZW = zw_mix2(n7563, n7409, 310u64);
    let n7578: ZW = zw_mix1(n7576, n7431, 311u64);
    let n7579: ZW = zw_mix2(n7577, n7431, 311u64);
    let n7580: ZW = zw_mix1(n7578, n7297, 312u64);
    let n7581: ZW = zw_mix2(n7579, n7297, 312u64);
    let n7582: ZW = zw_bits_n(n6223);
    let n7583: ZW = zw_mix1(n7580, n7582, 320u64);
    let n7584: ZW = zw_mix2(n7581, n7582, 320u64);
    let n7585: ZW = zw_bits_n(n6221);
    let n7586: ZW = zw_mix1(n7583, n7585, 321u64);
    let n7587: ZW = zw_mix2(n7584, n7585, 321u64);
    let n7588: ZW = zw_mix1(n7550, n7468, 311u64);
    let n7589: ZW = zw_mix2(n7551, n7468, 311u64);
    let n7590: ZW = zw_mix1(n7588, n7279, 312u64);
    let n7591: ZW = zw_mix2(n7589, n7279, 312u64);
    let n7592: ZW = zw_mix1(n7590, n7556, 320u64);
    let n7593: ZW = zw_mix2(n7591, n7556, 320u64);
    let n7594: ZW = zw_bits_n(n6226);
    let n7595: ZW = zw_mix1(n7592, n7594, 321u64);
    let n7596: ZW = zw_mix2(n7593, n7594, 321u64);
    let n7597: ZW = zw_mix1(n7564, n7468, 311u64);
    let n7598: ZW = zw_mix2(n7565, n7468, 311u64);
    let n7599: ZW = zw_mix1(n7597, n7288, 312u64);
    let n7600: ZW = zw_mix2(n7598, n7288, 312u64);
    let n7601: ZW = zw_mix1(n7599, n7570, 320u64);
    let n7602: ZW = zw_mix2(n7600, n7570, 320u64);
    let n7603: ZW = zw_bits_n(n6229);
    let n7604: ZW = zw_mix1(n7601, n7603, 321u64);
    let n7605: ZW = zw_mix2(n7602, n7603, 321u64);
    let n7606: ZW = zw_mix1(n7576, n7468, 311u64);
    let n7607: ZW = zw_mix2(n7577, n7468, 311u64);
    let n7608: ZW = zw_mix1(n7606, n7297, 312u64);
    let n7609: ZW = zw_mix2(n7607, n7297, 312u64);
    let n7610: ZW = zw_mix1(n7608, n7582, 320u64);
    let n7611: ZW = zw_mix2(n7609, n7582, 320u64);
    let n7612: ZW = zw_bits_n(n6232);
    let n7613: ZW = zw_mix1(n7610, n7612, 321u64);
    let n7614: ZW = zw_mix2(n7611, n7612, 321u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v0_b0: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v0_b0: u16 = ALL & zb_holds(n1509);
    let ok_v1_b1: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v1_b1: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v1_b1: u16 = ALL & zb_holds(n1591);
    let ok_v2_b2: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v2_b2: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v2_b2: u16 = ALL & zb_holds(n1660);
    let ok_v16_b3: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v16_b3: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v16_b3: u16 = ALL & zb_holds(n1716);
    let ok_v17_b4: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v17_b4: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v17_b4: u16 = ALL & zb_holds(n1771);
    let ok_v18_b5: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v18_b5: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v18_b5: u16 = ALL & zb_holds(n1826);
    let ok_v32_b6: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v32_b6: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v32_b6: u16 = ALL & zb_holds(n1882);
    let ok_v33_b7: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v33_b7: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v33_b7: u16 = ALL & zb_holds(n1914);
    let ok_v34_b8: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v34_b8: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v34_b8: u16 = ALL & zb_holds(n1945);
    let ok_v36_b9: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v36_b9: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v36_b9: u16 = ALL & zb_holds(n1977);
    let ok_v37_b10: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v37_b10: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v37_b10: u16 = ALL & zb_holds(n1914);
    let ok_v38_b11: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v38_b11: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v38_b11: u16 = ALL & zb_holds(n1945);
    let ok_v40_b12: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v40_b12: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v40_b12: u16 = ALL & zb_holds(n1977);
    let ok_v41_b13: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v41_b13: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v41_b13: u16 = ALL & zb_holds(n1914);
    let ok_v42_b14: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v42_b14: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v42_b14: u16 = ALL & zb_holds(n1945);
    let ok_v48_b15: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v48_b15: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v48_b15: u16 = ALL & zb_holds(n2031);
    let ok_v49_b16: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v49_b16: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v49_b16: u16 = ALL & zb_holds(n2061);
    let ok_v50_b17: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v50_b17: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v50_b17: u16 = ALL & zb_holds(n2091);
    let ok_v52_b18: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v52_b18: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v52_b18: u16 = ALL & zb_holds(n2119);
    let ok_v53_b19: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v53_b19: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v53_b19: u16 = ALL & zb_holds(n2061);
    let ok_v54_b20: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v54_b20: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v54_b20: u16 = ALL & zb_holds(n2091);
    let ok_v56_b21: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v56_b21: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v56_b21: u16 = ALL & zb_holds(n2119);
    let ok_v57_b22: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v57_b22: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v57_b22: u16 = ALL & zb_holds(n2061);
    let ok_v58_b23: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v58_b23: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v58_b23: u16 = ALL & zb_holds(n2091);
    let ok_v0_b24: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v0_b24: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v0_b24: u16 = ALL & zb_holds(n2141);
    let ok_v1_b25: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v1_b25: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v1_b25: u16 = ALL & zb_holds(n2149);
    let ok_v2_b26: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v2_b26: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v2_b26: u16 = ALL & zb_holds(n2157);
    let ok_v16_b27: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v16_b27: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v16_b27: u16 = ALL & zb_holds(n2165);
    let ok_v17_b28: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v17_b28: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v17_b28: u16 = ALL & zb_holds(n2173);
    let ok_v18_b29: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v18_b29: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v18_b29: u16 = ALL & zb_holds(n2181);
    let ok_v32_b30: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v32_b30: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v32_b30: u16 = ALL & zb_holds(n2192);
    let ok_v33_b31: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v33_b31: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v33_b31: u16 = ALL & zb_holds(n2202);
    let ok_v34_b32: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v34_b32: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v34_b32: u16 = ALL & zb_holds(n2212);
    let ok_v36_b33: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v36_b33: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v36_b33: u16 = ALL & zb_holds(n2222);
    let ok_v37_b34: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v37_b34: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v37_b34: u16 = ALL & zb_holds(n2202);
    let ok_v38_b35: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v38_b35: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v38_b35: u16 = ALL & zb_holds(n2212);
    let ok_v40_b36: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v40_b36: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v40_b36: u16 = ALL & zb_holds(n2222);
    let ok_v41_b37: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v41_b37: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v41_b37: u16 = ALL & zb_holds(n2202);
    let ok_v42_b38: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v42_b38: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v42_b38: u16 = ALL & zb_holds(n2212);
    let ok_v48_b39: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v48_b39: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v48_b39: u16 = ALL & zb_holds(n2232);
    let ok_v49_b40: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v49_b40: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v49_b40: u16 = ALL & zb_holds(n2242);
    let ok_v50_b41: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v50_b41: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v50_b41: u16 = ALL & zb_holds(n2252);
    let ok_v52_b42: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v52_b42: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v52_b42: u16 = ALL & zb_holds(n2262);
    let ok_v53_b43: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v53_b43: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v53_b43: u16 = ALL & zb_holds(n2242);
    let ok_v54_b44: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v54_b44: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v54_b44: u16 = ALL & zb_holds(n2252);
    let ok_v56_b45: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v56_b45: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v56_b45: u16 = ALL & zb_holds(n2262);
    let ok_v57_b46: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v57_b46: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v57_b46: u16 = ALL & zb_holds(n2242);
    let ok_v58_b47: u16 = ALL & zb_holds(n1337) & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73);
    let bd_v58_b47: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v58_b47: u16 = ALL & zb_holds(n2252);
    let ok_v0_b48: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2342);
    let bd_v0_b48: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v0_b48: u16 = ALL & zb_holds(n111) & zb_holds(n1468) & zb_holds(n2394);
    let ok_v1_b49: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2342);
    let bd_v1_b49: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v1_b49: u16 = ALL & zb_holds(n111) & zb_holds(n1468) & zb_holds(n2432);
    let ok_v2_b50: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2342);
    let bd_v2_b50: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v2_b50: u16 = ALL & zb_holds(n111) & zb_holds(n1468) & zb_holds(n2465);
    let ok_v16_b51: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2342);
    let bd_v16_b51: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v16_b51: u16 = ALL & zb_holds(n111) & zb_holds(n1468) & zb_holds(n2500);
    let ok_v17_b52: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2342);
    let bd_v17_b52: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v17_b52: u16 = ALL & zb_holds(n111) & zb_holds(n1468) & zb_holds(n2535);
    let ok_v18_b53: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2342);
    let bd_v18_b53: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v18_b53: u16 = ALL & zb_holds(n111) & zb_holds(n1468) & zb_holds(n2570);
    let ok_v32_b54: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2342);
    let bd_v32_b54: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v32_b54: u16 = ALL & zb_holds(n2594);
    let ok_v33_b55: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2342);
    let bd_v33_b55: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v33_b55: u16 = ALL & zb_holds(n2604);
    let ok_v34_b56: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2342);
    let bd_v34_b56: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v34_b56: u16 = ALL & zb_holds(n2614);
    let ok_v36_b57: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2342);
    let bd_v36_b57: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v36_b57: u16 = ALL & zb_holds(n2622);
    let ok_v48_b58: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2342);
    let bd_v48_b58: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v48_b58: u16 = ALL & zb_holds(n2644);
    let ok_v49_b59: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2342);
    let bd_v49_b59: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v49_b59: u16 = ALL & zb_holds(n2654);
    let ok_v50_b60: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2342);
    let bd_v50_b60: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v50_b60: u16 = ALL & zb_holds(n2664);
    let ok_v52_b61: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n2342);
    let bd_v52_b61: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v52_b61: u16 = ALL & zb_holds(n2672);
    let ok_v0_b62: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n4313);
    let bd_v0_b62: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v0_b62: u16 = ALL & zb_holds(n111) & zb_holds(n4501) & zb_holds(n4504);
    let ok_v1_b63: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n4313);
    let bd_v1_b63: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v1_b63: u16 = ALL & zb_holds(n111) & zb_holds(n4501) & zb_holds(n4582);
    let ok_v2_b64: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n4313);
    let bd_v2_b64: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v2_b64: u16 = ALL & zb_holds(n111) & zb_holds(n4501) & zb_holds(n4657);
    let ok_v16_b65: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n4313);
    let bd_v16_b65: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v16_b65: u16 = ALL & zb_holds(n111) & zb_holds(n4501) & zb_holds(n4717);
    let ok_v17_b66: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n4313);
    let bd_v17_b66: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v17_b66: u16 = ALL & zb_holds(n111) & zb_holds(n4501) & zb_holds(n4777);
    let ok_v18_b67: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n4313);
    let bd_v18_b67: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v18_b67: u16 = ALL & zb_holds(n111) & zb_holds(n4501) & zb_holds(n4837);
    let ok_v32_b68: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n4313);
    let bd_v32_b68: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v32_b68: u16 = ALL & zb_holds(n4870);
    let ok_v33_b69: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n4313);
    let bd_v33_b69: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v33_b69: u16 = ALL & zb_holds(n4881);
    let ok_v34_b70: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n4313);
    let bd_v34_b70: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v34_b70: u16 = ALL & zb_holds(n4892);
    let ok_v36_b71: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n4313);
    let bd_v36_b71: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v36_b71: u16 = ALL & zb_holds(n4901);
    let ok_v48_b72: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n4313);
    let bd_v48_b72: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v48_b72: u16 = ALL & zb_holds(n4924);
    let ok_v49_b73: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n4313);
    let bd_v49_b73: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v49_b73: u16 = ALL & zb_holds(n4935);
    let ok_v50_b74: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n4313);
    let bd_v50_b74: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v50_b74: u16 = ALL & zb_holds(n4946);
    let ok_v52_b75: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n4313);
    let bd_v52_b75: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v52_b75: u16 = ALL & zb_holds(n4955);
    let ok_v0_b76: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5050);
    let bd_v0_b76: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v0_b76: u16 = ALL & zb_holds(n111) & zb_holds(n5049);
    let ok_v1_b77: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5121);
    let bd_v1_b77: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v1_b77: u16 = ALL & zb_holds(n111) & zb_holds(n5120);
    let ok_v2_b78: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5192);
    let bd_v2_b78: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v2_b78: u16 = ALL & zb_holds(n111) & zb_holds(n5191);
    let ok_v16_b79: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5274);
    let bd_v16_b79: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v16_b79: u16 = ALL & zb_holds(n111) & zb_holds(n5273);
    let ok_v17_b80: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5356);
    let bd_v17_b80: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v17_b80: u16 = ALL & zb_holds(n111) & zb_holds(n5355);
    let ok_v18_b81: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5438);
    let bd_v18_b81: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v18_b81: u16 = ALL & zb_holds(n111) & zb_holds(n5437);
    let ok_v32_b82: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5482);
    let bd_v32_b82: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v32_b82: u16 = ALL & zb_holds(n5487);
    let ok_v33_b83: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5518);
    let bd_v33_b83: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v33_b83: u16 = ALL & zb_holds(n5523);
    let ok_v34_b84: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5554);
    let bd_v34_b84: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v34_b84: u16 = ALL & zb_holds(n5559);
    let ok_v36_b85: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5588);
    let bd_v36_b85: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v36_b85: u16 = ALL & zb_holds(n5593);
    let ok_v48_b86: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5636);
    let bd_v48_b86: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v48_b86: u16 = ALL & zb_holds(n5641);
    let ok_v49_b87: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5672);
    let bd_v49_b87: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v49_b87: u16 = ALL & zb_holds(n5677);
    let ok_v50_b88: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5708);
    let bd_v50_b88: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v50_b88: u16 = ALL & zb_holds(n5713);
    let ok_v52_b89: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5742);
    let bd_v52_b89: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v52_b89: u16 = ALL & zb_holds(n5747);
    let ok_v0_b90: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v0_b90: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v0_b90: u16 = ALL & zb_holds(n5863);
    let ok_v1_b91: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v1_b91: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v1_b91: u16 = ALL & zb_holds(n5891);
    let ok_v2_b92: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v2_b92: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v2_b92: u16 = ALL & zb_holds(n5918);
    let ok_v16_b93: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v16_b93: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v16_b93: u16 = ALL & zb_holds(n5941);
    let ok_v17_b94: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v17_b94: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v17_b94: u16 = ALL & zb_holds(n5960);
    let ok_v18_b95: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v18_b95: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v18_b95: u16 = ALL & zb_holds(n5979);
    let ok_v32_b96: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v32_b96: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v32_b96: u16 = ALL & zb_holds(n6025);
    let ok_v33_b97: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v33_b97: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v33_b97: u16 = ALL & zb_holds(n6050);
    let ok_v34_b98: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v34_b98: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v34_b98: u16 = ALL & zb_holds(n6072);
    let ok_v36_b99: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v36_b99: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v36_b99: u16 = ALL & zb_holds(n6103);
    let ok_v37_b100: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v37_b100: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v37_b100: u16 = ALL & zb_holds(n6050);
    let ok_v38_b101: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v38_b101: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v38_b101: u16 = ALL & zb_holds(n6072);
    let ok_v40_b102: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v40_b102: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v40_b102: u16 = ALL & zb_holds(n6103);
    let ok_v41_b103: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v41_b103: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v41_b103: u16 = ALL & zb_holds(n6050);
    let ok_v42_b104: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v42_b104: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v42_b104: u16 = ALL & zb_holds(n6072);
    let ok_v48_b105: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v48_b105: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v48_b105: u16 = ALL & zb_holds(n6150);
    let ok_v49_b106: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v49_b106: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v49_b106: u16 = ALL & zb_holds(n6169);
    let ok_v50_b107: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v50_b107: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v50_b107: u16 = ALL & zb_holds(n6188);
    let ok_v52_b108: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v52_b108: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v52_b108: u16 = ALL & zb_holds(n6207);
    let ok_v53_b109: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v53_b109: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v53_b109: u16 = ALL & zb_holds(n6169);
    let ok_v54_b110: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v54_b110: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v54_b110: u16 = ALL & zb_holds(n6188);
    let ok_v56_b111: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v56_b111: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v56_b111: u16 = ALL & zb_holds(n6207);
    let ok_v57_b112: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v57_b112: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v57_b112: u16 = ALL & zb_holds(n6169);
    let ok_v58_b113: u16 = ALL & zb_holds(n139) & zb_holds(n138) & zb_holds(r_c268) & zb_holds(n133) & zb_holds(r_c251) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c243) & zb_holds(n79) & zb_holds(n78) & zb_holds(n75) & zb_holds(n74) & zb_holds(r_c234) & zb_holds(n72) & zb_holds(n73) & zb_holds(n5841);
    let bd_v58_b113: bool = !n137 || !n136 || !n135 || !n134 || !n77 || !n76 || !n109 || !n108;
    let live_v58_b113: u16 = ALL & zb_holds(n6188);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n90,
        c86: n144,
        c280: n463,
        c281: n689,
        c256: n688,
        c85: n145,
    };
    let sh1 = KShared1 {
        c87: r_c87,
        c39: r_c39,
        c84: n90,
        c86: n144,
        c310: n463,
        c311: n689,
        c256: n688,
        c85: n145,
    };
    let sh2 = KShared2 {
        c87: n2400,
        c84: n90,
        c86: n144,
        c85: n145,
    };
    let sh3 = KShared3 {
        c87: n4508,
        c84: n90,
        c86: n144,
        c85: n145,
    };
    let sh4 = KShared4 {
        c84: n90,
        c86: n144,
        c85: n145,
    };
    let sh5 = KShared5 {
        c87: r_c87,
        c39: r_c39,
        c84: n90,
        c86: n144,
        c318: n5836,
        c319: n5837,
        c273: n5834,
        c85: n145,
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
    let mut take_1_14: u16 = 0;
    let mut take_1_15: u16 = 0;
    let mut take_1_16: u16 = 0;
    let mut take_1_17: u16 = 0;
    let mut take_1_18: u16 = 0;
    let mut take_1_19: u16 = 0;
    let mut take_1_20: u16 = 0;
    let mut take_1_21: u16 = 0;
    let mut take_1_22: u16 = 0;
    let mut take_1_23: u16 = 0;
    let mut take_2_0: u16 = 0;
    let mut take_2_1: u16 = 0;
    let mut take_3_0: u16 = 0;
    let mut take_3_1: u16 = 0;
    let mut take_4_0: u16 = 0;
    let mut take_4_1: u16 = 0;
    let mut take_4_2: u16 = 0;
    let mut take_4_3: u16 = 0;
    let mut take_4_4: u16 = 0;
    let mut take_4_5: u16 = 0;
    let mut take_4_6: u16 = 0;
    let mut take_4_7: u16 = 0;
    let mut take_4_8: u16 = 0;
    let mut take_4_9: u16 = 0;
    let mut take_4_10: u16 = 0;
    let mut take_4_11: u16 = 0;
    let mut take_4_12: u16 = 0;
    let mut take_4_13: u16 = 0;
    let mut take_5_0: u16 = 0;
    let mut take_5_1: u16 = 0;
    let mut take_5_2: u16 = 0;
    let mut take_5_3: u16 = 0;
    let mut take_5_4: u16 = 0;
    let mut take_5_5: u16 = 0;
    let mut take_5_6: u16 = 0;
    let mut take_5_7: u16 = 0;
    let mut take_5_8: u16 = 0;
    let mut take_5_9: u16 = 0;
    let mut take_5_10: u16 = 0;
    let mut take_5_11: u16 = 0;
    let mut take_5_12: u16 = 0;
    let mut take_5_13: u16 = 0;
    let mut take_5_14: u16 = 0;
    let mut take_5_15: u16 = 0;
    let mut take_5_16: u16 = 0;
    let mut take_5_17: u16 = 0;
    let mut take_5_18: u16 = 0;
    let mut take_5_19: u16 = 0;
    let mut take_5_20: u16 = 0;
    let mut take_5_21: u16 = 0;
    let mut take_5_22: u16 = 0;
    let mut take_5_23: u16 = 0;
    // 114 distinct button assignments; per outcome they fall
    // into [24, 24, 2, 2, 14, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c308,
        c271: r_c309,
        c236: n249,
        c272: r_c310,
        c273: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c274: n1416,
        c241: n1358,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n1511,
        c283: n1430,
        c255: n1510,
        h1: n6301, h2: n6302,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c308,
        c271: r_c309,
        c236: n249,
        c272: r_c310,
        c273: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c274: n1537,
        c241: n1358,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n1592,
        c283: n1561,
        c255: n1510,
        h1: n6310, h2: n6311,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c308,
        c271: r_c309,
        c236: n249,
        c272: r_c310,
        c273: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c274: n1606,
        c241: n1358,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n1661,
        c283: n1630,
        c255: n1510,
        h1: n6319, h2: n6320,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c308,
        c271: r_c309,
        c236: n249,
        c272: r_c310,
        c273: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c274: n1416,
        c241: n1688,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n1717,
        c283: n1690,
        c255: n1510,
        h1: n6345, h2: n6346,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c308,
        c271: r_c309,
        c236: n249,
        c272: r_c310,
        c273: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c274: n1537,
        c241: n1688,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n1772,
        c283: n1745,
        c255: n1510,
        h1: n6353, h2: n6354,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c308,
        c271: r_c309,
        c236: n249,
        c272: r_c310,
        c273: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c274: n1606,
        c241: n1688,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n1827,
        c283: n1800,
        c255: n1510,
        h1: n6361, h2: n6362,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1848,
        c271: n1849,
        c236: n1846,
        c272: n1850,
        c273: n1851,
        c238: n1847,
        c274: n1416,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1881,
        c283: n1853,
        c255: n1880,
        h1: n6402, h2: n6403,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1848,
        c271: n1885,
        c236: n1846,
        c272: n1886,
        c273: n1851,
        c238: n1847,
        c274: n1537,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1913,
        c283: n1888,
        c255: n1880,
        h1: n6418, h2: n6419,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1848,
        c271: n1885,
        c236: n1846,
        c272: n1917,
        c273: n1851,
        c238: n1847,
        c274: n1606,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1944,
        c283: n1919,
        c255: n1880,
        h1: n6431, h2: n6432,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1946,
        c271: n1947,
        c236: n1846,
        c272: n1948,
        c273: n1949,
        c238: n1847,
        c274: n1416,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1976,
        c283: n1951,
        c255: n1880,
        h1: n6451, h2: n6452,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1946,
        c271: n1885,
        c236: n1846,
        c272: n1886,
        c273: n1949,
        c238: n1847,
        c274: n1537,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1981,
        c283: n1979,
        c255: n1880,
        h1: n6465, h2: n6466,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1946,
        c271: n1885,
        c236: n1846,
        c272: n1917,
        c273: n1949,
        c238: n1847,
        c274: n1606,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1985,
        c283: n1983,
        c255: n1880,
        h1: n6477, h2: n6478,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1946,
        c271: n1947,
        c236: n1846,
        c272: n1948,
        c273: n1986,
        c238: n1847,
        c274: n1416,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1976,
        c283: n1987,
        c255: n1880,
        h1: n6487, h2: n6488,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1946,
        c271: n1885,
        c236: n1846,
        c272: n1886,
        c273: n1986,
        c238: n1847,
        c274: n1537,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1981,
        c283: n1988,
        c255: n1880,
        h1: n6496, h2: n6497,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1946,
        c271: n1885,
        c236: n1846,
        c272: n1917,
        c273: n1986,
        c238: n1847,
        c274: n1606,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1985,
        c283: n1989,
        c255: n1880,
        h1: n6505, h2: n6506,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1848,
        c271: n1849,
        c236: n1846,
        c272: n1850,
        c273: n1851,
        c238: n1847,
        c274: n1416,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n2030,
        c283: n2005,
        c255: n1880,
        h1: n6529, h2: n6530,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1848,
        c271: n1885,
        c236: n1846,
        c272: n1886,
        c273: n1851,
        c238: n1847,
        c274: n1537,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n2060,
        c283: n2035,
        c255: n1880,
        h1: n6543, h2: n6544,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1848,
        c271: n1885,
        c236: n1846,
        c272: n1917,
        c273: n1851,
        c238: n1847,
        c274: n1606,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n2090,
        c283: n2065,
        c255: n1880,
        h1: n6555, h2: n6556,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1946,
        c271: n1947,
        c236: n1846,
        c272: n1948,
        c273: n1949,
        c238: n1847,
        c274: n1416,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n2118,
        c283: n2093,
        c255: n1880,
        h1: n6571, h2: n6572,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1946,
        c271: n1885,
        c236: n1846,
        c272: n1886,
        c273: n1949,
        c238: n1847,
        c274: n1537,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n2123,
        c283: n2121,
        c255: n1880,
        h1: n6585, h2: n6586,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1946,
        c271: n1885,
        c236: n1846,
        c272: n1917,
        c273: n1949,
        c238: n1847,
        c274: n1606,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n2127,
        c283: n2125,
        c255: n1880,
        h1: n6597, h2: n6598,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1946,
        c271: n1947,
        c236: n1846,
        c272: n1948,
        c273: n1986,
        c238: n1847,
        c274: n1416,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n2118,
        c283: n2128,
        c255: n1880,
        h1: n6606, h2: n6607,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1946,
        c271: n1885,
        c236: n1846,
        c272: n1886,
        c273: n1986,
        c238: n1847,
        c274: n1537,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n2123,
        c283: n2129,
        c255: n1880,
        h1: n6615, h2: n6616,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1844,
        c41: n1845,
        c270: n1946,
        c271: n1885,
        c236: n1846,
        c272: n1917,
        c273: n1986,
        c238: n1847,
        c274: n1606,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n2127,
        c283: n2130,
        c255: n1880,
        h1: n6624, h2: n6625,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c300: r_c308,
        c301: r_c309,
        c236: n249,
        c302: r_c310,
        c303: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c239: n1357,
        c304: n1416,
        c241: n1358,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c312: n1511,
        c313: n1430,
        c255: n1510,
        h1: n6661, h2: n6662,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v1_b25 & (if bd_v1_b25 { ALL } else { !ok_v1_b25 });
    take_1_1 |= live_v1_b25 & ok_v1_b25 & (if bd_v1_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c300: r_c308,
        c301: r_c309,
        c236: n249,
        c302: r_c310,
        c303: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c239: n1357,
        c304: n1537,
        c241: n1358,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c312: n1592,
        c313: n1561,
        c255: n1510,
        h1: n6667, h2: n6668,
    };
    // body 25: buttons 0x01, forks 0x0
    sink.o1(1, take_1_1, &sh1, &o1);
    declined |= live_v2_b26 & (if bd_v2_b26 { ALL } else { !ok_v2_b26 });
    take_1_2 |= live_v2_b26 & ok_v2_b26 & (if bd_v2_b26 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c300: r_c308,
        c301: r_c309,
        c236: n249,
        c302: r_c310,
        c303: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c239: n1357,
        c304: n1606,
        c241: n1358,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c312: n1661,
        c313: n1630,
        c255: n1510,
        h1: n6673, h2: n6674,
    };
    // body 26: buttons 0x02, forks 0x0
    sink.o1(2, take_1_2, &sh1, &o1);
    declined |= live_v16_b27 & (if bd_v16_b27 { ALL } else { !ok_v16_b27 });
    take_1_3 |= live_v16_b27 & ok_v16_b27 & (if bd_v16_b27 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c300: r_c308,
        c301: r_c309,
        c236: n249,
        c302: r_c310,
        c303: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c239: n1357,
        c304: n1416,
        c241: n1688,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c312: n1717,
        c313: n1690,
        c255: n1510,
        h1: n6695, h2: n6696,
    };
    // body 27: buttons 0x10, forks 0x0
    sink.o1(16, take_1_3, &sh1, &o1);
    declined |= live_v17_b28 & (if bd_v17_b28 { ALL } else { !ok_v17_b28 });
    take_1_4 |= live_v17_b28 & ok_v17_b28 & (if bd_v17_b28 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c300: r_c308,
        c301: r_c309,
        c236: n249,
        c302: r_c310,
        c303: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c239: n1357,
        c304: n1537,
        c241: n1688,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c312: n1772,
        c313: n1745,
        c255: n1510,
        h1: n6701, h2: n6702,
    };
    // body 28: buttons 0x11, forks 0x0
    sink.o1(17, take_1_4, &sh1, &o1);
    declined |= live_v18_b29 & (if bd_v18_b29 { ALL } else { !ok_v18_b29 });
    take_1_5 |= live_v18_b29 & ok_v18_b29 & (if bd_v18_b29 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c300: r_c308,
        c301: r_c309,
        c236: n249,
        c302: r_c310,
        c303: r_c311,
        c238: zn_splat(P8::from_raw(-65536i32)),
        c239: n1357,
        c304: n1606,
        c241: n1688,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c312: n1827,
        c313: n1800,
        c255: n1510,
        h1: n6707, h2: n6708,
    };
    // body 29: buttons 0x12, forks 0x0
    sink.o1(18, take_1_5, &sh1, &o1);
    declined |= live_v32_b30 & (if bd_v32_b30 { ALL } else { !ok_v32_b30 });
    take_1_6 |= live_v32_b30 & ok_v32_b30 & (if bd_v32_b30 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1848,
        c301: n1849,
        c236: n1846,
        c302: n1850,
        c303: n1851,
        c238: n1847,
        c239: n2182,
        c304: n1416,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n1881,
        c313: n1853,
        c255: n1880,
        h1: n6740, h2: n6741,
    };
    // body 30: buttons 0x20, forks 0x0
    sink.o1(32, take_1_6, &sh1, &o1);
    declined |= live_v33_b31 & (if bd_v33_b31 { ALL } else { !ok_v33_b31 });
    take_1_7 |= live_v33_b31 & ok_v33_b31 & (if bd_v33_b31 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1848,
        c301: n1885,
        c236: n1846,
        c302: n1886,
        c303: n1851,
        c238: n1847,
        c239: n2182,
        c304: n1537,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n1913,
        c313: n1888,
        c255: n1880,
        h1: n6752, h2: n6753,
    };
    // body 31: buttons 0x21, forks 0x0
    sink.o1(33, take_1_7, &sh1, &o1);
    declined |= live_v34_b32 & (if bd_v34_b32 { ALL } else { !ok_v34_b32 });
    take_1_8 |= live_v34_b32 & ok_v34_b32 & (if bd_v34_b32 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1848,
        c301: n1885,
        c236: n1846,
        c302: n1917,
        c303: n1851,
        c238: n1847,
        c239: n2182,
        c304: n1606,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n1944,
        c313: n1919,
        c255: n1880,
        h1: n6762, h2: n6763,
    };
    // body 32: buttons 0x22, forks 0x0
    sink.o1(34, take_1_8, &sh1, &o1);
    declined |= live_v36_b33 & (if bd_v36_b33 { ALL } else { !ok_v36_b33 });
    take_1_9 |= live_v36_b33 & ok_v36_b33 & (if bd_v36_b33 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1946,
        c301: n1947,
        c236: n1846,
        c302: n1948,
        c303: n1949,
        c238: n1847,
        c239: n2182,
        c304: n1416,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n1976,
        c313: n1951,
        c255: n1880,
        h1: n6776, h2: n6777,
    };
    // body 33: buttons 0x24, forks 0x0
    sink.o1(36, take_1_9, &sh1, &o1);
    declined |= live_v37_b34 & (if bd_v37_b34 { ALL } else { !ok_v37_b34 });
    take_1_10 |= live_v37_b34 & ok_v37_b34 & (if bd_v37_b34 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1946,
        c301: n1885,
        c236: n1846,
        c302: n1886,
        c303: n1949,
        c238: n1847,
        c239: n2182,
        c304: n1537,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n1981,
        c313: n1979,
        c255: n1880,
        h1: n6788, h2: n6789,
    };
    // body 34: buttons 0x25, forks 0x0
    sink.o1(37, take_1_10, &sh1, &o1);
    declined |= live_v38_b35 & (if bd_v38_b35 { ALL } else { !ok_v38_b35 });
    take_1_11 |= live_v38_b35 & ok_v38_b35 & (if bd_v38_b35 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1946,
        c301: n1885,
        c236: n1846,
        c302: n1917,
        c303: n1949,
        c238: n1847,
        c239: n2182,
        c304: n1606,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n1985,
        c313: n1983,
        c255: n1880,
        h1: n6798, h2: n6799,
    };
    // body 35: buttons 0x26, forks 0x0
    sink.o1(38, take_1_11, &sh1, &o1);
    declined |= live_v40_b36 & (if bd_v40_b36 { ALL } else { !ok_v40_b36 });
    take_1_12 |= live_v40_b36 & ok_v40_b36 & (if bd_v40_b36 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1946,
        c301: n1947,
        c236: n1846,
        c302: n1948,
        c303: n1986,
        c238: n1847,
        c239: n2182,
        c304: n1416,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n1976,
        c313: n1987,
        c255: n1880,
        h1: n6806, h2: n6807,
    };
    // body 36: buttons 0x28, forks 0x0
    sink.o1(40, take_1_12, &sh1, &o1);
    declined |= live_v41_b37 & (if bd_v41_b37 { ALL } else { !ok_v41_b37 });
    take_1_13 |= live_v41_b37 & ok_v41_b37 & (if bd_v41_b37 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1946,
        c301: n1885,
        c236: n1846,
        c302: n1886,
        c303: n1986,
        c238: n1847,
        c239: n2182,
        c304: n1537,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n1981,
        c313: n1988,
        c255: n1880,
        h1: n6814, h2: n6815,
    };
    // body 37: buttons 0x29, forks 0x0
    sink.o1(41, take_1_13, &sh1, &o1);
    declined |= live_v42_b38 & (if bd_v42_b38 { ALL } else { !ok_v42_b38 });
    take_1_14 |= live_v42_b38 & ok_v42_b38 & (if bd_v42_b38 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1946,
        c301: n1885,
        c236: n1846,
        c302: n1917,
        c303: n1986,
        c238: n1847,
        c239: n2182,
        c304: n1606,
        c241: n1358,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c312: n1985,
        c313: n1989,
        c255: n1880,
        h1: n6822, h2: n6823,
    };
    // body 38: buttons 0x2a, forks 0x0
    sink.o1(42, take_1_14, &sh1, &o1);
    declined |= live_v48_b39 & (if bd_v48_b39 { ALL } else { !ok_v48_b39 });
    take_1_15 |= live_v48_b39 & ok_v48_b39 & (if bd_v48_b39 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1848,
        c301: n1849,
        c236: n1846,
        c302: n1850,
        c303: n1851,
        c238: n1847,
        c239: n2182,
        c304: n1416,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n2030,
        c313: n2005,
        c255: n1880,
        h1: n6844, h2: n6845,
    };
    // body 39: buttons 0x30, forks 0x0
    sink.o1(48, take_1_15, &sh1, &o1);
    declined |= live_v49_b40 & (if bd_v49_b40 { ALL } else { !ok_v49_b40 });
    take_1_16 |= live_v49_b40 & ok_v49_b40 & (if bd_v49_b40 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1848,
        c301: n1885,
        c236: n1846,
        c302: n1886,
        c303: n1851,
        c238: n1847,
        c239: n2182,
        c304: n1537,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n2060,
        c313: n2035,
        c255: n1880,
        h1: n6856, h2: n6857,
    };
    // body 40: buttons 0x31, forks 0x0
    sink.o1(49, take_1_16, &sh1, &o1);
    declined |= live_v50_b41 & (if bd_v50_b41 { ALL } else { !ok_v50_b41 });
    take_1_17 |= live_v50_b41 & ok_v50_b41 & (if bd_v50_b41 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1848,
        c301: n1885,
        c236: n1846,
        c302: n1917,
        c303: n1851,
        c238: n1847,
        c239: n2182,
        c304: n1606,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n2090,
        c313: n2065,
        c255: n1880,
        h1: n6866, h2: n6867,
    };
    // body 41: buttons 0x32, forks 0x0
    sink.o1(50, take_1_17, &sh1, &o1);
    declined |= live_v52_b42 & (if bd_v52_b42 { ALL } else { !ok_v52_b42 });
    take_1_18 |= live_v52_b42 & ok_v52_b42 & (if bd_v52_b42 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1946,
        c301: n1947,
        c236: n1846,
        c302: n1948,
        c303: n1949,
        c238: n1847,
        c239: n2182,
        c304: n1416,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n2118,
        c313: n2093,
        c255: n1880,
        h1: n6880, h2: n6881,
    };
    // body 42: buttons 0x34, forks 0x0
    sink.o1(52, take_1_18, &sh1, &o1);
    declined |= live_v53_b43 & (if bd_v53_b43 { ALL } else { !ok_v53_b43 });
    take_1_19 |= live_v53_b43 & ok_v53_b43 & (if bd_v53_b43 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1946,
        c301: n1885,
        c236: n1846,
        c302: n1886,
        c303: n1949,
        c238: n1847,
        c239: n2182,
        c304: n1537,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n2123,
        c313: n2121,
        c255: n1880,
        h1: n6892, h2: n6893,
    };
    // body 43: buttons 0x35, forks 0x0
    sink.o1(53, take_1_19, &sh1, &o1);
    declined |= live_v54_b44 & (if bd_v54_b44 { ALL } else { !ok_v54_b44 });
    take_1_20 |= live_v54_b44 & ok_v54_b44 & (if bd_v54_b44 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1946,
        c301: n1885,
        c236: n1846,
        c302: n1917,
        c303: n1949,
        c238: n1847,
        c239: n2182,
        c304: n1606,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n2127,
        c313: n2125,
        c255: n1880,
        h1: n6902, h2: n6903,
    };
    // body 44: buttons 0x36, forks 0x0
    sink.o1(54, take_1_20, &sh1, &o1);
    declined |= live_v56_b45 & (if bd_v56_b45 { ALL } else { !ok_v56_b45 });
    take_1_21 |= live_v56_b45 & ok_v56_b45 & (if bd_v56_b45 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1946,
        c301: n1947,
        c236: n1846,
        c302: n1948,
        c303: n1986,
        c238: n1847,
        c239: n2182,
        c304: n1416,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n2118,
        c313: n2128,
        c255: n1880,
        h1: n6910, h2: n6911,
    };
    // body 45: buttons 0x38, forks 0x0
    sink.o1(56, take_1_21, &sh1, &o1);
    declined |= live_v57_b46 & (if bd_v57_b46 { ALL } else { !ok_v57_b46 });
    take_1_22 |= live_v57_b46 & ok_v57_b46 & (if bd_v57_b46 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1946,
        c301: n1885,
        c236: n1846,
        c302: n1886,
        c303: n1986,
        c238: n1847,
        c239: n2182,
        c304: n1537,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n2123,
        c313: n2129,
        c255: n1880,
        h1: n6918, h2: n6919,
    };
    // body 46: buttons 0x39, forks 0x0
    sink.o1(57, take_1_22, &sh1, &o1);
    declined |= live_v58_b47 & (if bd_v58_b47 { ALL } else { !ok_v58_b47 });
    take_1_23 |= live_v58_b47 & ok_v58_b47 & (if bd_v58_b47 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1844,
        c41: n1845,
        c300: n1946,
        c301: n1885,
        c236: n1846,
        c302: n1917,
        c303: n1986,
        c238: n1847,
        c239: n2182,
        c304: n1606,
        c241: n1688,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c312: n2127,
        c313: n2130,
        c255: n1880,
        h1: n6926, h2: n6927,
    };
    // body 47: buttons 0x3a, forks 0x0
    sink.o1(58, take_1_23, &sh1, &o1);
    declined |= live_v0_b48 & (if bd_v0_b48 { ALL } else { !ok_v0_b48 });
    take_2_0 |= live_v0_b48 & ok_v0_b48 & (if bd_v0_b48 { 0 } else { ALL });
    declined |= live_v1_b49 & (if bd_v1_b49 { ALL } else { !ok_v1_b49 });
    take_2_0 |= live_v1_b49 & ok_v1_b49 & (if bd_v1_b49 { 0 } else { ALL });
    declined |= live_v2_b50 & (if bd_v2_b50 { ALL } else { !ok_v2_b50 });
    take_2_0 |= live_v2_b50 & ok_v2_b50 & (if bd_v2_b50 { 0 } else { ALL });
    declined |= live_v16_b51 & (if bd_v16_b51 { ALL } else { !ok_v16_b51 });
    take_2_0 |= live_v16_b51 & ok_v16_b51 & (if bd_v16_b51 { 0 } else { ALL });
    declined |= live_v17_b52 & (if bd_v17_b52 { ALL } else { !ok_v17_b52 });
    take_2_0 |= live_v17_b52 & ok_v17_b52 & (if bd_v17_b52 { 0 } else { ALL });
    declined |= live_v18_b53 & (if bd_v18_b53 { ALL } else { !ok_v18_b53 });
    take_2_0 |= live_v18_b53 & ok_v18_b53 & (if bd_v18_b53 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        c41: r_c41,
        h1: n6939, h2: n6940,
    };
    // body 53: buttons 0x12, forks 0x0
    sink.o2(18, take_2_0, &sh2, &o2);
    declined |= live_v32_b54 & (if bd_v32_b54 { ALL } else { !ok_v32_b54 });
    take_2_1 |= live_v32_b54 & ok_v32_b54 & (if bd_v32_b54 { 0 } else { ALL });
    declined |= live_v33_b55 & (if bd_v33_b55 { ALL } else { !ok_v33_b55 });
    take_2_1 |= live_v33_b55 & ok_v33_b55 & (if bd_v33_b55 { 0 } else { ALL });
    declined |= live_v34_b56 & (if bd_v34_b56 { ALL } else { !ok_v34_b56 });
    take_2_1 |= live_v34_b56 & ok_v34_b56 & (if bd_v34_b56 { 0 } else { ALL });
    declined |= live_v36_b57 & (if bd_v36_b57 { ALL } else { !ok_v36_b57 });
    take_2_1 |= live_v36_b57 & ok_v36_b57 & (if bd_v36_b57 { 0 } else { ALL });
    declined |= live_v48_b58 & (if bd_v48_b58 { ALL } else { !ok_v48_b58 });
    take_2_1 |= live_v48_b58 & ok_v48_b58 & (if bd_v48_b58 { 0 } else { ALL });
    declined |= live_v49_b59 & (if bd_v49_b59 { ALL } else { !ok_v49_b59 });
    take_2_1 |= live_v49_b59 & ok_v49_b59 & (if bd_v49_b59 { 0 } else { ALL });
    declined |= live_v50_b60 & (if bd_v50_b60 { ALL } else { !ok_v50_b60 });
    take_2_1 |= live_v50_b60 & ok_v50_b60 & (if bd_v50_b60 { 0 } else { ALL });
    declined |= live_v52_b61 & (if bd_v52_b61 { ALL } else { !ok_v52_b61 });
    take_2_1 |= live_v52_b61 & ok_v52_b61 & (if bd_v52_b61 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1844,
        c41: n1845,
        h1: n6943, h2: n6944,
    };
    // body 61: buttons 0x34, forks 0x0
    sink.o2(52, take_2_1, &sh2, &o2);
    declined |= live_v0_b62 & (if bd_v0_b62 { ALL } else { !ok_v0_b62 });
    take_3_0 |= live_v0_b62 & ok_v0_b62 & (if bd_v0_b62 { 0 } else { ALL });
    declined |= live_v1_b63 & (if bd_v1_b63 { ALL } else { !ok_v1_b63 });
    take_3_0 |= live_v1_b63 & ok_v1_b63 & (if bd_v1_b63 { 0 } else { ALL });
    declined |= live_v2_b64 & (if bd_v2_b64 { ALL } else { !ok_v2_b64 });
    take_3_0 |= live_v2_b64 & ok_v2_b64 & (if bd_v2_b64 { 0 } else { ALL });
    declined |= live_v16_b65 & (if bd_v16_b65 { ALL } else { !ok_v16_b65 });
    take_3_0 |= live_v16_b65 & ok_v16_b65 & (if bd_v16_b65 { 0 } else { ALL });
    declined |= live_v17_b66 & (if bd_v17_b66 { ALL } else { !ok_v17_b66 });
    take_3_0 |= live_v17_b66 & ok_v17_b66 & (if bd_v17_b66 { 0 } else { ALL });
    declined |= live_v18_b67 & (if bd_v18_b67 { ALL } else { !ok_v18_b67 });
    take_3_0 |= live_v18_b67 & ok_v18_b67 & (if bd_v18_b67 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: r_c20,
        c41: r_c41,
        h1: n6950, h2: n6951,
    };
    // body 67: buttons 0x12, forks 0x0
    sink.o3(18, take_3_0, &sh3, &o3);
    declined |= live_v32_b68 & (if bd_v32_b68 { ALL } else { !ok_v32_b68 });
    take_3_1 |= live_v32_b68 & ok_v32_b68 & (if bd_v32_b68 { 0 } else { ALL });
    declined |= live_v33_b69 & (if bd_v33_b69 { ALL } else { !ok_v33_b69 });
    take_3_1 |= live_v33_b69 & ok_v33_b69 & (if bd_v33_b69 { 0 } else { ALL });
    declined |= live_v34_b70 & (if bd_v34_b70 { ALL } else { !ok_v34_b70 });
    take_3_1 |= live_v34_b70 & ok_v34_b70 & (if bd_v34_b70 { 0 } else { ALL });
    declined |= live_v36_b71 & (if bd_v36_b71 { ALL } else { !ok_v36_b71 });
    take_3_1 |= live_v36_b71 & ok_v36_b71 & (if bd_v36_b71 { 0 } else { ALL });
    declined |= live_v48_b72 & (if bd_v48_b72 { ALL } else { !ok_v48_b72 });
    take_3_1 |= live_v48_b72 & ok_v48_b72 & (if bd_v48_b72 { 0 } else { ALL });
    declined |= live_v49_b73 & (if bd_v49_b73 { ALL } else { !ok_v49_b73 });
    take_3_1 |= live_v49_b73 & ok_v49_b73 & (if bd_v49_b73 { 0 } else { ALL });
    declined |= live_v50_b74 & (if bd_v50_b74 { ALL } else { !ok_v50_b74 });
    take_3_1 |= live_v50_b74 & ok_v50_b74 & (if bd_v50_b74 { 0 } else { ALL });
    declined |= live_v52_b75 & (if bd_v52_b75 { ALL } else { !ok_v52_b75 });
    take_3_1 |= live_v52_b75 & ok_v52_b75 & (if bd_v52_b75 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n4844,
        c41: n4845,
        h1: n6956, h2: n6957,
    };
    // body 75: buttons 0x34, forks 0x0
    sink.o3(52, take_3_1, &sh3, &o3);
    declined |= live_v0_b76 & (if bd_v0_b76 { ALL } else { !ok_v0_b76 });
    take_4_0 |= live_v0_b76 & ok_v0_b76 & (if bd_v0_b76 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n5056,
        c39: n5057,
        c20: r_c20,
        c234: n5046,
        c246: n5047,
        c250: n5048,
        c38: n5045,
        h1: n6976, h2: n6977,
    };
    // body 76: buttons 0x00, forks 0x0
    sink.o4(0, take_4_0, &sh4, &o4);
    declined |= live_v1_b77 & (if bd_v1_b77 { ALL } else { !ok_v1_b77 });
    take_4_1 |= live_v1_b77 & ok_v1_b77 & (if bd_v1_b77 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n5127,
        c39: n5128,
        c20: r_c20,
        c234: n5117,
        c246: n5118,
        c250: n5119,
        c38: n5116,
        h1: n6994, h2: n6995,
    };
    // body 77: buttons 0x01, forks 0x0
    sink.o4(1, take_4_1, &sh4, &o4);
    declined |= live_v2_b78 & (if bd_v2_b78 { ALL } else { !ok_v2_b78 });
    take_4_2 |= live_v2_b78 & ok_v2_b78 & (if bd_v2_b78 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n5198,
        c39: n5199,
        c20: r_c20,
        c234: n5188,
        c246: n5189,
        c250: n5190,
        c38: n5187,
        h1: n7012, h2: n7013,
    };
    // body 78: buttons 0x02, forks 0x0
    sink.o4(2, take_4_2, &sh4, &o4);
    declined |= live_v16_b79 & (if bd_v16_b79 { ALL } else { !ok_v16_b79 });
    take_4_3 |= live_v16_b79 & ok_v16_b79 & (if bd_v16_b79 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n5280,
        c39: n5281,
        c20: r_c20,
        c234: n5270,
        c246: n5271,
        c250: n5272,
        c38: n5269,
        h1: n7030, h2: n7031,
    };
    // body 79: buttons 0x10, forks 0x0
    sink.o4(16, take_4_3, &sh4, &o4);
    declined |= live_v17_b80 & (if bd_v17_b80 { ALL } else { !ok_v17_b80 });
    take_4_4 |= live_v17_b80 & ok_v17_b80 & (if bd_v17_b80 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n5362,
        c39: n5363,
        c20: r_c20,
        c234: n5352,
        c246: n5353,
        c250: n5354,
        c38: n5351,
        h1: n7048, h2: n7049,
    };
    // body 80: buttons 0x11, forks 0x0
    sink.o4(17, take_4_4, &sh4, &o4);
    declined |= live_v18_b81 & (if bd_v18_b81 { ALL } else { !ok_v18_b81 });
    take_4_5 |= live_v18_b81 & ok_v18_b81 & (if bd_v18_b81 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n5444,
        c39: n5445,
        c20: r_c20,
        c234: n5434,
        c246: n5435,
        c250: n5436,
        c38: n5433,
        h1: n7066, h2: n7067,
    };
    // body 81: buttons 0x12, forks 0x0
    sink.o4(18, take_4_5, &sh4, &o4);
    declined |= live_v32_b82 & (if bd_v32_b82 { ALL } else { !ok_v32_b82 });
    take_4_6 |= live_v32_b82 & ok_v32_b82 & (if bd_v32_b82 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n5492,
        c39: n5493,
        c20: n5476,
        c234: n5478,
        c246: n5479,
        c250: n5480,
        c38: n5477,
        h1: n7087, h2: n7088,
    };
    // body 82: buttons 0x20, forks 0x0
    sink.o4(32, take_4_6, &sh4, &o4);
    declined |= live_v33_b83 & (if bd_v33_b83 { ALL } else { !ok_v33_b83 });
    take_4_7 |= live_v33_b83 & ok_v33_b83 & (if bd_v33_b83 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n5528,
        c39: n5529,
        c20: n5512,
        c234: n5514,
        c246: n5515,
        c250: n5516,
        c38: n5513,
        h1: n7108, h2: n7109,
    };
    // body 83: buttons 0x21, forks 0x0
    sink.o4(33, take_4_7, &sh4, &o4);
    declined |= live_v34_b84 & (if bd_v34_b84 { ALL } else { !ok_v34_b84 });
    take_4_8 |= live_v34_b84 & ok_v34_b84 & (if bd_v34_b84 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n5564,
        c39: n5565,
        c20: n5548,
        c234: n5550,
        c246: n5551,
        c250: n5552,
        c38: n5549,
        h1: n7129, h2: n7130,
    };
    // body 84: buttons 0x22, forks 0x0
    sink.o4(34, take_4_8, &sh4, &o4);
    declined |= live_v36_b85 & (if bd_v36_b85 { ALL } else { !ok_v36_b85 });
    take_4_9 |= live_v36_b85 & ok_v36_b85 & (if bd_v36_b85 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n5598,
        c39: n5599,
        c20: n5582,
        c234: n5584,
        c246: n5585,
        c250: n5586,
        c38: n5583,
        h1: n7150, h2: n7151,
    };
    // body 85: buttons 0x24, forks 0x0
    sink.o4(36, take_4_9, &sh4, &o4);
    declined |= live_v48_b86 & (if bd_v48_b86 { ALL } else { !ok_v48_b86 });
    take_4_10 |= live_v48_b86 & ok_v48_b86 & (if bd_v48_b86 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n5646,
        c39: n5647,
        c20: n5630,
        c234: n5632,
        c246: n5633,
        c250: n5634,
        c38: n5631,
        h1: n7171, h2: n7172,
    };
    // body 86: buttons 0x30, forks 0x0
    sink.o4(48, take_4_10, &sh4, &o4);
    declined |= live_v49_b87 & (if bd_v49_b87 { ALL } else { !ok_v49_b87 });
    take_4_11 |= live_v49_b87 & ok_v49_b87 & (if bd_v49_b87 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n5682,
        c39: n5683,
        c20: n5666,
        c234: n5668,
        c246: n5669,
        c250: n5670,
        c38: n5667,
        h1: n7192, h2: n7193,
    };
    // body 87: buttons 0x31, forks 0x0
    sink.o4(49, take_4_11, &sh4, &o4);
    declined |= live_v50_b88 & (if bd_v50_b88 { ALL } else { !ok_v50_b88 });
    take_4_12 |= live_v50_b88 & ok_v50_b88 & (if bd_v50_b88 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n5718,
        c39: n5719,
        c20: n5702,
        c234: n5704,
        c246: n5705,
        c250: n5706,
        c38: n5703,
        h1: n7213, h2: n7214,
    };
    // body 88: buttons 0x32, forks 0x0
    sink.o4(50, take_4_12, &sh4, &o4);
    declined |= live_v52_b89 & (if bd_v52_b89 { ALL } else { !ok_v52_b89 });
    take_4_13 |= live_v52_b89 & ok_v52_b89 & (if bd_v52_b89 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n5752,
        c39: n5753,
        c20: n5736,
        c234: n5738,
        c246: n5739,
        c250: n5740,
        c38: n5737,
        h1: n7234, h2: n7235,
    };
    // body 89: buttons 0x34, forks 0x0
    sink.o4(52, take_4_13, &sh4, &o4);
    declined |= live_v0_b90 & (if bd_v0_b90 { ALL } else { !ok_v0_b90 });
    take_5_0 |= live_v0_b90 & ok_v0_b90 & (if bd_v0_b90 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n5828,
        c41: r_c41,
        c308: r_c308,
        c309: r_c309,
        c253: n5829,
        c310: r_c310,
        c311: r_c311,
        c255: n5830,
        c256: n5831,
        c312: n5835,
        c258: n5832,
        c265: n5761,
        c266: n5762,
        c320: n5862,
        c321: n5839,
        c272: n5861,
        h1: n7286, h2: n7287,
    };
    // body 90: buttons 0x00, forks 0x0
    sink.o5(0, take_5_0, &sh5, &o5);
    declined |= live_v1_b91 & (if bd_v1_b91 { ALL } else { !ok_v1_b91 });
    take_5_1 |= live_v1_b91 & ok_v1_b91 & (if bd_v1_b91 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n5828,
        c41: r_c41,
        c308: r_c308,
        c309: r_c309,
        c253: n5829,
        c310: r_c310,
        c311: r_c311,
        c255: n5830,
        c256: n5831,
        c312: n5877,
        c258: n5832,
        c265: n5761,
        c266: n5762,
        c320: n5890,
        c321: n5879,
        c272: n5861,
        h1: n7295, h2: n7296,
    };
    // body 91: buttons 0x01, forks 0x0
    sink.o5(1, take_5_1, &sh5, &o5);
    declined |= live_v2_b92 & (if bd_v2_b92 { ALL } else { !ok_v2_b92 });
    take_5_2 |= live_v2_b92 & ok_v2_b92 & (if bd_v2_b92 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n5828,
        c41: r_c41,
        c308: r_c308,
        c309: r_c309,
        c253: n5829,
        c310: r_c310,
        c311: r_c311,
        c255: n5830,
        c256: n5831,
        c312: n5904,
        c258: n5832,
        c265: n5761,
        c266: n5762,
        c320: n5917,
        c321: n5906,
        c272: n5861,
        h1: n7304, h2: n7305,
    };
    // body 92: buttons 0x02, forks 0x0
    sink.o5(2, take_5_2, &sh5, &o5);
    declined |= live_v16_b93 & (if bd_v16_b93 { ALL } else { !ok_v16_b93 });
    take_5_3 |= live_v16_b93 & ok_v16_b93 & (if bd_v16_b93 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n5828,
        c41: r_c41,
        c308: r_c308,
        c309: r_c309,
        c253: n5829,
        c310: r_c310,
        c311: r_c311,
        c255: n5830,
        c256: n5831,
        c312: n5835,
        c258: n5927,
        c265: n5761,
        c266: n5919,
        c320: n5940,
        c321: n5929,
        c272: n5861,
        h1: n7330, h2: n7331,
    };
    // body 93: buttons 0x10, forks 0x0
    sink.o5(16, take_5_3, &sh5, &o5);
    declined |= live_v17_b94 & (if bd_v17_b94 { ALL } else { !ok_v17_b94 });
    take_5_4 |= live_v17_b94 & ok_v17_b94 & (if bd_v17_b94 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n5828,
        c41: r_c41,
        c308: r_c308,
        c309: r_c309,
        c253: n5829,
        c310: r_c310,
        c311: r_c311,
        c255: n5830,
        c256: n5831,
        c312: n5877,
        c258: n5927,
        c265: n5761,
        c266: n5919,
        c320: n5959,
        c321: n5948,
        c272: n5861,
        h1: n7338, h2: n7339,
    };
    // body 94: buttons 0x11, forks 0x0
    sink.o5(17, take_5_4, &sh5, &o5);
    declined |= live_v18_b95 & (if bd_v18_b95 { ALL } else { !ok_v18_b95 });
    take_5_5 |= live_v18_b95 & ok_v18_b95 & (if bd_v18_b95 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n5828,
        c41: r_c41,
        c308: r_c308,
        c309: r_c309,
        c253: n5829,
        c310: r_c310,
        c311: r_c311,
        c255: n5830,
        c256: n5831,
        c312: n5904,
        c258: n5927,
        c265: n5761,
        c266: n5919,
        c320: n5978,
        c321: n5967,
        c272: n5861,
        h1: n7346, h2: n7347,
    };
    // body 95: buttons 0x12, forks 0x0
    sink.o5(18, take_5_5, &sh5, &o5);
    declined |= live_v32_b96 & (if bd_v32_b96 { ALL } else { !ok_v32_b96 });
    take_5_6 |= live_v32_b96 & ok_v32_b96 & (if bd_v32_b96 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6005,
        c309: n6006,
        c253: n6002,
        c310: n6007,
        c311: n6008,
        c255: n6003,
        c256: n6004,
        c312: n5835,
        c258: n5832,
        c265: n5980,
        c266: n5762,
        c320: n6024,
        c321: n6010,
        c272: n6023,
        h1: n7391, h2: n7392,
    };
    // body 96: buttons 0x20, forks 0x0
    sink.o5(32, take_5_6, &sh5, &o5);
    declined |= live_v33_b97 & (if bd_v33_b97 { ALL } else { !ok_v33_b97 });
    take_5_7 |= live_v33_b97 & ok_v33_b97 & (if bd_v33_b97 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6005,
        c309: n6035,
        c253: n6002,
        c310: n6036,
        c311: n6008,
        c255: n6003,
        c256: n6004,
        c312: n5877,
        c258: n5832,
        c265: n5980,
        c266: n5762,
        c320: n6049,
        c321: n6038,
        c272: n6023,
        h1: n7407, h2: n7408,
    };
    // body 97: buttons 0x21, forks 0x0
    sink.o5(33, take_5_7, &sh5, &o5);
    declined |= live_v34_b98 & (if bd_v34_b98 { ALL } else { !ok_v34_b98 });
    take_5_8 |= live_v34_b98 & ok_v34_b98 & (if bd_v34_b98 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6005,
        c309: n6035,
        c253: n6002,
        c310: n6058,
        c311: n6008,
        c255: n6003,
        c256: n6004,
        c312: n5904,
        c258: n5832,
        c265: n5980,
        c266: n5762,
        c320: n6071,
        c321: n6060,
        c272: n6023,
        h1: n7420, h2: n7421,
    };
    // body 98: buttons 0x22, forks 0x0
    sink.o5(34, take_5_8, &sh5, &o5);
    declined |= live_v36_b99 & (if bd_v36_b99 { ALL } else { !ok_v36_b99 });
    take_5_9 |= live_v36_b99 & ok_v36_b99 & (if bd_v36_b99 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6086,
        c309: n6087,
        c253: n6002,
        c310: n6088,
        c311: n6089,
        c255: n6003,
        c256: n6004,
        c312: n5835,
        c258: n5832,
        c265: n5980,
        c266: n5762,
        c320: n6102,
        c321: n6091,
        c272: n6023,
        h1: n7440, h2: n7441,
    };
    // body 99: buttons 0x24, forks 0x0
    sink.o5(36, take_5_9, &sh5, &o5);
    declined |= live_v37_b100 & (if bd_v37_b100 { ALL } else { !ok_v37_b100 });
    take_5_10 |= live_v37_b100 & ok_v37_b100 & (if bd_v37_b100 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6086,
        c309: n6035,
        c253: n6002,
        c310: n6036,
        c311: n6089,
        c255: n6003,
        c256: n6004,
        c312: n5877,
        c258: n5832,
        c265: n5980,
        c266: n5762,
        c320: n6111,
        c321: n6109,
        c272: n6023,
        h1: n7454, h2: n7455,
    };
    // body 100: buttons 0x25, forks 0x0
    sink.o5(37, take_5_10, &sh5, &o5);
    declined |= live_v38_b101 & (if bd_v38_b101 { ALL } else { !ok_v38_b101 });
    take_5_11 |= live_v38_b101 & ok_v38_b101 & (if bd_v38_b101 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6086,
        c309: n6035,
        c253: n6002,
        c310: n6058,
        c311: n6089,
        c255: n6003,
        c256: n6004,
        c312: n5904,
        c258: n5832,
        c265: n5980,
        c266: n5762,
        c320: n6119,
        c321: n6117,
        c272: n6023,
        h1: n7466, h2: n7467,
    };
    // body 101: buttons 0x26, forks 0x0
    sink.o5(38, take_5_11, &sh5, &o5);
    declined |= live_v40_b102 & (if bd_v40_b102 { ALL } else { !ok_v40_b102 });
    take_5_12 |= live_v40_b102 & ok_v40_b102 & (if bd_v40_b102 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6086,
        c309: n6087,
        c253: n6002,
        c310: n6088,
        c311: n6124,
        c255: n6003,
        c256: n6004,
        c312: n5835,
        c258: n5832,
        c265: n5980,
        c266: n5762,
        c320: n6102,
        c321: n6125,
        c272: n6023,
        h1: n7476, h2: n7477,
    };
    // body 102: buttons 0x28, forks 0x0
    sink.o5(40, take_5_12, &sh5, &o5);
    declined |= live_v41_b103 & (if bd_v41_b103 { ALL } else { !ok_v41_b103 });
    take_5_13 |= live_v41_b103 & ok_v41_b103 & (if bd_v41_b103 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6086,
        c309: n6035,
        c253: n6002,
        c310: n6036,
        c311: n6124,
        c255: n6003,
        c256: n6004,
        c312: n5877,
        c258: n5832,
        c265: n5980,
        c266: n5762,
        c320: n6111,
        c321: n6128,
        c272: n6023,
        h1: n7485, h2: n7486,
    };
    // body 103: buttons 0x29, forks 0x0
    sink.o5(41, take_5_13, &sh5, &o5);
    declined |= live_v42_b104 & (if bd_v42_b104 { ALL } else { !ok_v42_b104 });
    take_5_14 |= live_v42_b104 & ok_v42_b104 & (if bd_v42_b104 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6086,
        c309: n6035,
        c253: n6002,
        c310: n6058,
        c311: n6124,
        c255: n6003,
        c256: n6004,
        c312: n5904,
        c258: n5832,
        c265: n5980,
        c266: n5762,
        c320: n6119,
        c321: n6131,
        c272: n6023,
        h1: n7494, h2: n7495,
    };
    // body 104: buttons 0x2a, forks 0x0
    sink.o5(42, take_5_14, &sh5, &o5);
    declined |= live_v48_b105 & (if bd_v48_b105 { ALL } else { !ok_v48_b105 });
    take_5_15 |= live_v48_b105 & ok_v48_b105 & (if bd_v48_b105 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6005,
        c309: n6006,
        c253: n6002,
        c310: n6007,
        c311: n6008,
        c255: n6003,
        c256: n6004,
        c312: n5835,
        c258: n5927,
        c265: n5980,
        c266: n5919,
        c320: n6149,
        c321: n6138,
        c272: n6023,
        h1: n7518, h2: n7519,
    };
    // body 105: buttons 0x30, forks 0x0
    sink.o5(48, take_5_15, &sh5, &o5);
    declined |= live_v49_b106 & (if bd_v49_b106 { ALL } else { !ok_v49_b106 });
    take_5_16 |= live_v49_b106 & ok_v49_b106 & (if bd_v49_b106 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6005,
        c309: n6035,
        c253: n6002,
        c310: n6036,
        c311: n6008,
        c255: n6003,
        c256: n6004,
        c312: n5877,
        c258: n5927,
        c265: n5980,
        c266: n5919,
        c320: n6168,
        c321: n6157,
        c272: n6023,
        h1: n7532, h2: n7533,
    };
    // body 106: buttons 0x31, forks 0x0
    sink.o5(49, take_5_16, &sh5, &o5);
    declined |= live_v50_b107 & (if bd_v50_b107 { ALL } else { !ok_v50_b107 });
    take_5_17 |= live_v50_b107 & ok_v50_b107 & (if bd_v50_b107 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6005,
        c309: n6035,
        c253: n6002,
        c310: n6058,
        c311: n6008,
        c255: n6003,
        c256: n6004,
        c312: n5904,
        c258: n5927,
        c265: n5980,
        c266: n5919,
        c320: n6187,
        c321: n6176,
        c272: n6023,
        h1: n7544, h2: n7545,
    };
    // body 107: buttons 0x32, forks 0x0
    sink.o5(50, take_5_17, &sh5, &o5);
    declined |= live_v52_b108 & (if bd_v52_b108 { ALL } else { !ok_v52_b108 });
    take_5_18 |= live_v52_b108 & ok_v52_b108 & (if bd_v52_b108 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6086,
        c309: n6087,
        c253: n6002,
        c310: n6088,
        c311: n6089,
        c255: n6003,
        c256: n6004,
        c312: n5835,
        c258: n5927,
        c265: n5980,
        c266: n5919,
        c320: n6206,
        c321: n6195,
        c272: n6023,
        h1: n7560, h2: n7561,
    };
    // body 108: buttons 0x34, forks 0x0
    sink.o5(52, take_5_18, &sh5, &o5);
    declined |= live_v53_b109 & (if bd_v53_b109 { ALL } else { !ok_v53_b109 });
    take_5_19 |= live_v53_b109 & ok_v53_b109 & (if bd_v53_b109 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6086,
        c309: n6035,
        c253: n6002,
        c310: n6036,
        c311: n6089,
        c255: n6003,
        c256: n6004,
        c312: n5877,
        c258: n5927,
        c265: n5980,
        c266: n5919,
        c320: n6215,
        c321: n6213,
        c272: n6023,
        h1: n7574, h2: n7575,
    };
    // body 109: buttons 0x35, forks 0x0
    sink.o5(53, take_5_19, &sh5, &o5);
    declined |= live_v54_b110 & (if bd_v54_b110 { ALL } else { !ok_v54_b110 });
    take_5_20 |= live_v54_b110 & ok_v54_b110 & (if bd_v54_b110 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6086,
        c309: n6035,
        c253: n6002,
        c310: n6058,
        c311: n6089,
        c255: n6003,
        c256: n6004,
        c312: n5904,
        c258: n5927,
        c265: n5980,
        c266: n5919,
        c320: n6223,
        c321: n6221,
        c272: n6023,
        h1: n7586, h2: n7587,
    };
    // body 110: buttons 0x36, forks 0x0
    sink.o5(54, take_5_20, &sh5, &o5);
    declined |= live_v56_b111 & (if bd_v56_b111 { ALL } else { !ok_v56_b111 });
    take_5_21 |= live_v56_b111 & ok_v56_b111 & (if bd_v56_b111 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6086,
        c309: n6087,
        c253: n6002,
        c310: n6088,
        c311: n6124,
        c255: n6003,
        c256: n6004,
        c312: n5835,
        c258: n5927,
        c265: n5980,
        c266: n5919,
        c320: n6206,
        c321: n6226,
        c272: n6023,
        h1: n7595, h2: n7596,
    };
    // body 111: buttons 0x38, forks 0x0
    sink.o5(56, take_5_21, &sh5, &o5);
    declined |= live_v57_b112 & (if bd_v57_b112 { ALL } else { !ok_v57_b112 });
    take_5_22 |= live_v57_b112 & ok_v57_b112 & (if bd_v57_b112 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6086,
        c309: n6035,
        c253: n6002,
        c310: n6036,
        c311: n6124,
        c255: n6003,
        c256: n6004,
        c312: n5877,
        c258: n5927,
        c265: n5980,
        c266: n5919,
        c320: n6215,
        c321: n6229,
        c272: n6023,
        h1: n7604, h2: n7605,
    };
    // body 112: buttons 0x39, forks 0x0
    sink.o5(57, take_5_22, &sh5, &o5);
    declined |= live_v58_b113 & (if bd_v58_b113 { ALL } else { !ok_v58_b113 });
    take_5_23 |= live_v58_b113 & ok_v58_b113 & (if bd_v58_b113 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n6000,
        c41: n6001,
        c308: n6086,
        c309: n6035,
        c253: n6002,
        c310: n6058,
        c311: n6124,
        c255: n6003,
        c256: n6004,
        c312: n5904,
        c258: n5927,
        c265: n5980,
        c266: n5919,
        c320: n6223,
        c321: n6232,
        c272: n6023,
        h1: n7613, h2: n7614,
    };
    // body 113: buttons 0x3a, forks 0x0
    sink.o5(58, take_5_23, &sh5, &o5);
    declined
}
