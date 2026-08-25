// GENERATED from a TRACED frame (shape 5). Do not edit.
//
// One input shape, 4 output shapes, 76 distinct button
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
pub const SHAPE: u64 = 37911473208088391;

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
    pub c306: P8,
    pub c307: P8,
    pub c308: P8,
    pub c309: P8,
    pub c316: P8,
    pub c317: P8,
    pub c318: P8,
    pub c319: P8,
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
    ("objects[1].collideable", "bool"),
    ("objects[1].flip.x", "bool"),
    ("objects[1].flip.y", "bool"),
    ("objects[1].off", "num"),
    ("objects[1].rem.x", "num"),
    ("objects[1].rem.y", "num"),
    ("objects[1].solids", "bool"),
    ("objects[1].spd.x", "num"),
    ("objects[1].spd.y", "num"),
    ("objects[1].spr", "num"),
    ("objects[1].start", "num"),
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
    pub c300: ZN,
    pub c301: ZN,
    pub c236: ZN,
    pub c302: ZN,
    pub c303: ZN,
    pub c238: ZN,
    pub c239: ZN,
    pub c304: u16,
    pub c305: u16,
    pub c241: ZN,
    pub c248: u16,
    pub c249: u16,
    pub c310: ZN,
    pub c311: ZN,
    pub c251: u16,
    pub c312: ZN,
    pub c313: ZN,
    pub c255: ZN,
    pub c256: ZN,
    pub c259: u16,
    pub c314: u16,
    pub c315: u16,
    pub c267: ZN,
    pub c320: ZN,
    pub c321: ZN,
    pub c269: u16,
    pub c322: ZN,
    pub c323: ZN,
    pub c271: ZN,
    pub c272: ZN,
    pub c274: ZN,
    pub c275: ZN,
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
    pub c300: u32,
    pub c301: u32,
    pub c236: u32,
    pub c302: u32,
    pub c303: u32,
    pub c238: u32,
    pub c239: u32,
    pub c304: u32,
    pub c305: u32,
    pub c241: u32,
    pub c248: u32,
    pub c249: u32,
    pub c310: u32,
    pub c311: u32,
    pub c251: u32,
    pub c312: u32,
    pub c313: u32,
    pub c255: u32,
    pub c256: u32,
    pub c259: u32,
    pub c314: u32,
    pub c315: u32,
    pub c267: u32,
    pub c320: u32,
    pub c321: u32,
    pub c269: u32,
    pub c322: u32,
    pub c323: u32,
    pub c271: u32,
    pub c272: u32,
    pub c274: u32,
    pub c275: u32,
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
        c306: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c307: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c308: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c309: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c316: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c317: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c318: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c319: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
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
        c300: cell("objects[0].dash_accel.x")?,
        c301: cell("objects[0].dash_accel.y")?,
        c236: cell("objects[0].dash_effect_time")?,
        c302: cell("objects[0].dash_target.x")?,
        c303: cell("objects[0].dash_target.y")?,
        c238: cell("objects[0].dash_time")?,
        c239: cell("objects[0].djump")?,
        c304: cell("objects[0].flip.x")?,
        c305: cell("objects[0].flip.y")?,
        c241: cell("objects[0].grace")?,
        c248: cell("objects[0].p_dash")?,
        c249: cell("objects[0].p_jump")?,
        c310: cell("objects[0].rem.x")?,
        c311: cell("objects[0].rem.y")?,
        c251: cell("objects[0].solids")?,
        c312: cell("objects[0].spd.x")?,
        c313: cell("objects[0].spd.y")?,
        c255: cell("objects[0].x")?,
        c256: cell("objects[0].y")?,
        c259: cell("objects[1].collideable")?,
        c314: cell("objects[1].flip.x")?,
        c315: cell("objects[1].flip.y")?,
        c267: cell("objects[1].off")?,
        c320: cell("objects[1].rem.x")?,
        c321: cell("objects[1].rem.y")?,
        c269: cell("objects[1].solids")?,
        c322: cell("objects[1].spd.x")?,
        c323: cell("objects[1].spd.y")?,
        c271: cell("objects[1].spr")?,
        c272: cell("objects[1].start")?,
        c274: cell("objects[1].x")?,
        c275: cell("objects[1].y")?,
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
        c236: match &b.cols[s.c236 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c302: match &b.cols[s.c302 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c303: match &b.cols[s.c303 as usize] {
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
        c304: match &b.cols[s.c304 as usize] {
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
        c305: match &b.cols[s.c305 as usize] {
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
        c259: match &b.cols[s.c259 as usize] {
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
        c314: match &b.cols[s.c314 as usize] {
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
        c315: match &b.cols[s.c315 as usize] {
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
        c267: match &b.cols[s.c267 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c269: match &b.cols[s.c269 as usize] {
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
        c322: match &b.cols[s.c322 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c323: match &b.cols[s.c323 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c271: match &b.cols[s.c271 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c272: match &b.cols[s.c272 as usize] {
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

/// Outcome 1's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared1 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c241: ZN,
    pub c249: ZN,
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
    pub c310: ZN,
    pub c311: ZN,
    pub c256: ZN,
    pub c267: ZN,
    pub c275: ZN,
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
    b.cols[241] = Col::N(Vec::new());
    b.cols[267] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Bool(true));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[245] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[246] = Col::U(AV::Num(P8::from_raw(2359296i32)));
    b.cols[248] = Col::U(AV::Num(P8::from_raw(786432i32)));
    b.cols[249] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
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
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[241] { v.push(sh.c241.lane(i)); }
        if let Col::N(v) = &mut acc.cols[249] { v.push(sh.c249.lane(i)); }
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
    b.cols[234] = Col::U(AV::Num(P8::from_raw(196608i32)));
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
    b.cols[246] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[249] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(8126464i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
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
    b.cols[267] = Col::N(Vec::new());
    b.cols[320] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[321] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Bool(true));
    b.cols[322] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[323] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(2359296i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(786432i32)));
    b.cols[275] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
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
        if let Col::N(v) = &mut acc.cols[267] { v.push(sh.c267.lane(i)); }
        if let Col::N(v) = &mut acc.cols[275] { v.push(sh.c275.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
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
    let r_c236: ZN = rin.c236;
    let r_c238: ZN = rin.c238;
    let r_c239: ZN = rin.c239;
    let r_c241: ZN = rin.c241;
    let r_c248: ZB = ZB { val: rin.c248, known: ALL };
    let r_c249: ZB = ZB { val: rin.c249, known: ALL };
    let r_c251: ZB = ZB { val: rin.c251, known: ALL };
    let r_c255: ZN = rin.c255;
    let r_c256: ZN = rin.c256;
    let r_c259: ZB = ZB { val: rin.c259, known: ALL };
    let r_c267: ZN = rin.c267;
    let r_c269: ZB = ZB { val: rin.c269, known: ALL };
    let r_c271: ZN = rin.c271;
    let r_c272: ZN = rin.c272;
    let r_c274: ZN = rin.c274;
    let r_c275: ZN = rin.c275;
    let r_c300: ZN = rin.c300;
    let r_c301: ZN = rin.c301;
    let r_c302: ZN = rin.c302;
    let r_c303: ZN = rin.c303;
    let r_c304: ZB = ZB { val: rin.c304, known: ALL };
    let r_c305: ZB = ZB { val: rin.c305, known: ALL };
    let r_c310: ZN = rin.c310;
    let r_c311: ZN = rin.c311;
    let r_c312: ZN = rin.c312;
    let r_c313: ZN = rin.c313;
    let r_c314: ZB = ZB { val: rin.c314, known: ALL };
    let r_c315: ZB = ZB { val: rin.c315, known: ALL };
    let r_c320: ZN = rin.c320;
    let r_c321: ZN = rin.c321;
    let r_c322: ZN = rin.c322;
    let r_c323: ZN = rin.c323;
    let n70: ZB = zb_not(r_c42);
    let n71: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n72: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c313);
    let n73: ZB = zb_not(r_c314);
    let n74: bool = P8::from_raw(0i32) == u.c318;
    let n75: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c322);
    let n79: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n80: ZN = zn_rem(n79, zn_splat(P8::from_raw(1966080i32)));
    let n81: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n80);
    let n97: ZB = zb_not(r_c248);
    let n98: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n99: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n107: ZB = zb_not(r_c43);
    let n108: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c323);
    let n111: ZB = zb_not(r_c305);
    let n112: bool = P8::from_raw(327680i32) == u.c306;
    let n113: bool = P8::from_raw(393216i32) == u.c307;
    let n114: bool = P8::from_raw(65536i32) == u.c308;
    let n115: bool = P8::from_raw(196608i32) == u.c309;
    let n116: ZB = zb_not(r_c315);
    let n117: bool = P8::from_raw(524288i32) == u.c316;
    let n118: bool = P8::from_raw(524288i32) == u.c317;
    let n119: bool = P8::from_raw(0i32) == u.c319;
    let n120: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c320);
    let n121: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c321);
    let n122: ZB = zn_eq(zn_splat(P8::from_raw(1703936i32)), r_c271);
    let n123: ZB = zn_eq(zn_splat(P8::from_raw(2359296i32)), r_c272);
    let n124: ZB = zn_eq(zn_splat(P8::from_raw(786432i32)), r_c274);
    let n125: ZB = zb_not(r_c38);
    let n126: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n127: ZN = zn_rem(n126, zn_splat(P8::from_raw(3932160i32)));
    let n128: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n127);
    let n129: ZN = zsel_n(n128, n98, r_c86);
    let n130: ZN = zsel_n(n81, n129, r_c86);
    let n131: ZN = zsel_n(n81, n127, r_c85);
    let n132: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c312);
    let n133: ZB = zb_not(n132);
    let n134: ZB = zb_not(n72);
    let n135: ZB = zb_or(n133, n134);
    let n136: ZB = zb_not(n135);
    let n137: ZN = zn_add(r_c310, r_c312);
    let n138: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n137);
    let n139: ZN = zn_flr(n138);
    let n140: ZN = zn_sub(n138, zn_splat(P8::from_raw(32768i32)));
    let n141: ZN = zn_sub(n140, n139);
    let n142: ZB = zn_gt(n139, zn_splat(P8::from_raw(0i32)));
    let n143: ZB = zn_lt(n139, zn_splat(P8::from_raw(0i32)));
    let n144: ZN = zsel_n(n143, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n145: ZN = zsel_n(n142, zn_splat(P8::from_raw(65536i32)), n144);
    let n146: ZN = zn_abs(n139);
    let n147: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c255);
    let n148: ZN = zn_add(n145, n147);
    let n149: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c256);
    let n150: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n149);
    let n151: ZB = zn_tile_flag_at(g.cache, g.cart, n148, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n152: ZN = zn_add(r_c255, n145);
    let n153: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n146);
    let n154: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n152);
    let n155: ZN = zn_add(n145, n154);
    let n156: ZB = zn_tile_flag_at(g.cache, g.cart, n155, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n157: ZN = zn_add(n145, n152);
    let n158: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n146);
    let n159: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n157);
    let n160: ZN = zn_add(n145, n159);
    let n161: ZB = zn_tile_flag_at(g.cache, g.cart, n160, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n162: ZN = zn_add(n145, n157);
    let n163: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n146);
    let n164: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n162);
    let n165: ZN = zn_add(n145, n164);
    let n166: ZB = zn_tile_flag_at(g.cache, g.cart, n165, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n167: ZN = zn_add(n145, n162);
    let n168: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n146);
    let n169: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n167);
    let n170: ZN = zn_add(n145, n169);
    let n171: ZB = zn_tile_flag_at(g.cache, g.cart, n170, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n172: ZN = zn_add(n145, n167);
    let n173: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n146);
    let n174: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n172);
    let n175: ZN = zn_add(n145, n174);
    let n176: ZB = zn_tile_flag_at(g.cache, g.cart, n175, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n177: ZN = zn_add(n145, n172);
    let n178: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n146);
    let n179: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n177);
    let n180: ZN = zn_add(n145, n179);
    let n181: ZB = zn_tile_flag_at(g.cache, g.cart, n180, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n182: ZN = zn_add(n145, n177);
    let n183: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n146);
    let n184: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n182);
    let n185: ZN = zn_add(n145, n184);
    let n186: ZB = zn_tile_flag_at(g.cache, g.cart, n185, n150, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n187: ZN = zn_add(n145, n182);
    let n188: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n146);
    let n189: ZN = zsel_n(n186, n182, n187);
    let n190: ZN = zsel_n(n186, zn_splat(P8::from_raw(0i32)), n141);
    let n191: ZN = zsel_n(n186, zn_splat(P8::from_raw(0i32)), r_c312);
    let n192: ZB = zb_or(n186, n188);
    let n193: ZN = zsel_n(n183, n182, n189);
    let n194: ZN = zsel_n(n183, n141, n190);
    let n195: ZN = zsel_n(n183, r_c312, n191);
    let n196: ZB = zb_or(n183, n192);
    let n197: ZN = zsel_n(n181, n177, n193);
    let n198: ZN = zsel_n(n181, zn_splat(P8::from_raw(0i32)), n194);
    let n199: ZN = zsel_n(n181, zn_splat(P8::from_raw(0i32)), n195);
    let n200: ZB = zb_or(n181, n196);
    let n201: ZN = zsel_n(n178, n177, n197);
    let n202: ZN = zsel_n(n178, n141, n198);
    let n203: ZN = zsel_n(n178, r_c312, n199);
    let n204: ZB = zb_or(n178, n200);
    let n205: ZN = zsel_n(n176, n172, n201);
    let n206: ZN = zsel_n(n176, zn_splat(P8::from_raw(0i32)), n202);
    let n207: ZN = zsel_n(n176, zn_splat(P8::from_raw(0i32)), n203);
    let n208: ZB = zb_or(n176, n204);
    let n209: ZN = zsel_n(n173, n172, n205);
    let n210: ZN = zsel_n(n173, n141, n206);
    let n211: ZN = zsel_n(n173, r_c312, n207);
    let n212: ZB = zb_or(n173, n208);
    let n213: ZN = zsel_n(n171, n167, n209);
    let n214: ZN = zsel_n(n171, zn_splat(P8::from_raw(0i32)), n210);
    let n215: ZN = zsel_n(n171, zn_splat(P8::from_raw(0i32)), n211);
    let n216: ZB = zb_or(n171, n212);
    let n217: ZN = zsel_n(n168, n167, n213);
    let n218: ZN = zsel_n(n168, n141, n214);
    let n219: ZN = zsel_n(n168, r_c312, n215);
    let n220: ZB = zb_or(n168, n216);
    let n221: ZN = zsel_n(n166, n162, n217);
    let n222: ZN = zsel_n(n166, zn_splat(P8::from_raw(0i32)), n218);
    let n223: ZN = zsel_n(n166, zn_splat(P8::from_raw(0i32)), n219);
    let n224: ZB = zb_or(n166, n220);
    let n225: ZN = zsel_n(n163, n162, n221);
    let n226: ZN = zsel_n(n163, n141, n222);
    let n227: ZN = zsel_n(n163, r_c312, n223);
    let n228: ZB = zb_or(n163, n224);
    let n229: ZN = zsel_n(n161, n157, n225);
    let n230: ZN = zsel_n(n161, zn_splat(P8::from_raw(0i32)), n226);
    let n231: ZN = zsel_n(n161, zn_splat(P8::from_raw(0i32)), n227);
    let n232: ZB = zb_or(n161, n228);
    let n233: ZN = zsel_n(n158, n157, n229);
    let n234: ZN = zsel_n(n158, n141, n230);
    let n235: ZN = zsel_n(n158, r_c312, n231);
    let n236: ZB = zb_or(n158, n232);
    let n237: ZN = zsel_n(n156, n152, n233);
    let n238: ZN = zsel_n(n156, zn_splat(P8::from_raw(0i32)), n234);
    let n239: ZN = zsel_n(n156, zn_splat(P8::from_raw(0i32)), n235);
    let n240: ZB = zb_or(n156, n236);
    let n241: ZN = zsel_n(n153, n152, n237);
    let n242: ZN = zsel_n(n153, n141, n238);
    let n243: ZN = zsel_n(n153, r_c312, n239);
    let n244: ZB = zb_or(n153, n240);
    let n245: ZN = zsel_n(n151, r_c255, n241);
    let n246: ZN = zsel_n(n151, zn_splat(P8::from_raw(0i32)), n242);
    let n247: ZN = zsel_n(n151, zn_splat(P8::from_raw(0i32)), n243);
    let n248: ZB = zb_or(n151, n244);
    let n249: ZN = zn_add(r_c311, r_c313);
    let n250: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n249);
    let n251: ZN = zn_flr(n250);
    let n252: ZN = zn_sub(n250, zn_splat(P8::from_raw(32768i32)));
    let n253: ZN = zn_sub(n252, n251);
    let n254: ZB = zn_gt(n251, zn_splat(P8::from_raw(0i32)));
    let n255: ZB = zn_lt(n251, zn_splat(P8::from_raw(0i32)));
    let n256: ZN = zsel_n(n255, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n257: ZN = zsel_n(n254, zn_splat(P8::from_raw(65536i32)), n256);
    let n258: ZN = zn_abs(n251);
    let n259: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n245);
    let n260: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n259);
    let n261: ZN = zn_add(n149, n257);
    let n262: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n261, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n263: ZN = zn_add(r_c256, n257);
    let n264: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n258);
    let n265: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n263);
    let n266: ZN = zn_add(n257, n265);
    let n267: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n266, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n268: ZN = zn_add(n257, n263);
    let n269: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n258);
    let n270: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n268);
    let n271: ZN = zn_add(n257, n270);
    let n272: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n271, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n273: ZN = zn_add(n257, n268);
    let n274: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n258);
    let n275: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n273);
    let n276: ZN = zn_add(n257, n275);
    let n277: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n276, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n278: ZN = zn_add(n257, n273);
    let n279: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n258);
    let n280: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n278);
    let n281: ZN = zn_add(n257, n280);
    let n282: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n281, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n283: ZN = zn_add(n257, n278);
    let n284: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n258);
    let n285: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n283);
    let n286: ZN = zn_add(n257, n285);
    let n287: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n286, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n288: ZN = zn_add(n257, n283);
    let n289: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n258);
    let n290: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n288);
    let n291: ZN = zn_add(n257, n290);
    let n292: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n291, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n293: ZN = zn_add(n257, n288);
    let n294: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n258);
    let n295: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n293);
    let n296: ZN = zn_add(n257, n295);
    let n297: ZB = zn_tile_flag_at(g.cache, g.cart, n260, n296, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n298: ZN = zn_add(n257, n293);
    let n299: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n258);
    let n300: ZB = zb_and(n248, n299);
    let n301: ZN = zsel_n(n297, n293, n298);
    let n302: ZN = zsel_n(n297, zn_splat(P8::from_raw(0i32)), n253);
    let n303: ZN = zsel_n(n297, zn_splat(P8::from_raw(0i32)), r_c313);
    let n304: ZB = zsel_b(n297, n248, n300);
    let n305: ZN = zsel_n(n294, n293, n301);
    let n306: ZN = zsel_n(n294, n253, n302);
    let n307: ZN = zsel_n(n294, r_c313, n303);
    let n308: ZB = zsel_b(n294, n248, n304);
    let n309: ZN = zsel_n(n292, n288, n305);
    let n310: ZN = zsel_n(n292, zn_splat(P8::from_raw(0i32)), n306);
    let n311: ZN = zsel_n(n292, zn_splat(P8::from_raw(0i32)), n307);
    let n312: ZB = zsel_b(n292, n248, n308);
    let n313: ZN = zsel_n(n289, n288, n309);
    let n314: ZN = zsel_n(n289, n253, n310);
    let n315: ZN = zsel_n(n289, r_c313, n311);
    let n316: ZB = zsel_b(n289, n248, n312);
    let n317: ZN = zsel_n(n287, n283, n313);
    let n318: ZN = zsel_n(n287, zn_splat(P8::from_raw(0i32)), n314);
    let n319: ZN = zsel_n(n287, zn_splat(P8::from_raw(0i32)), n315);
    let n320: ZB = zsel_b(n287, n248, n316);
    let n321: ZN = zsel_n(n284, n283, n317);
    let n322: ZN = zsel_n(n284, n253, n318);
    let n323: ZN = zsel_n(n284, r_c313, n319);
    let n324: ZB = zsel_b(n284, n248, n320);
    let n325: ZN = zsel_n(n282, n278, n321);
    let n326: ZN = zsel_n(n282, zn_splat(P8::from_raw(0i32)), n322);
    let n327: ZN = zsel_n(n282, zn_splat(P8::from_raw(0i32)), n323);
    let n328: ZB = zsel_b(n282, n248, n324);
    let n329: ZN = zsel_n(n279, n278, n325);
    let n330: ZN = zsel_n(n279, n253, n326);
    let n331: ZN = zsel_n(n279, r_c313, n327);
    let n332: ZB = zsel_b(n279, n248, n328);
    let n333: ZN = zsel_n(n277, n273, n329);
    let n334: ZN = zsel_n(n277, zn_splat(P8::from_raw(0i32)), n330);
    let n335: ZN = zsel_n(n277, zn_splat(P8::from_raw(0i32)), n331);
    let n336: ZB = zsel_b(n277, n248, n332);
    let n337: ZN = zsel_n(n274, n273, n333);
    let n338: ZN = zsel_n(n274, n253, n334);
    let n339: ZN = zsel_n(n274, r_c313, n335);
    let n340: ZB = zsel_b(n274, n248, n336);
    let n341: ZN = zsel_n(n272, n268, n337);
    let n342: ZN = zsel_n(n272, zn_splat(P8::from_raw(0i32)), n338);
    let n343: ZN = zsel_n(n272, zn_splat(P8::from_raw(0i32)), n339);
    let n344: ZB = zsel_b(n272, n248, n340);
    let n345: ZN = zsel_n(n269, n268, n341);
    let n346: ZN = zsel_n(n269, n253, n342);
    let n347: ZN = zsel_n(n269, r_c313, n343);
    let n348: ZB = zsel_b(n269, n248, n344);
    let n349: ZN = zsel_n(n267, n263, n345);
    let n350: ZN = zsel_n(n267, zn_splat(P8::from_raw(0i32)), n346);
    let n351: ZN = zsel_n(n267, zn_splat(P8::from_raw(0i32)), n347);
    let n352: ZB = zsel_b(n267, n248, n348);
    let n353: ZN = zsel_n(n264, n263, n349);
    let n354: ZN = zsel_n(n264, n253, n350);
    let n355: ZN = zsel_n(n264, r_c313, n351);
    let n356: ZB = zsel_b(n264, n248, n352);
    let n357: ZN = zsel_n(n262, r_c256, n353);
    let n358: ZN = zsel_n(n262, zn_splat(P8::from_raw(0i32)), n354);
    let n359: ZN = zsel_n(n262, zn_splat(P8::from_raw(0i32)), n355);
    let n360: ZB = zsel_b(n262, n248, n356);
    let n361: ZN = zsel_n(n135, n245, r_c255);
    let n362: ZN = zsel_n(n135, n357, r_c256);
    let n363: ZN = zsel_n(n135, n246, r_c310);
    let n364: ZN = zsel_n(n135, n358, r_c311);
    let n365: ZN = zsel_n(n135, n247, r_c312);
    let n366: ZN = zsel_n(n135, n359, r_c313);
    let n367: ZB = zb_or(n136, n360);
    let n368: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n361);
    let n369: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n362);
    let n370: ZN = zn_div(n368, zn_splat(P8::from_raw(524288i32)));
    let n371: ZN = zn_flr(n370);
    let n372: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n371);
    let n373: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n368);
    let n374: ZN = zn_sub(n373, zn_splat(P8::from_raw(65536i32)));
    let n375: ZN = zn_div(n374, zn_splat(P8::from_raw(524288i32)));
    let n376: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n375);
    let n377: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n372);
    let n378: ZB = zn_le(n377, n376);
    let n379: ZB = zn_gt(n377, n376);
    let n380: ZB = zb_and(n99, n378);
    let n381: ZB = zb_and(n99, n379);
    let n382: ZN = zn_div(n369, zn_splat(P8::from_raw(524288i32)));
    let n383: ZN = zn_flr(n382);
    let n384: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n383);
    let n385: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n369);
    let n386: ZN = zn_sub(n385, zn_splat(P8::from_raw(65536i32)));
    let n387: ZN = zn_div(n386, zn_splat(P8::from_raw(524288i32)));
    let n388: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n387);
    let n389: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n384);
    let n390: ZB = zn_le(n389, n388);
    let n391: ZB = zn_gt(n389, n388);
    let n392: ZB = zb_and(n380, n390);
    let n393: ZB = zb_and(n380, n391);
    let n394: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n377);
    let n395: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n389);
    let n396: ZN = zn_mget(g.cart, n394, n395);
    let n397: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n396);
    let n398: ZB = zb_not(n397);
    let n399: ZB = zb_and(n392, n397);
    let n400: ZB = zb_and(n392, n398);
    let n401: ZN = zn_rem(n386, zn_splat(P8::from_raw(524288i32)));
    let n402: ZB = zn_ge(n401, zn_splat(P8::from_raw(393216i32)));
    let n403: ZB = zn_lt(n401, zn_splat(P8::from_raw(393216i32)));
    let n404: ZB = zb_and(n399, n403);
    let n405: ZB = zb_and(n399, n402);
    let n406: ZN = zn_mul(n389, zn_splat(P8::from_raw(524288i32)));
    let n407: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n406);
    let n408: ZB = zn_eq(n385, n407);
    let n409: ZB = zb_or(n404, n405);
    let n410: ZB = zb_or(n402, n408);
    let n411: ZB = zb_or(n400, n409);
    let n412: ZB = zb_and(n397, n410);
    let n413: ZB = zb_not(n412);
    let n414: ZB = zb_and(n411, n412);
    let n415: ZB = zb_and(n411, n413);
    let n416: ZB = zn_ge(n366, zn_splat(P8::from_raw(0i32)));
    let n417: ZB = zb_or(n414, n415);
    let n418: ZB = zb_and(n412, n416);
    let n419: ZB = zb_not(n418);
    let n420: ZB = zb_and(n417, n419);
    let n421: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n396);
    let n422: ZB = zb_not(n421);
    let n423: ZB = zb_and(n420, n421);
    let n424: ZB = zb_and(n420, n422);
    let n425: ZN = zn_rem(n369, zn_splat(P8::from_raw(524288i32)));
    let n426: ZB = zn_le(n425, zn_splat(P8::from_raw(131072i32)));
    let n427: ZB = zb_or(n423, n424);
    let n428: ZB = zb_and(n421, n426);
    let n429: ZB = zb_not(n428);
    let n430: ZB = zb_and(n427, n428);
    let n431: ZB = zb_and(n427, n429);
    let n432: ZB = zn_le(n366, zn_splat(P8::from_raw(0i32)));
    let n433: ZB = zb_or(n430, n431);
    let n434: ZB = zb_and(n428, n432);
    let n435: ZB = zb_not(n434);
    let n436: ZB = zb_and(n433, n435);
    let n437: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n396);
    let n438: ZB = zb_not(n437);
    let n439: ZB = zb_and(n436, n437);
    let n440: ZB = zb_and(n436, n438);
    let n441: ZN = zn_rem(n368, zn_splat(P8::from_raw(524288i32)));
    let n442: ZB = zn_le(n441, zn_splat(P8::from_raw(131072i32)));
    let n443: ZB = zb_or(n439, n440);
    let n444: ZB = zb_and(n437, n442);
    let n445: ZB = zb_not(n444);
    let n446: ZB = zb_and(n443, n444);
    let n447: ZB = zb_and(n443, n445);
    let n448: ZB = zn_le(n365, zn_splat(P8::from_raw(0i32)));
    let n449: ZB = zb_or(n446, n447);
    let n450: ZB = zb_and(n444, n448);
    let n451: ZB = zb_not(n450);
    let n452: ZB = zb_and(n449, n451);
    let n453: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n396);
    let n454: ZB = zb_not(n453);
    let n455: ZB = zb_and(n452, n453);
    let n456: ZB = zb_and(n452, n454);
    let n457: ZN = zn_rem(n374, zn_splat(P8::from_raw(524288i32)));
    let n458: ZB = zn_ge(n457, zn_splat(P8::from_raw(393216i32)));
    let n459: ZB = zn_lt(n457, zn_splat(P8::from_raw(393216i32)));
    let n460: ZB = zb_and(n455, n459);
    let n461: ZB = zb_and(n455, n458);
    let n462: ZN = zn_mul(n377, zn_splat(P8::from_raw(524288i32)));
    let n463: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n462);
    let n464: ZB = zn_eq(n373, n463);
    let n465: ZB = zb_or(n460, n461);
    let n466: ZB = zb_or(n458, n464);
    let n467: ZB = zb_or(n456, n465);
    let n468: ZB = zb_and(n453, n466);
    let n469: ZB = zb_not(n468);
    let n470: ZB = zb_and(n467, n468);
    let n471: ZB = zb_and(n467, n469);
    let n472: ZB = zn_ge(n365, zn_splat(P8::from_raw(0i32)));
    let n473: ZB = zb_or(n470, n471);
    let n474: ZB = zb_and(n468, n472);
    let n475: ZB = zb_not(n474);
    let n476: ZB = zb_and(n473, n475);
    let n477: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n384);
    let n478: ZB = zn_le(n477, n388);
    let n479: ZB = zn_gt(n477, n388);
    let n480: ZB = zb_and(n476, n478);
    let n481: ZB = zb_and(n476, n479);
    let n482: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n477);
    let n483: ZN = zn_mget(g.cart, n394, n482);
    let n484: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n483);
    let n485: ZB = zb_not(n484);
    let n486: ZB = zb_and(n480, n484);
    let n487: ZB = zb_and(n480, n485);
    let n488: ZB = zb_and(n403, n486);
    let n489: ZB = zb_and(n402, n486);
    let n490: ZN = zn_mul(n477, zn_splat(P8::from_raw(524288i32)));
    let n491: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n490);
    let n492: ZB = zn_eq(n385, n491);
    let n493: ZB = zb_or(n488, n489);
    let n494: ZB = zb_or(n402, n492);
    let n495: ZB = zb_or(n487, n493);
    let n496: ZB = zb_and(n484, n494);
    let n497: ZB = zb_not(n496);
    let n498: ZB = zb_and(n495, n496);
    let n499: ZB = zb_and(n495, n497);
    let n500: ZB = zb_or(n498, n499);
    let n501: ZB = zb_and(n416, n496);
    let n502: ZB = zb_not(n501);
    let n503: ZB = zb_and(n500, n502);
    let n504: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n483);
    let n505: ZB = zb_not(n504);
    let n506: ZB = zb_and(n503, n504);
    let n507: ZB = zb_and(n503, n505);
    let n508: ZB = zb_or(n506, n507);
    let n509: ZB = zb_and(n426, n504);
    let n510: ZB = zb_not(n509);
    let n511: ZB = zb_and(n508, n509);
    let n512: ZB = zb_and(n508, n510);
    let n513: ZB = zb_or(n511, n512);
    let n514: ZB = zb_and(n432, n509);
    let n515: ZB = zb_not(n514);
    let n516: ZB = zb_and(n513, n515);
    let n517: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n483);
    let n518: ZB = zb_not(n517);
    let n519: ZB = zb_and(n516, n517);
    let n520: ZB = zb_and(n516, n518);
    let n521: ZB = zb_or(n519, n520);
    let n522: ZB = zb_and(n442, n517);
    let n523: ZB = zb_not(n522);
    let n524: ZB = zb_and(n521, n522);
    let n525: ZB = zb_and(n521, n523);
    let n526: ZB = zb_or(n524, n525);
    let n527: ZB = zb_and(n448, n522);
    let n528: ZB = zb_not(n527);
    let n529: ZB = zb_and(n526, n528);
    let n530: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n483);
    let n531: ZB = zb_not(n530);
    let n532: ZB = zb_and(n529, n530);
    let n533: ZB = zb_and(n529, n531);
    let n534: ZB = zb_and(n459, n532);
    let n535: ZB = zb_and(n458, n532);
    let n536: ZB = zb_or(n534, n535);
    let n537: ZB = zb_or(n533, n536);
    let n538: ZB = zb_and(n466, n530);
    let n539: ZB = zb_not(n538);
    let n540: ZB = zb_and(n537, n538);
    let n541: ZB = zb_and(n537, n539);
    let n542: ZB = zb_or(n540, n541);
    let n543: ZB = zb_and(n472, n538);
    let n544: ZB = zb_not(n543);
    let n545: ZB = zb_and(n542, n544);
    let n546: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n384);
    let n547: ZB = zn_le(n546, n388);
    let n548: ZB = zn_gt(n546, n388);
    let n549: ZB = zb_and(n545, n547);
    let n550: ZB = zb_and(n545, n548);
    let n551: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n546);
    let n552: ZN = zn_mget(g.cart, n394, n551);
    let n553: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n552);
    let n554: ZB = zb_not(n553);
    let n555: ZB = zb_and(n549, n553);
    let n556: ZB = zb_and(n549, n554);
    let n557: ZB = zb_and(n403, n555);
    let n558: ZB = zb_and(n402, n555);
    let n559: ZN = zn_mul(n546, zn_splat(P8::from_raw(524288i32)));
    let n560: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n559);
    let n561: ZB = zn_eq(n385, n560);
    let n562: ZB = zb_or(n557, n558);
    let n563: ZB = zb_or(n402, n561);
    let n564: ZB = zb_or(n556, n562);
    let n565: ZB = zb_and(n553, n563);
    let n566: ZB = zb_not(n565);
    let n567: ZB = zb_and(n564, n565);
    let n568: ZB = zb_and(n564, n566);
    let n569: ZB = zb_or(n567, n568);
    let n570: ZB = zb_and(n416, n565);
    let n571: ZB = zb_not(n570);
    let n572: ZB = zb_and(n569, n571);
    let n573: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n552);
    let n574: ZB = zb_not(n573);
    let n575: ZB = zb_and(n572, n573);
    let n576: ZB = zb_and(n572, n574);
    let n577: ZB = zb_or(n575, n576);
    let n578: ZB = zb_and(n426, n573);
    let n579: ZB = zb_not(n578);
    let n580: ZB = zb_and(n577, n578);
    let n581: ZB = zb_and(n577, n579);
    let n582: ZB = zb_or(n580, n581);
    let n583: ZB = zb_and(n432, n578);
    let n584: ZB = zb_not(n583);
    let n585: ZB = zb_and(n582, n584);
    let n586: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n552);
    let n587: ZB = zb_not(n586);
    let n588: ZB = zb_and(n585, n586);
    let n589: ZB = zb_and(n585, n587);
    let n590: ZB = zb_or(n588, n589);
    let n591: ZB = zb_and(n442, n586);
    let n592: ZB = zb_not(n591);
    let n593: ZB = zb_and(n590, n591);
    let n594: ZB = zb_and(n590, n592);
    let n595: ZB = zb_or(n593, n594);
    let n596: ZB = zb_and(n448, n591);
    let n597: ZB = zb_not(n596);
    let n598: ZB = zb_and(n595, n597);
    let n599: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n552);
    let n600: ZB = zb_not(n599);
    let n601: ZB = zb_and(n598, n599);
    let n602: ZB = zb_and(n598, n600);
    let n603: ZB = zb_and(n459, n601);
    let n604: ZB = zb_and(n458, n601);
    let n605: ZB = zb_or(n603, n604);
    let n606: ZB = zb_or(n602, n605);
    let n607: ZB = zb_and(n466, n599);
    let n608: ZB = zb_not(n607);
    let n609: ZB = zb_and(n606, n607);
    let n610: ZB = zb_and(n606, n608);
    let n611: ZB = zb_or(n609, n610);
    let n612: ZB = zb_and(n472, n607);
    let n613: ZB = zb_not(n612);
    let n614: ZB = zb_and(n611, n613);
    let n615: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n384);
    let n616: ZB = zn_gt(n615, n388);
    let n617: ZB = zb_and(n367, n616);
    let n618: ZB = zb_or(n550, n614);
    let n619: ZB = zsel_b(n548, n367, n617);
    let n620: ZB = zb_or(n481, n618);
    let n621: ZB = zsel_b(n479, n367, n619);
    let n622: ZB = zb_or(n393, n620);
    let n623: ZB = zsel_b(n391, n367, n621);
    let n624: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n372);
    let n625: ZB = zn_le(n624, n376);
    let n626: ZB = zn_gt(n624, n376);
    let n627: ZB = zb_and(n622, n625);
    let n628: ZB = zb_and(n622, n626);
    let n629: ZB = zb_and(n390, n627);
    let n630: ZB = zb_and(n391, n627);
    let n631: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n624);
    let n632: ZN = zn_mget(g.cart, n631, n395);
    let n633: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n632);
    let n634: ZB = zb_not(n633);
    let n635: ZB = zb_and(n629, n633);
    let n636: ZB = zb_and(n629, n634);
    let n637: ZB = zb_and(n403, n635);
    let n638: ZB = zb_and(n402, n635);
    let n639: ZB = zb_or(n637, n638);
    let n640: ZB = zb_or(n636, n639);
    let n641: ZB = zb_and(n410, n633);
    let n642: ZB = zb_not(n641);
    let n643: ZB = zb_and(n640, n641);
    let n644: ZB = zb_and(n640, n642);
    let n645: ZB = zb_or(n643, n644);
    let n646: ZB = zb_and(n416, n641);
    let n647: ZB = zb_not(n646);
    let n648: ZB = zb_and(n645, n647);
    let n649: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n632);
    let n650: ZB = zb_not(n649);
    let n651: ZB = zb_and(n648, n649);
    let n652: ZB = zb_and(n648, n650);
    let n653: ZB = zb_or(n651, n652);
    let n654: ZB = zb_and(n426, n649);
    let n655: ZB = zb_not(n654);
    let n656: ZB = zb_and(n653, n654);
    let n657: ZB = zb_and(n653, n655);
    let n658: ZB = zb_or(n656, n657);
    let n659: ZB = zb_and(n432, n654);
    let n660: ZB = zb_not(n659);
    let n661: ZB = zb_and(n658, n660);
    let n662: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n632);
    let n663: ZB = zb_not(n662);
    let n664: ZB = zb_and(n661, n662);
    let n665: ZB = zb_and(n661, n663);
    let n666: ZB = zb_or(n664, n665);
    let n667: ZB = zb_and(n442, n662);
    let n668: ZB = zb_not(n667);
    let n669: ZB = zb_and(n666, n667);
    let n670: ZB = zb_and(n666, n668);
    let n671: ZB = zb_or(n669, n670);
    let n672: ZB = zb_and(n448, n667);
    let n673: ZB = zb_not(n672);
    let n674: ZB = zb_and(n671, n673);
    let n675: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n632);
    let n676: ZB = zb_not(n675);
    let n677: ZB = zb_and(n674, n675);
    let n678: ZB = zb_and(n674, n676);
    let n679: ZB = zb_and(n459, n677);
    let n680: ZB = zb_and(n458, n677);
    let n681: ZN = zn_mul(n624, zn_splat(P8::from_raw(524288i32)));
    let n682: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n681);
    let n683: ZB = zn_eq(n373, n682);
    let n684: ZB = zb_or(n679, n680);
    let n685: ZB = zb_or(n458, n683);
    let n686: ZB = zb_or(n678, n684);
    let n687: ZB = zb_and(n675, n685);
    let n688: ZB = zb_not(n687);
    let n689: ZB = zb_and(n686, n687);
    let n690: ZB = zb_and(n686, n688);
    let n691: ZB = zb_or(n689, n690);
    let n692: ZB = zb_and(n472, n687);
    let n693: ZB = zb_not(n692);
    let n694: ZB = zb_and(n691, n693);
    let n695: ZB = zb_and(n478, n694);
    let n696: ZB = zb_and(n479, n694);
    let n697: ZN = zn_mget(g.cart, n631, n482);
    let n698: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n697);
    let n699: ZB = zb_not(n698);
    let n700: ZB = zb_and(n695, n698);
    let n701: ZB = zb_and(n695, n699);
    let n702: ZB = zb_and(n403, n700);
    let n703: ZB = zb_and(n402, n700);
    let n704: ZB = zb_or(n702, n703);
    let n705: ZB = zb_or(n701, n704);
    let n706: ZB = zb_and(n494, n698);
    let n707: ZB = zb_not(n706);
    let n708: ZB = zb_and(n705, n706);
    let n709: ZB = zb_and(n705, n707);
    let n710: ZB = zb_or(n708, n709);
    let n711: ZB = zb_and(n416, n706);
    let n712: ZB = zb_not(n711);
    let n713: ZB = zb_and(n710, n712);
    let n714: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n697);
    let n715: ZB = zb_not(n714);
    let n716: ZB = zb_and(n713, n714);
    let n717: ZB = zb_and(n713, n715);
    let n718: ZB = zb_or(n716, n717);
    let n719: ZB = zb_and(n426, n714);
    let n720: ZB = zb_not(n719);
    let n721: ZB = zb_and(n718, n719);
    let n722: ZB = zb_and(n718, n720);
    let n723: ZB = zb_or(n721, n722);
    let n724: ZB = zb_and(n432, n719);
    let n725: ZB = zb_not(n724);
    let n726: ZB = zb_and(n723, n725);
    let n727: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n697);
    let n728: ZB = zb_not(n727);
    let n729: ZB = zb_and(n726, n727);
    let n730: ZB = zb_and(n726, n728);
    let n731: ZB = zb_or(n729, n730);
    let n732: ZB = zb_and(n442, n727);
    let n733: ZB = zb_not(n732);
    let n734: ZB = zb_and(n731, n732);
    let n735: ZB = zb_and(n731, n733);
    let n736: ZB = zb_or(n734, n735);
    let n737: ZB = zb_and(n448, n732);
    let n738: ZB = zb_not(n737);
    let n739: ZB = zb_and(n736, n738);
    let n740: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n697);
    let n741: ZB = zb_not(n740);
    let n742: ZB = zb_and(n739, n740);
    let n743: ZB = zb_and(n739, n741);
    let n744: ZB = zb_and(n459, n742);
    let n745: ZB = zb_and(n458, n742);
    let n746: ZB = zb_or(n744, n745);
    let n747: ZB = zb_or(n743, n746);
    let n748: ZB = zb_and(n685, n740);
    let n749: ZB = zb_not(n748);
    let n750: ZB = zb_and(n747, n748);
    let n751: ZB = zb_and(n747, n749);
    let n752: ZB = zb_or(n750, n751);
    let n753: ZB = zb_and(n472, n748);
    let n754: ZB = zb_not(n753);
    let n755: ZB = zb_and(n752, n754);
    let n756: ZB = zb_and(n547, n755);
    let n757: ZB = zb_and(n548, n755);
    let n758: ZN = zn_mget(g.cart, n631, n551);
    let n759: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n758);
    let n760: ZB = zb_not(n759);
    let n761: ZB = zb_and(n756, n759);
    let n762: ZB = zb_and(n756, n760);
    let n763: ZB = zb_and(n403, n761);
    let n764: ZB = zb_and(n402, n761);
    let n765: ZB = zb_or(n763, n764);
    let n766: ZB = zb_or(n762, n765);
    let n767: ZB = zb_and(n563, n759);
    let n768: ZB = zb_not(n767);
    let n769: ZB = zb_and(n766, n767);
    let n770: ZB = zb_and(n766, n768);
    let n771: ZB = zb_or(n769, n770);
    let n772: ZB = zb_and(n416, n767);
    let n773: ZB = zb_not(n772);
    let n774: ZB = zb_and(n771, n773);
    let n775: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n758);
    let n776: ZB = zb_not(n775);
    let n777: ZB = zb_and(n774, n775);
    let n778: ZB = zb_and(n774, n776);
    let n779: ZB = zb_or(n777, n778);
    let n780: ZB = zb_and(n426, n775);
    let n781: ZB = zb_not(n780);
    let n782: ZB = zb_and(n779, n780);
    let n783: ZB = zb_and(n779, n781);
    let n784: ZB = zb_or(n782, n783);
    let n785: ZB = zb_and(n432, n780);
    let n786: ZB = zb_not(n785);
    let n787: ZB = zb_and(n784, n786);
    let n788: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n758);
    let n789: ZB = zb_not(n788);
    let n790: ZB = zb_and(n787, n788);
    let n791: ZB = zb_and(n787, n789);
    let n792: ZB = zb_or(n790, n791);
    let n793: ZB = zb_and(n442, n788);
    let n794: ZB = zb_not(n793);
    let n795: ZB = zb_and(n792, n793);
    let n796: ZB = zb_and(n792, n794);
    let n797: ZB = zb_or(n795, n796);
    let n798: ZB = zb_and(n448, n793);
    let n799: ZB = zb_not(n798);
    let n800: ZB = zb_and(n797, n799);
    let n801: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n758);
    let n802: ZB = zb_not(n801);
    let n803: ZB = zb_and(n800, n801);
    let n804: ZB = zb_and(n800, n802);
    let n805: ZB = zb_and(n459, n803);
    let n806: ZB = zb_and(n458, n803);
    let n807: ZB = zb_or(n805, n806);
    let n808: ZB = zb_or(n804, n807);
    let n809: ZB = zb_and(n685, n801);
    let n810: ZB = zb_not(n809);
    let n811: ZB = zb_and(n808, n809);
    let n812: ZB = zb_and(n808, n810);
    let n813: ZB = zb_or(n811, n812);
    let n814: ZB = zb_and(n472, n809);
    let n815: ZB = zb_not(n814);
    let n816: ZB = zb_and(n813, n815);
    let n817: ZB = zb_and(n616, n623);
    let n818: ZB = zb_or(n757, n816);
    let n819: ZB = zsel_b(n548, n623, n817);
    let n820: ZB = zb_or(n696, n818);
    let n821: ZB = zsel_b(n479, n623, n819);
    let n822: ZB = zb_or(n630, n820);
    let n823: ZB = zsel_b(n391, n623, n821);
    let n824: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n372);
    let n825: ZB = zn_le(n824, n376);
    let n826: ZB = zn_gt(n824, n376);
    let n827: ZB = zb_and(n822, n825);
    let n828: ZB = zb_and(n822, n826);
    let n829: ZB = zb_and(n390, n827);
    let n830: ZB = zb_and(n391, n827);
    let n831: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n824);
    let n832: ZN = zn_mget(g.cart, n831, n395);
    let n833: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n832);
    let n834: ZB = zb_not(n833);
    let n835: ZB = zb_and(n829, n833);
    let n836: ZB = zb_and(n829, n834);
    let n837: ZB = zb_and(n403, n835);
    let n838: ZB = zb_and(n402, n835);
    let n839: ZB = zb_or(n837, n838);
    let n840: ZB = zb_or(n836, n839);
    let n841: ZB = zb_and(n410, n833);
    let n842: ZB = zb_not(n841);
    let n843: ZB = zb_and(n840, n841);
    let n844: ZB = zb_and(n840, n842);
    let n845: ZB = zb_or(n843, n844);
    let n846: ZB = zb_and(n416, n841);
    let n847: ZB = zb_not(n846);
    let n848: ZB = zb_and(n845, n847);
    let n849: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n832);
    let n850: ZB = zb_not(n849);
    let n851: ZB = zb_and(n848, n849);
    let n852: ZB = zb_and(n848, n850);
    let n853: ZB = zb_or(n851, n852);
    let n854: ZB = zb_and(n426, n849);
    let n855: ZB = zb_not(n854);
    let n856: ZB = zb_and(n853, n854);
    let n857: ZB = zb_and(n853, n855);
    let n858: ZB = zb_or(n856, n857);
    let n859: ZB = zb_and(n432, n854);
    let n860: ZB = zb_not(n859);
    let n861: ZB = zb_and(n858, n860);
    let n862: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n832);
    let n863: ZB = zb_not(n862);
    let n864: ZB = zb_and(n861, n862);
    let n865: ZB = zb_and(n861, n863);
    let n866: ZB = zb_or(n864, n865);
    let n867: ZB = zb_and(n442, n862);
    let n868: ZB = zb_not(n867);
    let n869: ZB = zb_and(n866, n867);
    let n870: ZB = zb_and(n866, n868);
    let n871: ZB = zb_or(n869, n870);
    let n872: ZB = zb_and(n448, n867);
    let n873: ZB = zb_not(n872);
    let n874: ZB = zb_and(n871, n873);
    let n875: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n832);
    let n876: ZB = zb_not(n875);
    let n877: ZB = zb_and(n874, n875);
    let n878: ZB = zb_and(n874, n876);
    let n879: ZB = zb_and(n459, n877);
    let n880: ZB = zb_and(n458, n877);
    let n881: ZN = zn_mul(n824, zn_splat(P8::from_raw(524288i32)));
    let n882: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n881);
    let n883: ZB = zn_eq(n373, n882);
    let n884: ZB = zb_or(n879, n880);
    let n885: ZB = zb_or(n458, n883);
    let n886: ZB = zb_or(n878, n884);
    let n887: ZB = zb_and(n875, n885);
    let n888: ZB = zb_not(n887);
    let n889: ZB = zb_and(n886, n887);
    let n890: ZB = zb_and(n886, n888);
    let n891: ZB = zb_or(n889, n890);
    let n892: ZB = zb_and(n472, n887);
    let n893: ZB = zb_not(n892);
    let n894: ZB = zb_and(n891, n893);
    let n895: ZB = zb_and(n478, n894);
    let n896: ZB = zb_and(n479, n894);
    let n897: ZN = zn_mget(g.cart, n831, n482);
    let n898: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n897);
    let n899: ZB = zb_not(n898);
    let n900: ZB = zb_and(n895, n898);
    let n901: ZB = zb_and(n895, n899);
    let n902: ZB = zb_and(n403, n900);
    let n903: ZB = zb_and(n402, n900);
    let n904: ZB = zb_or(n902, n903);
    let n905: ZB = zb_or(n901, n904);
    let n906: ZB = zb_and(n494, n898);
    let n907: ZB = zb_not(n906);
    let n908: ZB = zb_and(n905, n906);
    let n909: ZB = zb_and(n905, n907);
    let n910: ZB = zb_or(n908, n909);
    let n911: ZB = zb_and(n416, n906);
    let n912: ZB = zb_not(n911);
    let n913: ZB = zb_and(n910, n912);
    let n914: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n897);
    let n915: ZB = zb_not(n914);
    let n916: ZB = zb_and(n913, n914);
    let n917: ZB = zb_and(n913, n915);
    let n918: ZB = zb_or(n916, n917);
    let n919: ZB = zb_and(n426, n914);
    let n920: ZB = zb_not(n919);
    let n921: ZB = zb_and(n918, n919);
    let n922: ZB = zb_and(n918, n920);
    let n923: ZB = zb_or(n921, n922);
    let n924: ZB = zb_and(n432, n919);
    let n925: ZB = zb_not(n924);
    let n926: ZB = zb_and(n923, n925);
    let n927: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n897);
    let n928: ZB = zb_not(n927);
    let n929: ZB = zb_and(n926, n927);
    let n930: ZB = zb_and(n926, n928);
    let n931: ZB = zb_or(n929, n930);
    let n932: ZB = zb_and(n442, n927);
    let n933: ZB = zb_not(n932);
    let n934: ZB = zb_and(n931, n932);
    let n935: ZB = zb_and(n931, n933);
    let n936: ZB = zb_or(n934, n935);
    let n937: ZB = zb_and(n448, n932);
    let n938: ZB = zb_not(n937);
    let n939: ZB = zb_and(n936, n938);
    let n940: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n897);
    let n941: ZB = zb_not(n940);
    let n942: ZB = zb_and(n939, n940);
    let n943: ZB = zb_and(n939, n941);
    let n944: ZB = zb_and(n459, n942);
    let n945: ZB = zb_and(n458, n942);
    let n946: ZB = zb_or(n944, n945);
    let n947: ZB = zb_or(n943, n946);
    let n948: ZB = zb_and(n885, n940);
    let n949: ZB = zb_not(n948);
    let n950: ZB = zb_and(n947, n948);
    let n951: ZB = zb_and(n947, n949);
    let n952: ZB = zb_or(n950, n951);
    let n953: ZB = zb_and(n472, n948);
    let n954: ZB = zb_not(n953);
    let n955: ZB = zb_and(n952, n954);
    let n956: ZB = zb_and(n547, n955);
    let n957: ZB = zb_and(n548, n955);
    let n958: ZN = zn_mget(g.cart, n831, n551);
    let n959: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n958);
    let n960: ZB = zb_not(n959);
    let n961: ZB = zb_and(n956, n959);
    let n962: ZB = zb_and(n956, n960);
    let n963: ZB = zb_and(n403, n961);
    let n964: ZB = zb_and(n402, n961);
    let n965: ZB = zb_or(n963, n964);
    let n966: ZB = zb_or(n962, n965);
    let n967: ZB = zb_and(n563, n959);
    let n968: ZB = zb_not(n967);
    let n969: ZB = zb_and(n966, n967);
    let n970: ZB = zb_and(n966, n968);
    let n971: ZB = zb_or(n969, n970);
    let n972: ZB = zb_and(n416, n967);
    let n973: ZB = zb_not(n972);
    let n974: ZB = zb_and(n971, n973);
    let n975: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n958);
    let n976: ZB = zb_not(n975);
    let n977: ZB = zb_and(n974, n975);
    let n978: ZB = zb_and(n974, n976);
    let n979: ZB = zb_or(n977, n978);
    let n980: ZB = zb_and(n426, n975);
    let n981: ZB = zb_not(n980);
    let n982: ZB = zb_and(n979, n980);
    let n983: ZB = zb_and(n979, n981);
    let n984: ZB = zb_or(n982, n983);
    let n985: ZB = zb_and(n432, n980);
    let n986: ZB = zb_not(n985);
    let n987: ZB = zb_and(n984, n986);
    let n988: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n958);
    let n989: ZB = zb_not(n988);
    let n990: ZB = zb_and(n987, n988);
    let n991: ZB = zb_and(n987, n989);
    let n992: ZB = zb_or(n990, n991);
    let n993: ZB = zb_and(n442, n988);
    let n994: ZB = zb_not(n993);
    let n995: ZB = zb_and(n992, n993);
    let n996: ZB = zb_and(n992, n994);
    let n997: ZB = zb_or(n995, n996);
    let n998: ZB = zb_and(n448, n993);
    let n999: ZB = zb_not(n998);
    let n1000: ZB = zb_and(n997, n999);
    let n1001: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n958);
    let n1002: ZB = zb_not(n1001);
    let n1003: ZB = zb_and(n1000, n1001);
    let n1004: ZB = zb_and(n1000, n1002);
    let n1005: ZB = zb_and(n459, n1003);
    let n1006: ZB = zb_and(n458, n1003);
    let n1007: ZB = zb_or(n1005, n1006);
    let n1008: ZB = zb_or(n1004, n1007);
    let n1009: ZB = zb_and(n885, n1001);
    let n1010: ZB = zb_not(n1009);
    let n1011: ZB = zb_and(n1008, n1009);
    let n1012: ZB = zb_and(n1008, n1010);
    let n1013: ZB = zb_or(n1011, n1012);
    let n1014: ZB = zb_and(n472, n1009);
    let n1015: ZB = zb_not(n1014);
    let n1016: ZB = zb_and(n1013, n1015);
    let n1017: ZB = zb_and(n616, n823);
    let n1018: ZB = zb_or(n957, n1016);
    let n1019: ZB = zsel_b(n548, n823, n1017);
    let n1020: ZB = zb_or(n896, n1018);
    let n1021: ZB = zsel_b(n479, n823, n1019);
    let n1022: ZB = zb_or(n830, n1020);
    let n1023: ZB = zsel_b(n391, n823, n1021);
    let n1024: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n372);
    let n1025: ZB = zn_gt(n1024, n376);
    let n1026: ZB = zb_and(n1023, n1025);
    let n1027: ZB = zb_or(n828, n1022);
    let n1028: ZB = zsel_b(n826, n823, n1026);
    let n1029: ZB = zb_or(n628, n1027);
    let n1030: ZB = zsel_b(n626, n623, n1028);
    let n1031: ZB = zb_or(n381, n1029);
    let n1032: ZB = zsel_b(n379, n367, n1030);
    let n1033: ZB = zn_le(n362, zn_splat(P8::from_raw(8388608i32)));
    let n1034: ZB = zb_and(n1031, n1033);
    let n1035: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n368);
    let n1036: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n369);
    let n1037: ZB = zn_tile_flag_at(g.cache, g.cart, n1035, n1036, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1038: ZB = zb_not(n1037);
    let n1039: ZB = zb_and(n1034, n1038);
    let n1040: ZB = zb_and(n1034, n1037);
    let n1041: ZB = zb_or(n1039, n1040);
    let n1042: ZB = zb_and(n1038, n1041);
    let n1043: ZB = zb_and(n1037, n1041);
    let n1044: ZB = zb_or(n1042, n1043);
    let n1045: ZB = zb_not(r_c249);
    let n1046: ZB = zb_and(n1037, n1044);
    let n1047: ZB = zb_and(n1038, n1044);
    let n1048: ZB = zn_lt(r_c239, zn_splat(P8::from_raw(65536i32)));
    let n1049: ZB = zn_ge(r_c239, zn_splat(P8::from_raw(65536i32)));
    let n1050: ZB = zb_and(n1046, n1048);
    let n1051: ZB = zb_and(n1046, n1049);
    let n1052: ZN = zsel_n(n1048, zn_splat(P8::from_raw(65536i32)), r_c239);
    let n1053: ZB = zb_or(n1050, n1051);
    let n1054: ZB = zn_gt(r_c241, zn_splat(P8::from_raw(0i32)));
    let n1055: ZB = zn_le(r_c241, zn_splat(P8::from_raw(0i32)));
    let n1056: ZB = zb_and(n1047, n1054);
    let n1057: ZB = zb_and(n1047, n1055);
    let n1058: ZN = zn_sub(r_c241, zn_splat(P8::from_raw(65536i32)));
    let n1059: ZN = zsel_n(n1054, n1058, r_c241);
    let n1060: ZB = zb_or(n1056, n1057);
    let n1061: ZN = zsel_n(n1037, n1052, r_c239);
    let n1062: ZN = zsel_n(n1037, zn_splat(P8::from_raw(393216i32)), n1059);
    let n1063: ZB = zb_or(n1053, n1060);
    let n1064: ZN = zn_sub(r_c236, zn_splat(P8::from_raw(65536i32)));
    let n1065: ZB = zn_gt(r_c238, zn_splat(P8::from_raw(0i32)));
    let n1066: ZB = zn_le(r_c238, zn_splat(P8::from_raw(0i32)));
    let n1067: ZB = zb_and(n1063, n1065);
    let n1068: ZB = zb_and(n1063, n1066);
    let n1069: ZN = zn_sub(r_c238, zn_splat(P8::from_raw(65536i32)));
    let n1070: ZB = zn_gt(n365, r_c302);
    let n1071: ZB = zn_le(n365, r_c302);
    let n1072: ZB = zb_and(n1067, n1070);
    let n1073: ZB = zb_and(n1067, n1071);
    let n1074: ZN = zn_sub(n365, r_c300);
    let n1075: ZN = zn_max(r_c302, n1074);
    let n1076: ZN = zn_add(r_c300, n365);
    let n1077: ZN = zn_min(r_c302, n1076);
    let n1078: ZN = zsel_n(n1070, n1075, n1077);
    let n1079: ZB = zb_or(n1072, n1073);
    let n1080: ZB = zn_gt(n366, r_c303);
    let n1081: ZB = zn_le(n366, r_c303);
    let n1082: ZB = zb_and(n1079, n1080);
    let n1083: ZB = zb_and(n1079, n1081);
    let n1084: ZN = zn_sub(n366, r_c301);
    let n1085: ZN = zn_max(r_c303, n1084);
    let n1086: ZN = zn_add(r_c301, n366);
    let n1087: ZN = zn_min(r_c303, n1086);
    let n1088: ZN = zsel_n(n1080, n1085, n1087);
    let n1089: ZB = zb_or(n1082, n1083);
    let n1090: ZB = zb_and(n1038, n1068);
    let n1091: ZB = zb_and(n1037, n1068);
    let n1092: ZN = zsel_n(n1038, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1093: ZB = zb_or(n1090, n1091);
    let n1094: ZN = zn_abs(n365);
    let n1095: ZB = zn_gt(n1094, zn_splat(P8::from_raw(65536i32)));
    let n1096: ZB = zn_le(n1094, zn_splat(P8::from_raw(65536i32)));
    let n1097: ZB = zb_and(n1093, n1095);
    let n1098: ZB = zb_and(n1093, n1096);
    let n1099: ZB = zn_gt(n365, zn_splat(P8::from_raw(0i32)));
    let n1100: ZB = zb_and(n1097, n1099);
    let n1101: ZB = zb_and(n448, n1097);
    let n1102: ZB = zn_lt(n365, zn_splat(P8::from_raw(0i32)));
    let n1103: ZB = zb_and(n1101, n1102);
    let n1104: ZB = zb_and(n472, n1101);
    let n1105: ZB = zn_gt(n365, zn_splat(P8::from_raw(65536i32)));
    let n1106: ZB = zn_le(n365, zn_splat(P8::from_raw(65536i32)));
    let n1107: ZB = zb_and(n1100, n1105);
    let n1108: ZB = zb_and(n1100, n1106);
    let n1109: ZN = zn_sub(n365, zn_splat(P8::from_raw(9830i32)));
    let n1110: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1109);
    let n1111: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n365);
    let n1112: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1111);
    let n1113: ZB = zn_gt(n365, zn_splat(P8::from_raw(-65536i32)));
    let n1114: ZB = zn_le(n365, zn_splat(P8::from_raw(-65536i32)));
    let n1115: ZB = zb_and(n1103, n1113);
    let n1116: ZB = zb_and(n1103, n1114);
    let n1117: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1109);
    let n1118: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1111);
    let n1119: ZB = zb_and(n448, n1104);
    let n1120: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1109);
    let n1121: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1111);
    let n1122: ZN = zsel_n(n1113, n1117, n1118);
    let n1123: ZB = zb_or(n1115, n1116);
    let n1124: ZN = zsel_n(n1099, n1120, n1121);
    let n1125: ZN = zsel_n(n1105, n1110, n1112);
    let n1126: ZB = zb_or(n1107, n1108);
    let n1127: ZN = zsel_n(n1102, n1122, n1124);
    let n1128: ZB = zb_or(n1119, n1123);
    let n1129: ZN = zsel_n(n1099, n1125, n1127);
    let n1130: ZB = zb_or(n1126, n1128);
    let n1131: ZB = zb_and(n1098, n1099);
    let n1132: ZB = zb_and(n448, n1098);
    let n1133: ZN = zn_sub(n365, n1092);
    let n1134: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1133);
    let n1135: ZN = zn_add(n365, n1092);
    let n1136: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1135);
    let n1137: ZN = zsel_n(n1099, n1134, n1136);
    let n1138: ZB = zb_or(n1131, n1132);
    let n1139: ZN = zsel_n(n1095, n1129, n1137);
    let n1140: ZB = zb_or(n1130, n1138);
    let n1141: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1139);
    let n1142: ZB = zb_not(n1141);
    let n1143: ZB = zb_and(n1140, n1142);
    let n1144: ZB = zb_and(n1140, n1141);
    let n1145: ZB = zn_lt(n1139, zn_splat(P8::from_raw(0i32)));
    let n1146: ZB = zsel_b(n1142, n1145, r_c304);
    let n1147: ZB = zb_or(n1143, n1144);
    let n1148: ZN = zn_abs(n366);
    let n1149: ZB = zn_le(n1148, zn_splat(P8::from_raw(9830i32)));
    let n1150: ZB = zn_gt(n1148, zn_splat(P8::from_raw(9830i32)));
    let n1151: ZB = zb_and(n1147, n1149);
    let n1152: ZB = zb_and(n1147, n1150);
    let n1153: ZN = zsel_n(n1149, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n1154: ZB = zb_or(n1151, n1152);
    let n1155: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n369);
    let n1156: ZB = zb_and(n1038, n1154);
    let n1157: ZB = zb_and(n1037, n1154);
    let n1158: ZB = zn_gt(n366, zn_splat(P8::from_raw(131072i32)));
    let n1159: ZB = zn_le(n366, zn_splat(P8::from_raw(131072i32)));
    let n1160: ZB = zb_and(n1156, n1158);
    let n1161: ZB = zb_and(n1156, n1159);
    let n1162: ZN = zn_sub(n366, n1153);
    let n1163: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n1162);
    let n1164: ZN = zn_add(n366, n1153);
    let n1165: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n1164);
    let n1166: ZN = zsel_n(n1158, n1163, n1165);
    let n1167: ZB = zb_or(n1160, n1161);
    let n1168: ZN = zsel_n(n1038, n1166, n366);
    let n1169: ZB = zb_or(n1157, n1167);
    let n1170: ZB = zn_gt(n1062, zn_splat(P8::from_raw(0i32)));
    let n1171: ZB = zn_le(n1062, zn_splat(P8::from_raw(0i32)));
    let n1172: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n368);
    let n1173: ZB = zn_tile_flag_at(g.cache, g.cart, n1172, n1155, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1174: ZB = zb_not(n1173);
    let n1175: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n368);
    let n1176: ZB = zn_tile_flag_at(g.cache, g.cart, n1175, n1155, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1177: ZB = zb_not(n1176);
    let n1178: ZN = zsel_n(n1176, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1179: ZN = zsel_n(n1173, zn_splat(P8::from_raw(-65536i32)), n1178);
    let n1180: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1179);
    let n1181: ZB = zb_not(n1180);
    let n1182: ZN = zn_neg(n1179);
    let n1183: ZN = zn_mul(n1182, zn_splat(P8::from_raw(131072i32)));
    let n1184: ZN = zsel_n(n1181, n1183, n1139);
    let n1185: ZN = zsel_n(n1181, zn_splat(P8::from_raw(-131072i32)), n1168);
    let n1186: ZN = zsel_n(n1170, zn_splat(P8::from_raw(0i32)), n1062);
    let n1187: ZN = zsel_n(n1170, n1139, n1184);
    let n1188: ZN = zsel_n(n1170, zn_splat(P8::from_raw(-131072i32)), n1185);
    let n1189: ZB = zn_gt(n1061, zn_splat(P8::from_raw(0i32)));
    let n1190: ZB = zn_le(n1061, zn_splat(P8::from_raw(0i32)));
    let n1191: ZB = zb_and(n1169, n1189);
    let n1192: ZB = zb_and(n1169, n1190);
    let n1193: ZB = zb_or(n1191, n1192);
    let n1194: ZB = zb_not(n1146);
    let n1195: ZN = zsel_n(n1146, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1196: ZB = zn_gt(n1195, zn_splat(P8::from_raw(0i32)));
    let n1197: ZB = zn_le(n1195, zn_splat(P8::from_raw(0i32)));
    let n1198: ZB = zn_lt(n1195, zn_splat(P8::from_raw(0i32)));
    let n1199: ZB = zn_ge(n1195, zn_splat(P8::from_raw(0i32)));
    let n1200: ZN = zsel_n(n1198, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n1201: ZN = zsel_n(n1196, zn_splat(P8::from_raw(131072i32)), n1200);
    let n1202: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1195);
    let n1203: ZB = zb_not(n1202);
    let n1204: ZN = zsel_n(n1203, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n1205: ZN = zsel_n(n1065, n1069, r_c238);
    let n1206: ZB = zsel_b(n1065, r_c304, n1146);
    let n1207: ZN = zsel_n(n1065, n1078, n1139);
    let n1208: ZN = zsel_n(n1065, n1088, n1168);
    let n1209: ZB = zb_or(n1089, n1193);
    let n1210: ZB = zn_lt(n362, zn_splat(P8::from_raw(-262144i32)));
    let n1211: ZB = zn_ge(n362, zn_splat(P8::from_raw(-262144i32)));
    let n1212: ZB = zb_and(n1209, n1210);
    let n1213: ZB = zb_and(n1209, n1211);
    let n1214: ZB = zb_or(n1212, n1213);
    let n1215: ZB = zb_and(n1211, n1214);
    let n1216: ZB = zn_gt(n373, zn_splat(P8::from_raw(786432i32)));
    let n1217: ZB = zn_le(n373, zn_splat(P8::from_raw(786432i32)));
    let n1218: ZB = zb_and(n1215, n1216);
    let n1219: ZB = zb_and(n1215, n1217);
    let n1220: ZN = zn_add(zn_splat(P8::from_raw(0i32)), r_c275);
    let n1221: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1220);
    let n1222: ZB = zn_gt(n385, n1221);
    let n1223: ZB = zb_or(n1218, n1219);
    let n1224: ZB = zb_and(n1216, n1222);
    let n1225: ZB = zb_not(n1224);
    let n1226: ZB = zb_and(n1223, n1224);
    let n1227: ZB = zb_and(n1223, n1225);
    let n1228: ZB = zn_lt(n368, zn_splat(P8::from_raw(1310720i32)));
    let n1229: ZB = zb_or(n1226, n1227);
    let n1230: ZB = zb_and(n1224, n1228);
    let n1231: ZB = zb_not(n1230);
    let n1232: ZB = zb_and(n1229, n1230);
    let n1233: ZB = zb_and(n1229, n1231);
    let n1234: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1220);
    let n1235: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1234);
    let n1236: ZB = zn_lt(n369, n1235);
    let n1237: ZB = zb_or(n1232, n1233);
    let n1238: ZB = zb_and(n1230, n1236);
    let n1239: ZB = zb_and(n1237, n1238);
    let n1240: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n1241: ZB = zb_and(n99, n1239);
    let n1242: ZB = zn_lt(n361, zn_splat(P8::from_raw(-65536i32)));
    let n1243: ZB = zn_ge(n361, zn_splat(P8::from_raw(-65536i32)));
    let n1244: ZB = zb_and(n1241, n1243);
    let n1245: ZB = zb_and(n1241, n1242);
    let n1246: ZB = zn_gt(n361, zn_splat(P8::from_raw(7929856i32)));
    let n1247: ZB = zb_or(n1244, n1245);
    let n1248: ZB = zb_or(n1242, n1246);
    let n1249: ZB = zb_not(n1248);
    let n1250: ZB = zb_and(n1247, n1248);
    let n1251: ZB = zb_and(n1247, n1249);
    let n1252: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n361);
    let n1253: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1252);
    let n1254: ZN = zsel_n(n1248, n1253, n361);
    let n1255: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1207);
    let n1256: ZB = zb_or(n1250, n1251);
    let n1257: ZN = zsel_n(n1240, n361, n1254);
    let n1258: ZN = zsel_n(n1240, n1207, n1255);
    let n1285: ZB = zb_and(n1098, n1113);
    let n1286: ZB = zb_and(n1098, n1114);
    let n1287: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1133);
    let n1288: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1135);
    let n1289: ZN = zsel_n(n1113, n1287, n1288);
    let n1290: ZB = zb_or(n1285, n1286);
    let n1291: ZN = zsel_n(n1095, n1129, n1289);
    let n1292: ZB = zb_or(n1130, n1290);
    let n1293: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1291);
    let n1294: ZB = zb_not(n1293);
    let n1295: ZB = zb_and(n1292, n1294);
    let n1296: ZB = zb_and(n1292, n1293);
    let n1297: ZB = zn_lt(n1291, zn_splat(P8::from_raw(0i32)));
    let n1298: ZB = zsel_b(n1294, n1297, r_c304);
    let n1299: ZB = zb_or(n1295, n1296);
    let n1300: ZB = zb_and(n1149, n1299);
    let n1301: ZB = zb_and(n1150, n1299);
    let n1302: ZB = zb_or(n1300, n1301);
    let n1303: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n368);
    let n1304: ZB = zn_tile_flag_at(g.cache, g.cart, n1303, n1155, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1305: ZB = zb_not(n1304);
    let n1306: ZB = zb_and(n1302, n1305);
    let n1307: ZB = zb_and(n1302, n1304);
    let n1308: ZB = zb_or(n1306, n1307);
    let n1309: ZB = zb_and(n1305, n1308);
    let n1310: ZB = zb_and(n1304, n1308);
    let n1311: ZB = zb_or(n1309, n1310);
    let n1312: ZB = zb_and(n1304, n1311);
    let n1313: ZB = zb_and(n1305, n1311);
    let n1314: ZB = zb_or(n1312, n1313);
    let n1315: ZB = zb_and(n1304, n1314);
    let n1316: ZB = zb_and(n1305, n1314);
    let n1317: ZN = zsel_n(n1304, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1318: ZB = zb_or(n1315, n1316);
    let n1319: ZB = zb_and(n1038, n1318);
    let n1320: ZB = zb_and(n1037, n1318);
    let n1321: ZB = zn_gt(n366, n1317);
    let n1322: ZB = zn_le(n366, n1317);
    let n1323: ZB = zb_and(n1319, n1321);
    let n1324: ZB = zb_and(n1319, n1322);
    let n1325: ZN = zn_max(n1162, n1317);
    let n1326: ZN = zn_min(n1164, n1317);
    let n1327: ZN = zsel_n(n1321, n1325, n1326);
    let n1328: ZB = zb_or(n1323, n1324);
    let n1329: ZN = zsel_n(n1038, n1327, n366);
    let n1330: ZB = zb_or(n1320, n1328);
    let n1331: ZN = zsel_n(n1181, n1183, n1291);
    let n1332: ZN = zsel_n(n1181, zn_splat(P8::from_raw(-131072i32)), n1329);
    let n1333: ZN = zsel_n(n1170, n1291, n1331);
    let n1334: ZN = zsel_n(n1170, zn_splat(P8::from_raw(-131072i32)), n1332);
    let n1335: ZB = zb_and(n1189, n1330);
    let n1336: ZB = zb_and(n1190, n1330);
    let n1337: ZB = zb_or(n1335, n1336);
    let n1338: ZB = zsel_b(n1065, r_c304, n1298);
    let n1339: ZN = zsel_n(n1065, n1078, n1291);
    let n1340: ZN = zsel_n(n1065, n1088, n1329);
    let n1341: ZB = zb_or(n1089, n1337);
    let n1342: ZB = zb_and(n1210, n1341);
    let n1343: ZB = zb_and(n1211, n1341);
    let n1344: ZB = zb_or(n1342, n1343);
    let n1345: ZB = zb_and(n1211, n1344);
    let n1346: ZB = zb_and(n1216, n1345);
    let n1347: ZB = zb_and(n1217, n1345);
    let n1348: ZB = zb_or(n1346, n1347);
    let n1349: ZB = zb_and(n1224, n1348);
    let n1350: ZB = zb_and(n1225, n1348);
    let n1351: ZB = zb_or(n1349, n1350);
    let n1352: ZB = zb_and(n1230, n1351);
    let n1353: ZB = zb_and(n1231, n1351);
    let n1354: ZB = zb_or(n1352, n1353);
    let n1355: ZB = zb_and(n1238, n1354);
    let n1356: ZB = zb_and(n99, n1355);
    let n1357: ZB = zb_and(n1243, n1356);
    let n1358: ZB = zb_and(n1242, n1356);
    let n1359: ZB = zb_or(n1357, n1358);
    let n1360: ZB = zb_and(n1248, n1359);
    let n1361: ZB = zb_and(n1249, n1359);
    let n1362: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1339);
    let n1363: ZB = zb_or(n1360, n1361);
    let n1364: ZN = zsel_n(n1240, n1339, n1362);
    let n1365: ZB = zb_and(n1098, n1105);
    let n1366: ZB = zb_and(n1098, n1106);
    let n1367: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1133);
    let n1368: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1135);
    let n1369: ZN = zsel_n(n1105, n1367, n1368);
    let n1370: ZB = zb_or(n1365, n1366);
    let n1371: ZN = zsel_n(n1095, n1129, n1369);
    let n1372: ZB = zb_or(n1130, n1370);
    let n1373: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1371);
    let n1374: ZB = zb_not(n1373);
    let n1375: ZB = zb_and(n1372, n1374);
    let n1376: ZB = zb_and(n1372, n1373);
    let n1377: ZB = zn_lt(n1371, zn_splat(P8::from_raw(0i32)));
    let n1378: ZB = zsel_b(n1374, n1377, r_c304);
    let n1379: ZB = zb_or(n1375, n1376);
    let n1380: ZB = zb_and(n1149, n1379);
    let n1381: ZB = zb_and(n1150, n1379);
    let n1382: ZB = zb_or(n1380, n1381);
    let n1383: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n368);
    let n1384: ZB = zn_tile_flag_at(g.cache, g.cart, n1383, n1155, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1385: ZB = zb_not(n1384);
    let n1386: ZB = zb_and(n1382, n1385);
    let n1387: ZB = zb_and(n1382, n1384);
    let n1388: ZB = zb_or(n1386, n1387);
    let n1389: ZB = zb_and(n1385, n1388);
    let n1390: ZB = zb_and(n1384, n1388);
    let n1391: ZB = zb_or(n1389, n1390);
    let n1392: ZB = zb_and(n1384, n1391);
    let n1393: ZB = zb_and(n1385, n1391);
    let n1394: ZB = zb_or(n1392, n1393);
    let n1395: ZB = zb_and(n1384, n1394);
    let n1396: ZB = zb_and(n1385, n1394);
    let n1397: ZN = zsel_n(n1384, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1398: ZB = zb_or(n1395, n1396);
    let n1399: ZB = zb_and(n1038, n1398);
    let n1400: ZB = zb_and(n1037, n1398);
    let n1401: ZB = zn_gt(n366, n1397);
    let n1402: ZB = zn_le(n366, n1397);
    let n1403: ZB = zb_and(n1399, n1401);
    let n1404: ZB = zb_and(n1399, n1402);
    let n1405: ZN = zn_max(n1162, n1397);
    let n1406: ZN = zn_min(n1164, n1397);
    let n1407: ZN = zsel_n(n1401, n1405, n1406);
    let n1408: ZB = zb_or(n1403, n1404);
    let n1409: ZN = zsel_n(n1038, n1407, n366);
    let n1410: ZB = zb_or(n1400, n1408);
    let n1411: ZN = zsel_n(n1181, n1183, n1371);
    let n1412: ZN = zsel_n(n1181, zn_splat(P8::from_raw(-131072i32)), n1409);
    let n1413: ZN = zsel_n(n1170, n1371, n1411);
    let n1414: ZN = zsel_n(n1170, zn_splat(P8::from_raw(-131072i32)), n1412);
    let n1415: ZB = zb_and(n1189, n1410);
    let n1416: ZB = zb_and(n1190, n1410);
    let n1417: ZB = zb_or(n1415, n1416);
    let n1418: ZB = zsel_b(n1065, r_c304, n1378);
    let n1419: ZN = zsel_n(n1065, n1078, n1371);
    let n1420: ZN = zsel_n(n1065, n1088, n1409);
    let n1421: ZB = zb_or(n1089, n1417);
    let n1422: ZB = zb_and(n1210, n1421);
    let n1423: ZB = zb_and(n1211, n1421);
    let n1424: ZB = zb_or(n1422, n1423);
    let n1425: ZB = zb_and(n1211, n1424);
    let n1426: ZB = zb_and(n1216, n1425);
    let n1427: ZB = zb_and(n1217, n1425);
    let n1428: ZB = zb_or(n1426, n1427);
    let n1429: ZB = zb_and(n1224, n1428);
    let n1430: ZB = zb_and(n1225, n1428);
    let n1431: ZB = zb_or(n1429, n1430);
    let n1432: ZB = zb_and(n1230, n1431);
    let n1433: ZB = zb_and(n1231, n1431);
    let n1434: ZB = zb_or(n1432, n1433);
    let n1435: ZB = zb_and(n1238, n1434);
    let n1436: ZB = zb_and(n99, n1435);
    let n1437: ZB = zb_and(n1243, n1436);
    let n1438: ZB = zb_and(n1242, n1436);
    let n1439: ZB = zb_or(n1437, n1438);
    let n1440: ZB = zb_and(n1248, n1439);
    let n1441: ZB = zb_and(n1249, n1439);
    let n1442: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1419);
    let n1443: ZB = zb_or(n1440, n1441);
    let n1444: ZN = zsel_n(n1240, n1419, n1442);
    let n1445: ZB = zb_and(n1045, n1169);
    let n1446: ZB = zb_and(r_c249, n1169);
    let n1447: ZB = zb_and(n1170, n1445);
    let n1448: ZB = zb_and(n1171, n1445);
    let n1449: ZB = zb_and(n1174, n1448);
    let n1450: ZB = zb_and(n1173, n1448);
    let n1451: ZB = zb_or(n1449, n1450);
    let n1452: ZB = zb_and(n1174, n1451);
    let n1453: ZB = zb_and(n1173, n1451);
    let n1454: ZB = zb_or(n1452, n1453);
    let n1455: ZB = zb_and(n1173, n1454);
    let n1456: ZB = zb_and(n1174, n1454);
    let n1457: ZB = zb_and(n1177, n1456);
    let n1458: ZB = zb_and(n1176, n1456);
    let n1459: ZB = zb_or(n1457, n1458);
    let n1460: ZB = zb_and(n1177, n1459);
    let n1461: ZB = zb_and(n1176, n1459);
    let n1462: ZB = zb_or(n1460, n1461);
    let n1463: ZB = zb_and(n1176, n1462);
    let n1464: ZB = zb_and(n1177, n1462);
    let n1465: ZB = zb_or(n1463, n1464);
    let n1466: ZB = zb_or(n1455, n1465);
    let n1467: ZB = zb_and(n1181, n1466);
    let n1468: ZB = zb_and(n1180, n1466);
    let n1469: ZB = zb_or(n1467, n1468);
    let n1470: ZB = zb_or(n1447, n1469);
    let n1471: ZN = zsel_n(n1045, n1186, n1062);
    let n1472: ZN = zsel_n(n1045, n1187, n1139);
    let n1473: ZN = zsel_n(n1045, n1188, n1168);
    let n1474: ZB = zb_or(n1446, n1470);
    let n1475: ZB = zb_and(n1189, n1474);
    let n1476: ZB = zb_and(n1190, n1474);
    let n1477: ZB = zb_or(n1475, n1476);
    let n1478: ZN = zsel_n(n1065, n1062, n1471);
    let n1479: ZN = zsel_n(n1065, n1078, n1472);
    let n1480: ZN = zsel_n(n1065, n1088, n1473);
    let n1481: ZB = zb_or(n1089, n1477);
    let n1482: ZB = zb_and(n1210, n1481);
    let n1483: ZB = zb_and(n1211, n1481);
    let n1484: ZB = zb_or(n1482, n1483);
    let n1485: ZB = zb_and(n1211, n1484);
    let n1486: ZB = zb_and(n1216, n1485);
    let n1487: ZB = zb_and(n1217, n1485);
    let n1488: ZB = zb_or(n1486, n1487);
    let n1489: ZB = zb_and(n1224, n1488);
    let n1490: ZB = zb_and(n1225, n1488);
    let n1491: ZB = zb_or(n1489, n1490);
    let n1492: ZB = zb_and(n1230, n1491);
    let n1493: ZB = zb_and(n1231, n1491);
    let n1494: ZB = zb_or(n1492, n1493);
    let n1495: ZB = zb_and(n1238, n1494);
    let n1496: ZB = zb_and(n99, n1495);
    let n1497: ZB = zb_and(n1243, n1496);
    let n1498: ZB = zb_and(n1242, n1496);
    let n1499: ZB = zb_or(n1497, n1498);
    let n1500: ZB = zb_and(n1248, n1499);
    let n1501: ZB = zb_and(n1249, n1499);
    let n1502: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1479);
    let n1503: ZB = zb_or(n1500, n1501);
    let n1504: ZN = zsel_n(n1240, n1479, n1502);
    let n1505: ZB = zb_and(n1045, n1330);
    let n1506: ZB = zb_and(r_c249, n1330);
    let n1507: ZB = zb_and(n1170, n1505);
    let n1508: ZB = zb_and(n1171, n1505);
    let n1509: ZB = zb_and(n1174, n1508);
    let n1510: ZB = zb_and(n1173, n1508);
    let n1511: ZB = zb_or(n1509, n1510);
    let n1512: ZB = zb_and(n1174, n1511);
    let n1513: ZB = zb_and(n1173, n1511);
    let n1514: ZB = zb_or(n1512, n1513);
    let n1515: ZB = zb_and(n1173, n1514);
    let n1516: ZB = zb_and(n1174, n1514);
    let n1517: ZB = zb_and(n1177, n1516);
    let n1518: ZB = zb_and(n1176, n1516);
    let n1519: ZB = zb_or(n1517, n1518);
    let n1520: ZB = zb_and(n1177, n1519);
    let n1521: ZB = zb_and(n1176, n1519);
    let n1522: ZB = zb_or(n1520, n1521);
    let n1523: ZB = zb_and(n1176, n1522);
    let n1524: ZB = zb_and(n1177, n1522);
    let n1525: ZB = zb_or(n1523, n1524);
    let n1526: ZB = zb_or(n1515, n1525);
    let n1527: ZB = zb_and(n1181, n1526);
    let n1528: ZB = zb_and(n1180, n1526);
    let n1529: ZB = zb_or(n1527, n1528);
    let n1530: ZB = zb_or(n1507, n1529);
    let n1531: ZN = zsel_n(n1045, n1333, n1291);
    let n1532: ZN = zsel_n(n1045, n1334, n1329);
    let n1533: ZB = zb_or(n1506, n1530);
    let n1534: ZB = zb_and(n1189, n1533);
    let n1535: ZB = zb_and(n1190, n1533);
    let n1536: ZB = zb_or(n1534, n1535);
    let n1537: ZN = zsel_n(n1065, n1078, n1531);
    let n1538: ZN = zsel_n(n1065, n1088, n1532);
    let n1539: ZB = zb_or(n1089, n1536);
    let n1540: ZB = zb_and(n1210, n1539);
    let n1541: ZB = zb_and(n1211, n1539);
    let n1542: ZB = zb_or(n1540, n1541);
    let n1543: ZB = zb_and(n1211, n1542);
    let n1544: ZB = zb_and(n1216, n1543);
    let n1545: ZB = zb_and(n1217, n1543);
    let n1546: ZB = zb_or(n1544, n1545);
    let n1547: ZB = zb_and(n1224, n1546);
    let n1548: ZB = zb_and(n1225, n1546);
    let n1549: ZB = zb_or(n1547, n1548);
    let n1550: ZB = zb_and(n1230, n1549);
    let n1551: ZB = zb_and(n1231, n1549);
    let n1552: ZB = zb_or(n1550, n1551);
    let n1553: ZB = zb_and(n1238, n1552);
    let n1554: ZB = zb_and(n99, n1553);
    let n1555: ZB = zb_and(n1243, n1554);
    let n1556: ZB = zb_and(n1242, n1554);
    let n1557: ZB = zb_or(n1555, n1556);
    let n1558: ZB = zb_and(n1248, n1557);
    let n1559: ZB = zb_and(n1249, n1557);
    let n1560: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1537);
    let n1561: ZB = zb_or(n1558, n1559);
    let n1562: ZN = zsel_n(n1240, n1537, n1560);
    let n1563: ZB = zb_and(n1045, n1410);
    let n1564: ZB = zb_and(r_c249, n1410);
    let n1565: ZB = zb_and(n1170, n1563);
    let n1566: ZB = zb_and(n1171, n1563);
    let n1567: ZB = zb_and(n1174, n1566);
    let n1568: ZB = zb_and(n1173, n1566);
    let n1569: ZB = zb_or(n1567, n1568);
    let n1570: ZB = zb_and(n1174, n1569);
    let n1571: ZB = zb_and(n1173, n1569);
    let n1572: ZB = zb_or(n1570, n1571);
    let n1573: ZB = zb_and(n1173, n1572);
    let n1574: ZB = zb_and(n1174, n1572);
    let n1575: ZB = zb_and(n1177, n1574);
    let n1576: ZB = zb_and(n1176, n1574);
    let n1577: ZB = zb_or(n1575, n1576);
    let n1578: ZB = zb_and(n1177, n1577);
    let n1579: ZB = zb_and(n1176, n1577);
    let n1580: ZB = zb_or(n1578, n1579);
    let n1581: ZB = zb_and(n1176, n1580);
    let n1582: ZB = zb_and(n1177, n1580);
    let n1583: ZB = zb_or(n1581, n1582);
    let n1584: ZB = zb_or(n1573, n1583);
    let n1585: ZB = zb_and(n1181, n1584);
    let n1586: ZB = zb_and(n1180, n1584);
    let n1587: ZB = zb_or(n1585, n1586);
    let n1588: ZB = zb_or(n1565, n1587);
    let n1589: ZN = zsel_n(n1045, n1413, n1371);
    let n1590: ZN = zsel_n(n1045, n1414, n1409);
    let n1591: ZB = zb_or(n1564, n1588);
    let n1592: ZB = zb_and(n1189, n1591);
    let n1593: ZB = zb_and(n1190, n1591);
    let n1594: ZB = zb_or(n1592, n1593);
    let n1595: ZN = zsel_n(n1065, n1078, n1589);
    let n1596: ZN = zsel_n(n1065, n1088, n1590);
    let n1597: ZB = zb_or(n1089, n1594);
    let n1598: ZB = zb_and(n1210, n1597);
    let n1599: ZB = zb_and(n1211, n1597);
    let n1600: ZB = zb_or(n1598, n1599);
    let n1601: ZB = zb_and(n1211, n1600);
    let n1602: ZB = zb_and(n1216, n1601);
    let n1603: ZB = zb_and(n1217, n1601);
    let n1604: ZB = zb_or(n1602, n1603);
    let n1605: ZB = zb_and(n1224, n1604);
    let n1606: ZB = zb_and(n1225, n1604);
    let n1607: ZB = zb_or(n1605, n1606);
    let n1608: ZB = zb_and(n1230, n1607);
    let n1609: ZB = zb_and(n1231, n1607);
    let n1610: ZB = zb_or(n1608, n1609);
    let n1611: ZB = zb_and(n1238, n1610);
    let n1612: ZB = zb_and(n99, n1611);
    let n1613: ZB = zb_and(n1243, n1612);
    let n1614: ZB = zb_and(n1242, n1612);
    let n1615: ZB = zb_or(n1613, n1614);
    let n1616: ZB = zb_and(n1248, n1615);
    let n1617: ZB = zb_and(n1249, n1615);
    let n1618: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1595);
    let n1619: ZB = zb_or(n1616, n1617);
    let n1620: ZN = zsel_n(n1240, n1595, n1618);
    let n1621: ZB = zb_and(n97, n1189);
    let n1622: ZB = zb_not(n1621);
    let n1623: ZB = zb_and(n1193, n1621);
    let n1624: ZB = zb_and(n1193, n1622);
    let n1625: ZB = zb_and(n1146, n1623);
    let n1626: ZB = zb_and(n1194, n1623);
    let n1627: ZB = zb_or(n1625, n1626);
    let n1628: ZB = zb_and(n1196, n1627);
    let n1629: ZB = zb_and(n1197, n1627);
    let n1630: ZB = zb_and(n1198, n1629);
    let n1631: ZB = zb_and(n1199, n1629);
    let n1632: ZB = zb_or(n1630, n1631);
    let n1633: ZB = zb_or(n1628, n1632);
    let n1634: ZB = zb_and(n1203, n1633);
    let n1635: ZB = zb_and(n1202, n1633);
    let n1636: ZB = zb_or(n1634, n1635);
    let n1637: ZN = zsel_n(n1621, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1638: ZB = zb_or(r_c41, n1621);
    let n1639: ZN = zsel_n(n1621, zn_splat(P8::from_raw(655360i32)), n1064);
    let n1640: ZN = zsel_n(n1621, zn_splat(P8::from_raw(262144i32)), r_c238);
    let n1641: ZN = zsel_n(n1621, zn_splat(P8::from_raw(98304i32)), r_c300);
    let n1642: ZN = zsel_n(n1621, n1204, r_c301);
    let n1643: ZN = zsel_n(n1621, n1201, r_c302);
    let n1644: ZN = zsel_n(n1621, zn_splat(P8::from_raw(0i32)), r_c303);
    let n1645: ZN = zsel_n(n1621, n1195, n1139);
    let n1646: ZN = zsel_n(n1621, zn_splat(P8::from_raw(0i32)), n1168);
    let n1647: ZB = zb_or(n1624, n1636);
    let n1648: ZN = zsel_n(n1065, r_c20, n1637);
    let n1649: ZB = zsel_b(n1065, r_c41, n1638);
    let n1650: ZN = zsel_n(n1065, n1064, n1639);
    let n1651: ZN = zsel_n(n1065, n1069, n1640);
    let n1652: ZN = zsel_n(n1065, r_c300, n1641);
    let n1653: ZN = zsel_n(n1065, r_c301, n1642);
    let n1654: ZN = zsel_n(n1065, r_c302, n1643);
    let n1655: ZN = zsel_n(n1065, r_c303, n1644);
    let n1656: ZN = zsel_n(n1065, n1078, n1645);
    let n1657: ZN = zsel_n(n1065, n1088, n1646);
    let n1658: ZB = zb_or(n1089, n1647);
    let n1659: ZB = zb_and(n1210, n1658);
    let n1660: ZB = zb_and(n1211, n1658);
    let n1661: ZB = zb_or(n1659, n1660);
    let n1662: ZB = zb_and(n1211, n1661);
    let n1663: ZB = zb_and(n1216, n1662);
    let n1664: ZB = zb_and(n1217, n1662);
    let n1665: ZB = zb_or(n1663, n1664);
    let n1666: ZB = zb_and(n1224, n1665);
    let n1667: ZB = zb_and(n1225, n1665);
    let n1668: ZB = zb_or(n1666, n1667);
    let n1669: ZB = zb_and(n1230, n1668);
    let n1670: ZB = zb_and(n1231, n1668);
    let n1671: ZB = zb_or(n1669, n1670);
    let n1672: ZB = zb_and(n1238, n1671);
    let n1673: ZB = zn_gt(n1648, zn_splat(P8::from_raw(0i32)));
    let n1674: ZB = zn_le(n1648, zn_splat(P8::from_raw(0i32)));
    let n1675: ZB = zb_and(n1672, n1673);
    let n1676: ZB = zb_and(n1672, n1674);
    let n1677: ZB = zb_and(n1243, n1676);
    let n1678: ZB = zb_and(n1242, n1676);
    let n1679: ZB = zb_or(n1677, n1678);
    let n1680: ZB = zb_and(n1248, n1679);
    let n1681: ZB = zb_and(n1249, n1679);
    let n1682: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1656);
    let n1683: ZB = zb_or(n1680, n1681);
    let n1684: ZN = zsel_n(n1673, n361, n1254);
    let n1685: ZN = zsel_n(n1673, n1656, n1682);
    let n1686: ZB = zb_or(n1675, n1683);
    let n1687: ZB = zb_and(n1337, n1621);
    let n1688: ZB = zb_and(n1337, n1622);
    let n1689: ZN = zsel_n(n1621, zn_splat(P8::from_raw(69510i32)), r_c301);
    let n1690: ZN = zsel_n(n1621, zn_splat(P8::from_raw(-131072i32)), r_c302);
    let n1691: ZN = zsel_n(n1621, zn_splat(P8::from_raw(-327680i32)), n1291);
    let n1692: ZN = zsel_n(n1621, zn_splat(P8::from_raw(0i32)), n1329);
    let n1693: ZB = zb_or(n1687, n1688);
    let n1694: ZN = zsel_n(n1065, r_c301, n1689);
    let n1695: ZN = zsel_n(n1065, r_c302, n1690);
    let n1696: ZN = zsel_n(n1065, n1078, n1691);
    let n1697: ZN = zsel_n(n1065, n1088, n1692);
    let n1698: ZB = zb_or(n1089, n1693);
    let n1699: ZB = zb_and(n1210, n1698);
    let n1700: ZB = zb_and(n1211, n1698);
    let n1701: ZB = zb_or(n1699, n1700);
    let n1702: ZB = zb_and(n1211, n1701);
    let n1703: ZB = zb_and(n1216, n1702);
    let n1704: ZB = zb_and(n1217, n1702);
    let n1705: ZB = zb_or(n1703, n1704);
    let n1706: ZB = zb_and(n1224, n1705);
    let n1707: ZB = zb_and(n1225, n1705);
    let n1708: ZB = zb_or(n1706, n1707);
    let n1709: ZB = zb_and(n1230, n1708);
    let n1710: ZB = zb_and(n1231, n1708);
    let n1711: ZB = zb_or(n1709, n1710);
    let n1712: ZB = zb_and(n1238, n1711);
    let n1713: ZB = zb_and(n1673, n1712);
    let n1714: ZB = zb_and(n1674, n1712);
    let n1715: ZB = zb_and(n1243, n1714);
    let n1716: ZB = zb_and(n1242, n1714);
    let n1717: ZB = zb_or(n1715, n1716);
    let n1718: ZB = zb_and(n1248, n1717);
    let n1719: ZB = zb_and(n1249, n1717);
    let n1720: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1696);
    let n1721: ZB = zb_or(n1718, n1719);
    let n1722: ZN = zsel_n(n1673, n1696, n1720);
    let n1723: ZB = zb_or(n1713, n1721);
    let n1724: ZB = zb_and(n1417, n1621);
    let n1725: ZB = zb_and(n1417, n1622);
    let n1726: ZN = zsel_n(n1621, zn_splat(P8::from_raw(131072i32)), r_c302);
    let n1727: ZN = zsel_n(n1621, zn_splat(P8::from_raw(327680i32)), n1371);
    let n1728: ZN = zsel_n(n1621, zn_splat(P8::from_raw(0i32)), n1409);
    let n1729: ZB = zb_or(n1724, n1725);
    let n1730: ZN = zsel_n(n1065, r_c302, n1726);
    let n1731: ZN = zsel_n(n1065, n1078, n1727);
    let n1732: ZN = zsel_n(n1065, n1088, n1728);
    let n1733: ZB = zb_or(n1089, n1729);
    let n1734: ZB = zb_and(n1210, n1733);
    let n1735: ZB = zb_and(n1211, n1733);
    let n1736: ZB = zb_or(n1734, n1735);
    let n1737: ZB = zb_and(n1211, n1736);
    let n1738: ZB = zb_and(n1216, n1737);
    let n1739: ZB = zb_and(n1217, n1737);
    let n1740: ZB = zb_or(n1738, n1739);
    let n1741: ZB = zb_and(n1224, n1740);
    let n1742: ZB = zb_and(n1225, n1740);
    let n1743: ZB = zb_or(n1741, n1742);
    let n1744: ZB = zb_and(n1230, n1743);
    let n1745: ZB = zb_and(n1231, n1743);
    let n1746: ZB = zb_or(n1744, n1745);
    let n1747: ZB = zb_and(n1238, n1746);
    let n1748: ZB = zb_and(n1673, n1747);
    let n1749: ZB = zb_and(n1674, n1747);
    let n1750: ZB = zb_and(n1243, n1749);
    let n1751: ZB = zb_and(n1242, n1749);
    let n1752: ZB = zb_or(n1750, n1751);
    let n1753: ZB = zb_and(n1248, n1752);
    let n1754: ZB = zb_and(n1249, n1752);
    let n1755: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1731);
    let n1756: ZB = zb_or(n1753, n1754);
    let n1757: ZN = zsel_n(n1673, n1731, n1755);
    let n1758: ZB = zb_or(n1748, n1756);
    let n1760: ZN = zsel_n(n1621, zn_splat(P8::from_raw(69510i32)), r_c300);
    let n1761: ZN = zsel_n(n1621, zn_splat(P8::from_raw(98304i32)), r_c301);
    let n1762: ZN = zsel_n(n1621, zn_splat(P8::from_raw(0i32)), r_c302);
    let n1763: ZN = zsel_n(n1621, zn_splat(P8::from_raw(-98304i32)), r_c303);
    let n1764: ZN = zsel_n(n1621, zn_splat(P8::from_raw(0i32)), n1139);
    let n1765: ZN = zsel_n(n1621, zn_splat(P8::from_raw(-327680i32)), n1168);
    let n1766: ZB = zb_or(n1623, n1624);
    let n1767: ZN = zsel_n(n1065, r_c300, n1760);
    let n1768: ZN = zsel_n(n1065, r_c301, n1761);
    let n1769: ZN = zsel_n(n1065, r_c302, n1762);
    let n1770: ZN = zsel_n(n1065, r_c303, n1763);
    let n1771: ZN = zsel_n(n1065, n1078, n1764);
    let n1772: ZN = zsel_n(n1065, n1088, n1765);
    let n1773: ZB = zb_or(n1089, n1766);
    let n1774: ZB = zb_and(n1210, n1773);
    let n1775: ZB = zb_and(n1211, n1773);
    let n1776: ZB = zb_or(n1774, n1775);
    let n1777: ZB = zb_and(n1211, n1776);
    let n1778: ZB = zb_and(n1216, n1777);
    let n1779: ZB = zb_and(n1217, n1777);
    let n1780: ZB = zb_or(n1778, n1779);
    let n1781: ZB = zb_and(n1224, n1780);
    let n1782: ZB = zb_and(n1225, n1780);
    let n1783: ZB = zb_or(n1781, n1782);
    let n1784: ZB = zb_and(n1230, n1783);
    let n1785: ZB = zb_and(n1231, n1783);
    let n1786: ZB = zb_or(n1784, n1785);
    let n1787: ZB = zb_and(n1238, n1786);
    let n1788: ZB = zb_and(n1673, n1787);
    let n1789: ZB = zb_and(n1674, n1787);
    let n1790: ZB = zb_and(n1243, n1789);
    let n1791: ZB = zb_and(n1242, n1789);
    let n1792: ZB = zb_or(n1790, n1791);
    let n1793: ZB = zb_and(n1248, n1792);
    let n1794: ZB = zb_and(n1249, n1792);
    let n1795: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1771);
    let n1796: ZB = zb_or(n1793, n1794);
    let n1797: ZN = zsel_n(n1673, n1771, n1795);
    let n1798: ZB = zb_or(n1788, n1796);
    let n1799: ZN = zsel_n(n1621, zn_splat(P8::from_raw(-231700i32)), n1291);
    let n1800: ZN = zsel_n(n1621, zn_splat(P8::from_raw(-231700i32)), n1329);
    let n1801: ZN = zsel_n(n1065, n1078, n1799);
    let n1802: ZN = zsel_n(n1065, n1088, n1800);
    let n1803: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1801);
    let n1804: ZN = zsel_n(n1673, n1801, n1803);
    let n1805: ZN = zsel_n(n1621, zn_splat(P8::from_raw(231700i32)), n1371);
    let n1806: ZN = zsel_n(n1621, zn_splat(P8::from_raw(-231700i32)), n1409);
    let n1807: ZN = zsel_n(n1065, n1078, n1805);
    let n1808: ZN = zsel_n(n1065, n1088, n1806);
    let n1809: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1807);
    let n1810: ZN = zsel_n(n1673, n1807, n1809);
    let n1811: ZN = zsel_n(n1621, zn_splat(P8::from_raw(131072i32)), r_c303);
    let n1812: ZN = zsel_n(n1621, zn_splat(P8::from_raw(327680i32)), n1168);
    let n1813: ZN = zsel_n(n1065, r_c303, n1811);
    let n1814: ZN = zsel_n(n1065, n1088, n1812);
    let n1815: ZN = zsel_n(n1621, zn_splat(P8::from_raw(231700i32)), n1329);
    let n1816: ZN = zsel_n(n1065, n1088, n1815);
    let n1817: ZN = zsel_n(n1621, zn_splat(P8::from_raw(231700i32)), n1409);
    let n1818: ZN = zsel_n(n1065, n1088, n1817);
    let n1819: ZB = zb_and(n1477, n1621);
    let n1820: ZB = zb_and(n1477, n1622);
    let n1821: ZB = zb_and(n1146, n1819);
    let n1822: ZB = zb_and(n1194, n1819);
    let n1823: ZB = zb_or(n1821, n1822);
    let n1824: ZB = zb_and(n1196, n1823);
    let n1825: ZB = zb_and(n1197, n1823);
    let n1826: ZB = zb_and(n1198, n1825);
    let n1827: ZB = zb_and(n1199, n1825);
    let n1828: ZB = zb_or(n1826, n1827);
    let n1829: ZB = zb_or(n1824, n1828);
    let n1830: ZB = zb_and(n1203, n1829);
    let n1831: ZB = zb_and(n1202, n1829);
    let n1832: ZB = zb_or(n1830, n1831);
    let n1833: ZN = zsel_n(n1621, n1195, n1472);
    let n1834: ZN = zsel_n(n1621, zn_splat(P8::from_raw(0i32)), n1473);
    let n1835: ZB = zb_or(n1820, n1832);
    let n1836: ZN = zsel_n(n1065, n1078, n1833);
    let n1837: ZN = zsel_n(n1065, n1088, n1834);
    let n1838: ZB = zb_or(n1089, n1835);
    let n1839: ZB = zb_and(n1210, n1838);
    let n1840: ZB = zb_and(n1211, n1838);
    let n1841: ZB = zb_or(n1839, n1840);
    let n1842: ZB = zb_and(n1211, n1841);
    let n1843: ZB = zb_and(n1216, n1842);
    let n1844: ZB = zb_and(n1217, n1842);
    let n1845: ZB = zb_or(n1843, n1844);
    let n1846: ZB = zb_and(n1224, n1845);
    let n1847: ZB = zb_and(n1225, n1845);
    let n1848: ZB = zb_or(n1846, n1847);
    let n1849: ZB = zb_and(n1230, n1848);
    let n1850: ZB = zb_and(n1231, n1848);
    let n1851: ZB = zb_or(n1849, n1850);
    let n1852: ZB = zb_and(n1238, n1851);
    let n1853: ZB = zb_and(n1673, n1852);
    let n1854: ZB = zb_and(n1674, n1852);
    let n1855: ZB = zb_and(n1243, n1854);
    let n1856: ZB = zb_and(n1242, n1854);
    let n1857: ZB = zb_or(n1855, n1856);
    let n1858: ZB = zb_and(n1248, n1857);
    let n1859: ZB = zb_and(n1249, n1857);
    let n1860: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1836);
    let n1861: ZB = zb_or(n1858, n1859);
    let n1862: ZN = zsel_n(n1673, n1836, n1860);
    let n1863: ZB = zb_or(n1853, n1861);
    let n1864: ZB = zb_and(n1536, n1621);
    let n1865: ZB = zb_and(n1536, n1622);
    let n1866: ZN = zsel_n(n1621, zn_splat(P8::from_raw(-327680i32)), n1531);
    let n1867: ZN = zsel_n(n1621, zn_splat(P8::from_raw(0i32)), n1532);
    let n1868: ZB = zb_or(n1864, n1865);
    let n1869: ZN = zsel_n(n1065, n1078, n1866);
    let n1870: ZN = zsel_n(n1065, n1088, n1867);
    let n1871: ZB = zb_or(n1089, n1868);
    let n1872: ZB = zb_and(n1210, n1871);
    let n1873: ZB = zb_and(n1211, n1871);
    let n1874: ZB = zb_or(n1872, n1873);
    let n1875: ZB = zb_and(n1211, n1874);
    let n1876: ZB = zb_and(n1216, n1875);
    let n1877: ZB = zb_and(n1217, n1875);
    let n1878: ZB = zb_or(n1876, n1877);
    let n1879: ZB = zb_and(n1224, n1878);
    let n1880: ZB = zb_and(n1225, n1878);
    let n1881: ZB = zb_or(n1879, n1880);
    let n1882: ZB = zb_and(n1230, n1881);
    let n1883: ZB = zb_and(n1231, n1881);
    let n1884: ZB = zb_or(n1882, n1883);
    let n1885: ZB = zb_and(n1238, n1884);
    let n1886: ZB = zb_and(n1673, n1885);
    let n1887: ZB = zb_and(n1674, n1885);
    let n1888: ZB = zb_and(n1243, n1887);
    let n1889: ZB = zb_and(n1242, n1887);
    let n1890: ZB = zb_or(n1888, n1889);
    let n1891: ZB = zb_and(n1248, n1890);
    let n1892: ZB = zb_and(n1249, n1890);
    let n1893: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1869);
    let n1894: ZB = zb_or(n1891, n1892);
    let n1895: ZN = zsel_n(n1673, n1869, n1893);
    let n1896: ZB = zb_or(n1886, n1894);
    let n1897: ZB = zb_and(n1594, n1621);
    let n1898: ZB = zb_and(n1594, n1622);
    let n1899: ZN = zsel_n(n1621, zn_splat(P8::from_raw(327680i32)), n1589);
    let n1900: ZN = zsel_n(n1621, zn_splat(P8::from_raw(0i32)), n1590);
    let n1901: ZB = zb_or(n1897, n1898);
    let n1902: ZN = zsel_n(n1065, n1078, n1899);
    let n1903: ZN = zsel_n(n1065, n1088, n1900);
    let n1904: ZB = zb_or(n1089, n1901);
    let n1905: ZB = zb_and(n1210, n1904);
    let n1906: ZB = zb_and(n1211, n1904);
    let n1907: ZB = zb_or(n1905, n1906);
    let n1908: ZB = zb_and(n1211, n1907);
    let n1909: ZB = zb_and(n1216, n1908);
    let n1910: ZB = zb_and(n1217, n1908);
    let n1911: ZB = zb_or(n1909, n1910);
    let n1912: ZB = zb_and(n1224, n1911);
    let n1913: ZB = zb_and(n1225, n1911);
    let n1914: ZB = zb_or(n1912, n1913);
    let n1915: ZB = zb_and(n1230, n1914);
    let n1916: ZB = zb_and(n1231, n1914);
    let n1917: ZB = zb_or(n1915, n1916);
    let n1918: ZB = zb_and(n1238, n1917);
    let n1919: ZB = zb_and(n1673, n1918);
    let n1920: ZB = zb_and(n1674, n1918);
    let n1921: ZB = zb_and(n1243, n1920);
    let n1922: ZB = zb_and(n1242, n1920);
    let n1923: ZB = zb_or(n1921, n1922);
    let n1924: ZB = zb_and(n1248, n1923);
    let n1925: ZB = zb_and(n1249, n1923);
    let n1926: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1902);
    let n1927: ZB = zb_or(n1924, n1925);
    let n1928: ZN = zsel_n(n1673, n1902, n1926);
    let n1929: ZB = zb_or(n1919, n1927);
    let n1930: ZN = zsel_n(n1621, zn_splat(P8::from_raw(0i32)), n1472);
    let n1931: ZN = zsel_n(n1621, zn_splat(P8::from_raw(-327680i32)), n1473);
    let n1932: ZB = zb_or(n1819, n1820);
    let n1933: ZN = zsel_n(n1065, n1078, n1930);
    let n1934: ZN = zsel_n(n1065, n1088, n1931);
    let n1935: ZB = zb_or(n1089, n1932);
    let n1936: ZB = zb_and(n1210, n1935);
    let n1937: ZB = zb_and(n1211, n1935);
    let n1938: ZB = zb_or(n1936, n1937);
    let n1939: ZB = zb_and(n1211, n1938);
    let n1940: ZB = zb_and(n1216, n1939);
    let n1941: ZB = zb_and(n1217, n1939);
    let n1942: ZB = zb_or(n1940, n1941);
    let n1943: ZB = zb_and(n1224, n1942);
    let n1944: ZB = zb_and(n1225, n1942);
    let n1945: ZB = zb_or(n1943, n1944);
    let n1946: ZB = zb_and(n1230, n1945);
    let n1947: ZB = zb_and(n1231, n1945);
    let n1948: ZB = zb_or(n1946, n1947);
    let n1949: ZB = zb_and(n1238, n1948);
    let n1950: ZB = zb_and(n1673, n1949);
    let n1951: ZB = zb_and(n1674, n1949);
    let n1952: ZB = zb_and(n1243, n1951);
    let n1953: ZB = zb_and(n1242, n1951);
    let n1954: ZB = zb_or(n1952, n1953);
    let n1955: ZB = zb_and(n1248, n1954);
    let n1956: ZB = zb_and(n1249, n1954);
    let n1957: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1933);
    let n1958: ZB = zb_or(n1955, n1956);
    let n1959: ZN = zsel_n(n1673, n1933, n1957);
    let n1960: ZB = zb_or(n1950, n1958);
    let n1961: ZN = zsel_n(n1621, zn_splat(P8::from_raw(-231700i32)), n1531);
    let n1962: ZN = zsel_n(n1621, zn_splat(P8::from_raw(-231700i32)), n1532);
    let n1963: ZN = zsel_n(n1065, n1078, n1961);
    let n1964: ZN = zsel_n(n1065, n1088, n1962);
    let n1965: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1963);
    let n1966: ZN = zsel_n(n1673, n1963, n1965);
    let n1967: ZN = zsel_n(n1621, zn_splat(P8::from_raw(231700i32)), n1589);
    let n1968: ZN = zsel_n(n1621, zn_splat(P8::from_raw(-231700i32)), n1590);
    let n1969: ZN = zsel_n(n1065, n1078, n1967);
    let n1970: ZN = zsel_n(n1065, n1088, n1968);
    let n1971: ZN = zsel_n(n1248, zn_splat(P8::from_raw(0i32)), n1969);
    let n1972: ZN = zsel_n(n1673, n1969, n1971);
    let n1973: ZN = zsel_n(n1621, zn_splat(P8::from_raw(327680i32)), n1473);
    let n1974: ZN = zsel_n(n1065, n1088, n1973);
    let n1975: ZN = zsel_n(n1621, zn_splat(P8::from_raw(231700i32)), n1532);
    let n1976: ZN = zsel_n(n1065, n1088, n1975);
    let n1977: ZN = zsel_n(n1621, zn_splat(P8::from_raw(231700i32)), n1590);
    let n1978: ZN = zsel_n(n1065, n1088, n1977);
    let n1982: ZB = zb_and(n417, n418);
    let n1983: ZB = zb_and(n433, n434);
    let n1984: ZB = zb_and(n449, n450);
    let n1985: ZB = zb_and(n473, n474);
    let n1986: ZB = zb_or(n1984, n1985);
    let n1987: ZB = zb_or(n1983, n1986);
    let n1988: ZB = zb_or(n1982, n1987);
    let n1989: ZB = zb_and(n500, n501);
    let n1990: ZB = zb_and(n513, n514);
    let n1991: ZB = zb_and(n526, n527);
    let n1992: ZB = zb_and(n542, n543);
    let n1993: ZB = zb_or(n1991, n1992);
    let n1994: ZB = zb_or(n1990, n1993);
    let n1995: ZB = zb_or(n1989, n1994);
    let n1996: ZB = zb_and(n569, n570);
    let n1997: ZB = zb_and(n582, n583);
    let n1998: ZB = zb_and(n595, n596);
    let n1999: ZB = zb_and(n611, n612);
    let n2000: ZB = zb_or(n1998, n1999);
    let n2001: ZB = zb_or(n1997, n2000);
    let n2002: ZB = zb_or(n1996, n2001);
    let n2003: ZB = zb_or(n1995, n2002);
    let n2004: ZB = zb_or(n1988, n2003);
    let n2005: ZB = zb_and(n645, n646);
    let n2006: ZB = zb_and(n658, n659);
    let n2007: ZB = zb_and(n671, n672);
    let n2008: ZB = zb_and(n691, n692);
    let n2009: ZB = zb_or(n2007, n2008);
    let n2010: ZB = zb_or(n2006, n2009);
    let n2011: ZB = zb_or(n2005, n2010);
    let n2012: ZB = zb_and(n710, n711);
    let n2013: ZB = zb_and(n723, n724);
    let n2014: ZB = zb_and(n736, n737);
    let n2015: ZB = zb_and(n752, n753);
    let n2016: ZB = zb_or(n2014, n2015);
    let n2017: ZB = zb_or(n2013, n2016);
    let n2018: ZB = zb_or(n2012, n2017);
    let n2019: ZB = zb_and(n771, n772);
    let n2020: ZB = zb_and(n784, n785);
    let n2021: ZB = zb_and(n797, n798);
    let n2022: ZB = zb_and(n813, n814);
    let n2023: ZB = zb_or(n2021, n2022);
    let n2024: ZB = zb_or(n2020, n2023);
    let n2025: ZB = zb_or(n2019, n2024);
    let n2026: ZB = zb_or(n2018, n2025);
    let n2027: ZB = zb_or(n2011, n2026);
    let n2028: ZB = zb_and(n845, n846);
    let n2029: ZB = zb_and(n858, n859);
    let n2030: ZB = zb_and(n871, n872);
    let n2031: ZB = zb_and(n891, n892);
    let n2032: ZB = zb_or(n2030, n2031);
    let n2033: ZB = zb_or(n2029, n2032);
    let n2034: ZB = zb_or(n2028, n2033);
    let n2035: ZB = zb_and(n910, n911);
    let n2036: ZB = zb_and(n923, n924);
    let n2037: ZB = zb_and(n936, n937);
    let n2038: ZB = zb_and(n952, n953);
    let n2039: ZB = zb_or(n2037, n2038);
    let n2040: ZB = zb_or(n2036, n2039);
    let n2041: ZB = zb_or(n2035, n2040);
    let n2042: ZB = zb_and(n971, n972);
    let n2043: ZB = zb_and(n984, n985);
    let n2044: ZB = zb_and(n997, n998);
    let n2045: ZB = zb_and(n1013, n1014);
    let n2046: ZB = zb_or(n2044, n2045);
    let n2047: ZB = zb_or(n2043, n2046);
    let n2048: ZB = zb_or(n2042, n2047);
    let n2049: ZB = zb_or(n2041, n2048);
    let n2050: ZB = zb_or(n2034, n2049);
    let n2051: ZB = zb_or(n2027, n2050);
    let n2052: ZB = zsel_b(n2027, n623, n823);
    let n2053: ZB = zb_or(n2004, n2051);
    let n2054: ZB = zsel_b(n2004, n367, n2052);
    let n2055: ZB = zn_gt(n362, zn_splat(P8::from_raw(8388608i32)));
    let n2056: ZB = zb_and(n2053, n2055);
    let n2057: ZB = zb_and(n1033, n2053);
    let n2058: ZB = zb_or(n2056, n2057);
    let n2059: ZB = zb_and(n1031, n2055);
    let n2060: ZB = zb_or(n2058, n2059);
    let n2061: ZB = zsel_b(n2058, n2054, n1032);
    let n2062: ZB = zb_and(n1038, n2060);
    let n2063: ZB = zb_and(n1037, n2060);
    let n2064: ZB = zb_or(n2062, n2063);
    let n2065: ZB = zb_and(n1038, n2064);
    let n2066: ZB = zb_and(n1037, n2064);
    let n2067: ZB = zb_or(n2065, n2066);
    let n2068: ZB = zb_and(n1037, n2067);
    let n2069: ZB = zb_and(n1038, n2067);
    let n2070: ZB = zb_and(n1048, n2068);
    let n2071: ZB = zb_and(n1049, n2068);
    let n2072: ZB = zb_or(n2070, n2071);
    let n2073: ZB = zb_and(n1054, n2069);
    let n2074: ZB = zb_and(n1055, n2069);
    let n2075: ZB = zb_or(n2073, n2074);
    let n2076: ZB = zb_or(n2072, n2075);
    let n2077: ZB = zb_and(n1065, n2076);
    let n2078: ZB = zb_and(n1066, n2076);
    let n2079: ZB = zb_and(n1070, n2077);
    let n2080: ZB = zb_and(n1071, n2077);
    let n2081: ZB = zb_or(n2079, n2080);
    let n2082: ZB = zb_and(n1080, n2081);
    let n2083: ZB = zb_and(n1081, n2081);
    let n2084: ZB = zb_or(n2082, n2083);
    let n2085: ZB = zb_and(n1038, n2078);
    let n2086: ZB = zb_and(n1037, n2078);
    let n2087: ZB = zb_or(n2085, n2086);
    let n2088: ZB = zb_and(n1095, n2087);
    let n2089: ZB = zb_and(n1096, n2087);
    let n2090: ZB = zb_and(n1099, n2088);
    let n2091: ZB = zb_and(n448, n2088);
    let n2092: ZB = zb_and(n1102, n2091);
    let n2093: ZB = zb_and(n472, n2091);
    let n2094: ZB = zb_and(n1105, n2090);
    let n2095: ZB = zb_and(n1106, n2090);
    let n2096: ZB = zb_and(n1113, n2092);
    let n2097: ZB = zb_and(n1114, n2092);
    let n2098: ZB = zb_and(n448, n2093);
    let n2099: ZB = zb_or(n2096, n2097);
    let n2100: ZB = zb_or(n2094, n2095);
    let n2101: ZB = zb_or(n2098, n2099);
    let n2102: ZB = zb_or(n2100, n2101);
    let n2103: ZB = zb_and(n1099, n2089);
    let n2104: ZB = zb_and(n448, n2089);
    let n2105: ZB = zb_or(n2103, n2104);
    let n2106: ZB = zb_or(n2102, n2105);
    let n2107: ZB = zb_and(n1142, n2106);
    let n2108: ZB = zb_and(n1141, n2106);
    let n2109: ZB = zb_or(n2107, n2108);
    let n2110: ZB = zb_and(n1149, n2109);
    let n2111: ZB = zb_and(n1150, n2109);
    let n2112: ZB = zb_or(n2110, n2111);
    let n2113: ZB = zb_and(n1038, n2112);
    let n2114: ZB = zb_and(n1037, n2112);
    let n2115: ZB = zb_and(n1158, n2113);
    let n2116: ZB = zb_and(n1159, n2113);
    let n2117: ZB = zb_or(n2115, n2116);
    let n2118: ZB = zb_or(n2114, n2117);
    let n2119: ZB = zb_and(n1189, n2118);
    let n2120: ZB = zb_and(n1190, n2118);
    let n2121: ZB = zb_or(n2119, n2120);
    let n2122: ZB = zb_or(n2084, n2121);
    let n2123: ZB = zb_and(n1210, n2122);
    let n2124: ZB = zb_and(n1211, n2122);
    let n2125: ZB = zb_or(n2123, n2124);
    let n2127: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c267);
    let n2128: ZN = zn_div(n2127, zn_splat(P8::from_raw(2621440i32)));
    let n2129: ZN = zn_sin(n2128);
    let n2130: ZN = zn_mul(n2129, zn_splat(P8::from_raw(163840i32)));
    let n2131: ZN = zn_add(zn_splat(P8::from_raw(2359296i32)), n2130);
    let n2133: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n2134: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2133);
    let n2135: ZN = zsel_n(n2055, n2134, n2133);
    let n2136: ZN = zsel_n(n2058, n2135, n2133);
    let n2138: ZB = zb_and(n1113, n2089);
    let n2139: ZB = zb_and(n1114, n2089);
    let n2140: ZB = zb_or(n2138, n2139);
    let n2141: ZB = zb_or(n2102, n2140);
    let n2142: ZB = zb_and(n1294, n2141);
    let n2143: ZB = zb_and(n1293, n2141);
    let n2144: ZB = zb_or(n2142, n2143);
    let n2145: ZB = zb_and(n1149, n2144);
    let n2146: ZB = zb_and(n1150, n2144);
    let n2147: ZB = zb_or(n2145, n2146);
    let n2148: ZB = zb_and(n1305, n2147);
    let n2149: ZB = zb_and(n1304, n2147);
    let n2150: ZB = zb_or(n2148, n2149);
    let n2151: ZB = zb_and(n1305, n2150);
    let n2152: ZB = zb_and(n1304, n2150);
    let n2153: ZB = zb_or(n2151, n2152);
    let n2154: ZB = zb_and(n1304, n2153);
    let n2155: ZB = zb_and(n1305, n2153);
    let n2156: ZB = zb_or(n2154, n2155);
    let n2157: ZB = zb_and(n1304, n2156);
    let n2158: ZB = zb_and(n1305, n2156);
    let n2159: ZB = zb_or(n2157, n2158);
    let n2160: ZB = zb_and(n1038, n2159);
    let n2161: ZB = zb_and(n1037, n2159);
    let n2162: ZB = zb_and(n1321, n2160);
    let n2163: ZB = zb_and(n1322, n2160);
    let n2164: ZB = zb_or(n2162, n2163);
    let n2165: ZB = zb_or(n2161, n2164);
    let n2166: ZB = zb_and(n1189, n2165);
    let n2167: ZB = zb_and(n1190, n2165);
    let n2168: ZB = zb_or(n2166, n2167);
    let n2169: ZB = zb_or(n2084, n2168);
    let n2170: ZB = zb_and(n1210, n2169);
    let n2171: ZB = zb_and(n1211, n2169);
    let n2172: ZB = zb_or(n2170, n2171);
    let n2175: ZB = zb_and(n1105, n2089);
    let n2176: ZB = zb_and(n1106, n2089);
    let n2177: ZB = zb_or(n2175, n2176);
    let n2178: ZB = zb_or(n2102, n2177);
    let n2179: ZB = zb_and(n1374, n2178);
    let n2180: ZB = zb_and(n1373, n2178);
    let n2181: ZB = zb_or(n2179, n2180);
    let n2182: ZB = zb_and(n1149, n2181);
    let n2183: ZB = zb_and(n1150, n2181);
    let n2184: ZB = zb_or(n2182, n2183);
    let n2185: ZB = zb_and(n1385, n2184);
    let n2186: ZB = zb_and(n1384, n2184);
    let n2187: ZB = zb_or(n2185, n2186);
    let n2188: ZB = zb_and(n1385, n2187);
    let n2189: ZB = zb_and(n1384, n2187);
    let n2190: ZB = zb_or(n2188, n2189);
    let n2191: ZB = zb_and(n1384, n2190);
    let n2192: ZB = zb_and(n1385, n2190);
    let n2193: ZB = zb_or(n2191, n2192);
    let n2194: ZB = zb_and(n1384, n2193);
    let n2195: ZB = zb_and(n1385, n2193);
    let n2196: ZB = zb_or(n2194, n2195);
    let n2197: ZB = zb_and(n1038, n2196);
    let n2198: ZB = zb_and(n1037, n2196);
    let n2199: ZB = zb_and(n1401, n2197);
    let n2200: ZB = zb_and(n1402, n2197);
    let n2201: ZB = zb_or(n2199, n2200);
    let n2202: ZB = zb_or(n2198, n2201);
    let n2203: ZB = zb_and(n1189, n2202);
    let n2204: ZB = zb_and(n1190, n2202);
    let n2205: ZB = zb_or(n2203, n2204);
    let n2206: ZB = zb_or(n2084, n2205);
    let n2207: ZB = zb_and(n1210, n2206);
    let n2208: ZB = zb_and(n1211, n2206);
    let n2209: ZB = zb_or(n2207, n2208);
    let n2212: ZB = zb_and(n1045, n2118);
    let n2213: ZB = zb_and(r_c249, n2118);
    let n2214: ZB = zb_and(n1170, n2212);
    let n2215: ZB = zb_and(n1171, n2212);
    let n2216: ZB = zb_and(n1174, n2215);
    let n2217: ZB = zb_and(n1173, n2215);
    let n2218: ZB = zb_or(n2216, n2217);
    let n2219: ZB = zb_and(n1174, n2218);
    let n2220: ZB = zb_and(n1173, n2218);
    let n2221: ZB = zb_or(n2219, n2220);
    let n2222: ZB = zb_and(n1173, n2221);
    let n2223: ZB = zb_and(n1174, n2221);
    let n2224: ZB = zb_and(n1177, n2223);
    let n2225: ZB = zb_and(n1176, n2223);
    let n2226: ZB = zb_or(n2224, n2225);
    let n2227: ZB = zb_and(n1177, n2226);
    let n2228: ZB = zb_and(n1176, n2226);
    let n2229: ZB = zb_or(n2227, n2228);
    let n2230: ZB = zb_and(n1176, n2229);
    let n2231: ZB = zb_and(n1177, n2229);
    let n2232: ZB = zb_or(n2230, n2231);
    let n2233: ZB = zb_or(n2222, n2232);
    let n2234: ZB = zb_and(n1181, n2233);
    let n2235: ZB = zb_and(n1180, n2233);
    let n2236: ZB = zb_or(n2234, n2235);
    let n2237: ZB = zb_or(n2214, n2236);
    let n2238: ZB = zb_or(n2213, n2237);
    let n2239: ZB = zb_and(n1189, n2238);
    let n2240: ZB = zb_and(n1190, n2238);
    let n2241: ZB = zb_or(n2239, n2240);
    let n2242: ZB = zb_or(n2084, n2241);
    let n2243: ZB = zb_and(n1210, n2242);
    let n2244: ZB = zb_and(n1211, n2242);
    let n2245: ZB = zb_or(n2243, n2244);
    let n2248: ZB = zb_and(n1045, n2165);
    let n2249: ZB = zb_and(r_c249, n2165);
    let n2250: ZB = zb_and(n1170, n2248);
    let n2251: ZB = zb_and(n1171, n2248);
    let n2252: ZB = zb_and(n1174, n2251);
    let n2253: ZB = zb_and(n1173, n2251);
    let n2254: ZB = zb_or(n2252, n2253);
    let n2255: ZB = zb_and(n1174, n2254);
    let n2256: ZB = zb_and(n1173, n2254);
    let n2257: ZB = zb_or(n2255, n2256);
    let n2258: ZB = zb_and(n1173, n2257);
    let n2259: ZB = zb_and(n1174, n2257);
    let n2260: ZB = zb_and(n1177, n2259);
    let n2261: ZB = zb_and(n1176, n2259);
    let n2262: ZB = zb_or(n2260, n2261);
    let n2263: ZB = zb_and(n1177, n2262);
    let n2264: ZB = zb_and(n1176, n2262);
    let n2265: ZB = zb_or(n2263, n2264);
    let n2266: ZB = zb_and(n1176, n2265);
    let n2267: ZB = zb_and(n1177, n2265);
    let n2268: ZB = zb_or(n2266, n2267);
    let n2269: ZB = zb_or(n2258, n2268);
    let n2270: ZB = zb_and(n1181, n2269);
    let n2271: ZB = zb_and(n1180, n2269);
    let n2272: ZB = zb_or(n2270, n2271);
    let n2273: ZB = zb_or(n2250, n2272);
    let n2274: ZB = zb_or(n2249, n2273);
    let n2275: ZB = zb_and(n1189, n2274);
    let n2276: ZB = zb_and(n1190, n2274);
    let n2277: ZB = zb_or(n2275, n2276);
    let n2278: ZB = zb_or(n2084, n2277);
    let n2279: ZB = zb_and(n1210, n2278);
    let n2280: ZB = zb_and(n1211, n2278);
    let n2281: ZB = zb_or(n2279, n2280);
    let n2284: ZB = zb_and(n1045, n2202);
    let n2285: ZB = zb_and(r_c249, n2202);
    let n2286: ZB = zb_and(n1170, n2284);
    let n2287: ZB = zb_and(n1171, n2284);
    let n2288: ZB = zb_and(n1174, n2287);
    let n2289: ZB = zb_and(n1173, n2287);
    let n2290: ZB = zb_or(n2288, n2289);
    let n2291: ZB = zb_and(n1174, n2290);
    let n2292: ZB = zb_and(n1173, n2290);
    let n2293: ZB = zb_or(n2291, n2292);
    let n2294: ZB = zb_and(n1173, n2293);
    let n2295: ZB = zb_and(n1174, n2293);
    let n2296: ZB = zb_and(n1177, n2295);
    let n2297: ZB = zb_and(n1176, n2295);
    let n2298: ZB = zb_or(n2296, n2297);
    let n2299: ZB = zb_and(n1177, n2298);
    let n2300: ZB = zb_and(n1176, n2298);
    let n2301: ZB = zb_or(n2299, n2300);
    let n2302: ZB = zb_and(n1176, n2301);
    let n2303: ZB = zb_and(n1177, n2301);
    let n2304: ZB = zb_or(n2302, n2303);
    let n2305: ZB = zb_or(n2294, n2304);
    let n2306: ZB = zb_and(n1181, n2305);
    let n2307: ZB = zb_and(n1180, n2305);
    let n2308: ZB = zb_or(n2306, n2307);
    let n2309: ZB = zb_or(n2286, n2308);
    let n2310: ZB = zb_or(n2285, n2309);
    let n2311: ZB = zb_and(n1189, n2310);
    let n2312: ZB = zb_and(n1190, n2310);
    let n2313: ZB = zb_or(n2311, n2312);
    let n2314: ZB = zb_or(n2084, n2313);
    let n2315: ZB = zb_and(n1210, n2314);
    let n2316: ZB = zb_and(n1211, n2314);
    let n2317: ZB = zb_or(n2315, n2316);
    let n2320: ZB = zb_and(n1621, n2121);
    let n2321: ZB = zb_and(n1622, n2121);
    let n2322: ZB = zb_and(n1146, n2320);
    let n2323: ZB = zb_and(n1194, n2320);
    let n2324: ZB = zb_or(n2322, n2323);
    let n2325: ZB = zb_and(n1196, n2324);
    let n2326: ZB = zb_and(n1197, n2324);
    let n2327: ZB = zb_and(n1198, n2326);
    let n2328: ZB = zb_and(n1199, n2326);
    let n2329: ZB = zb_or(n2327, n2328);
    let n2330: ZB = zb_or(n2325, n2329);
    let n2331: ZB = zb_and(n1203, n2330);
    let n2332: ZB = zb_and(n1202, n2330);
    let n2333: ZB = zb_or(n2331, n2332);
    let n2334: ZB = zb_or(n2321, n2333);
    let n2335: ZB = zb_or(n2084, n2334);
    let n2336: ZB = zb_and(n1210, n2335);
    let n2337: ZB = zb_and(n1211, n2335);
    let n2338: ZB = zb_or(n2336, n2337);
    let n2339: ZB = zb_and(n1211, n2338);
    let n2340: ZB = zb_and(n1673, n2339);
    let n2341: ZB = zb_and(n1674, n2339);
    let n2342: ZB = zb_or(n2340, n2341);
    let n2343: ZB = zb_and(n1621, n2168);
    let n2344: ZB = zb_and(n1622, n2168);
    let n2345: ZB = zb_or(n2343, n2344);
    let n2346: ZB = zb_or(n2084, n2345);
    let n2347: ZB = zb_and(n1210, n2346);
    let n2348: ZB = zb_and(n1211, n2346);
    let n2349: ZB = zb_or(n2347, n2348);
    let n2350: ZB = zb_and(n1211, n2349);
    let n2351: ZB = zb_and(n1673, n2350);
    let n2352: ZB = zb_and(n1674, n2350);
    let n2353: ZB = zb_or(n2351, n2352);
    let n2354: ZB = zb_and(n1621, n2205);
    let n2355: ZB = zb_and(n1622, n2205);
    let n2356: ZB = zb_or(n2354, n2355);
    let n2357: ZB = zb_or(n2084, n2356);
    let n2358: ZB = zb_and(n1210, n2357);
    let n2359: ZB = zb_and(n1211, n2357);
    let n2360: ZB = zb_or(n2358, n2359);
    let n2361: ZB = zb_and(n1211, n2360);
    let n2362: ZB = zb_and(n1673, n2361);
    let n2363: ZB = zb_and(n1674, n2361);
    let n2364: ZB = zb_or(n2362, n2363);
    let n2365: ZB = zb_or(n2320, n2321);
    let n2366: ZB = zb_or(n2084, n2365);
    let n2367: ZB = zb_and(n1210, n2366);
    let n2368: ZB = zb_and(n1211, n2366);
    let n2369: ZB = zb_or(n2367, n2368);
    let n2370: ZB = zb_and(n1211, n2369);
    let n2371: ZB = zb_and(n1673, n2370);
    let n2372: ZB = zb_and(n1674, n2370);
    let n2373: ZB = zb_or(n2371, n2372);
    let n2374: ZB = zb_and(n1621, n2241);
    let n2375: ZB = zb_and(n1622, n2241);
    let n2376: ZB = zb_and(n1146, n2374);
    let n2377: ZB = zb_and(n1194, n2374);
    let n2378: ZB = zb_or(n2376, n2377);
    let n2379: ZB = zb_and(n1196, n2378);
    let n2380: ZB = zb_and(n1197, n2378);
    let n2381: ZB = zb_and(n1198, n2380);
    let n2382: ZB = zb_and(n1199, n2380);
    let n2383: ZB = zb_or(n2381, n2382);
    let n2384: ZB = zb_or(n2379, n2383);
    let n2385: ZB = zb_and(n1203, n2384);
    let n2386: ZB = zb_and(n1202, n2384);
    let n2387: ZB = zb_or(n2385, n2386);
    let n2388: ZB = zb_or(n2375, n2387);
    let n2389: ZB = zb_or(n2084, n2388);
    let n2390: ZB = zb_and(n1210, n2389);
    let n2391: ZB = zb_and(n1211, n2389);
    let n2392: ZB = zb_or(n2390, n2391);
    let n2393: ZB = zb_and(n1211, n2392);
    let n2394: ZB = zb_and(n1673, n2393);
    let n2395: ZB = zb_and(n1674, n2393);
    let n2396: ZB = zb_or(n2394, n2395);
    let n2397: ZB = zb_and(n1621, n2277);
    let n2398: ZB = zb_and(n1622, n2277);
    let n2399: ZB = zb_or(n2397, n2398);
    let n2400: ZB = zb_or(n2084, n2399);
    let n2401: ZB = zb_and(n1210, n2400);
    let n2402: ZB = zb_and(n1211, n2400);
    let n2403: ZB = zb_or(n2401, n2402);
    let n2404: ZB = zb_and(n1211, n2403);
    let n2405: ZB = zb_and(n1673, n2404);
    let n2406: ZB = zb_and(n1674, n2404);
    let n2407: ZB = zb_or(n2405, n2406);
    let n2408: ZB = zb_and(n1621, n2313);
    let n2409: ZB = zb_and(n1622, n2313);
    let n2410: ZB = zb_or(n2408, n2409);
    let n2411: ZB = zb_or(n2084, n2410);
    let n2412: ZB = zb_and(n1210, n2411);
    let n2413: ZB = zb_and(n1211, n2411);
    let n2414: ZB = zb_or(n2412, n2413);
    let n2415: ZB = zb_and(n1211, n2414);
    let n2416: ZB = zb_and(n1673, n2415);
    let n2417: ZB = zb_and(n1674, n2415);
    let n2418: ZB = zb_or(n2416, n2417);
    let n2419: ZB = zb_or(n2374, n2375);
    let n2420: ZB = zb_or(n2084, n2419);
    let n2421: ZB = zb_and(n1210, n2420);
    let n2422: ZB = zb_and(n1211, n2420);
    let n2423: ZB = zb_or(n2421, n2422);
    let n2424: ZB = zb_and(n1211, n2423);
    let n2425: ZB = zb_and(n1673, n2424);
    let n2426: ZB = zb_and(n1674, n2424);
    let n2427: ZB = zb_or(n2425, n2426);
    let n2430: ZB = zb_and(n1210, n1214);
    let n2431: ZB = zb_and(n1210, n2125);
    let n2432: ZB = zb_not(n2430);
    let n2433: ZB = zb_or(n2430, n2431);
    let n2434: ZB = zsel_b(n2430, n1032, n2061);
    let n2436: ZN = zsel_n(n2430, r_c87, n2136);
    let n2437: ZN = zsel_n(n2430, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2439: ZB = zb_and(n1210, n1344);
    let n2440: ZB = zb_and(n1210, n2172);
    let n2441: ZB = zb_not(n2439);
    let n2442: ZB = zb_or(n2439, n2440);
    let n2443: ZB = zsel_b(n2439, n1032, n2061);
    let n2445: ZN = zsel_n(n2439, r_c87, n2136);
    let n2446: ZN = zsel_n(n2439, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2448: ZB = zb_and(n1210, n1424);
    let n2449: ZB = zb_and(n1210, n2209);
    let n2450: ZB = zb_not(n2448);
    let n2451: ZB = zb_or(n2448, n2449);
    let n2452: ZB = zsel_b(n2448, n1032, n2061);
    let n2454: ZN = zsel_n(n2448, r_c87, n2136);
    let n2455: ZN = zsel_n(n2448, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2457: ZB = zb_and(n1210, n1484);
    let n2458: ZB = zb_and(n1210, n2245);
    let n2459: ZB = zb_not(n2457);
    let n2460: ZB = zb_or(n2457, n2458);
    let n2461: ZB = zsel_b(n2457, n1032, n2061);
    let n2463: ZN = zsel_n(n2457, r_c87, n2136);
    let n2464: ZN = zsel_n(n2457, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2466: ZB = zb_and(n1210, n1542);
    let n2467: ZB = zb_and(n1210, n2281);
    let n2468: ZB = zb_not(n2466);
    let n2469: ZB = zb_or(n2466, n2467);
    let n2470: ZB = zsel_b(n2466, n1032, n2061);
    let n2472: ZN = zsel_n(n2466, r_c87, n2136);
    let n2473: ZN = zsel_n(n2466, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2475: ZB = zb_and(n1210, n1600);
    let n2476: ZB = zb_and(n1210, n2317);
    let n2477: ZB = zb_not(n2475);
    let n2478: ZB = zb_or(n2475, n2476);
    let n2479: ZB = zsel_b(n2475, n1032, n2061);
    let n2481: ZN = zsel_n(n2475, r_c87, n2136);
    let n2482: ZN = zsel_n(n2475, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2484: ZB = zb_and(n1210, n1661);
    let n2485: ZB = zb_and(n1210, n2338);
    let n2486: ZB = zb_not(n2484);
    let n2487: ZB = zb_or(n2484, n2485);
    let n2488: ZB = zsel_b(n2484, n1032, n2061);
    let n2489: ZB = zb_and(n1673, n2487);
    let n2490: ZB = zb_and(n1674, n2487);
    let n2491: ZB = zb_or(n2489, n2490);
    let n2492: ZN = zsel_n(n2484, r_c87, n2136);
    let n2493: ZN = zsel_n(n2484, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2495: ZB = zb_and(n1210, n1701);
    let n2496: ZB = zb_and(n1210, n2349);
    let n2497: ZB = zb_not(n2495);
    let n2498: ZB = zb_or(n2495, n2496);
    let n2499: ZB = zsel_b(n2495, n1032, n2061);
    let n2500: ZB = zb_and(n1673, n2498);
    let n2501: ZB = zb_and(n1674, n2498);
    let n2502: ZB = zb_or(n2500, n2501);
    let n2503: ZN = zsel_n(n2495, r_c87, n2136);
    let n2504: ZN = zsel_n(n2495, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2506: ZB = zb_and(n1210, n1736);
    let n2507: ZB = zb_and(n1210, n2360);
    let n2508: ZB = zb_not(n2506);
    let n2509: ZB = zb_or(n2506, n2507);
    let n2510: ZB = zsel_b(n2506, n1032, n2061);
    let n2511: ZB = zb_and(n1673, n2509);
    let n2512: ZB = zb_and(n1674, n2509);
    let n2513: ZB = zb_or(n2511, n2512);
    let n2514: ZN = zsel_n(n2506, r_c87, n2136);
    let n2515: ZN = zsel_n(n2506, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2517: ZB = zb_and(n1210, n1776);
    let n2518: ZB = zb_and(n1210, n2369);
    let n2519: ZB = zb_not(n2517);
    let n2520: ZB = zb_or(n2517, n2518);
    let n2521: ZB = zsel_b(n2517, n1032, n2061);
    let n2522: ZB = zb_and(n1673, n2520);
    let n2523: ZB = zb_and(n1674, n2520);
    let n2524: ZB = zb_or(n2522, n2523);
    let n2525: ZN = zsel_n(n2517, r_c87, n2136);
    let n2526: ZN = zsel_n(n2517, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2528: ZB = zb_and(n1210, n1841);
    let n2529: ZB = zb_and(n1210, n2392);
    let n2530: ZB = zb_not(n2528);
    let n2531: ZB = zb_or(n2528, n2529);
    let n2532: ZB = zsel_b(n2528, n1032, n2061);
    let n2533: ZB = zb_and(n1673, n2531);
    let n2534: ZB = zb_and(n1674, n2531);
    let n2535: ZB = zb_or(n2533, n2534);
    let n2536: ZN = zsel_n(n2528, r_c87, n2136);
    let n2537: ZN = zsel_n(n2528, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2539: ZB = zb_and(n1210, n1874);
    let n2540: ZB = zb_and(n1210, n2403);
    let n2541: ZB = zb_not(n2539);
    let n2542: ZB = zb_or(n2539, n2540);
    let n2543: ZB = zsel_b(n2539, n1032, n2061);
    let n2544: ZB = zb_and(n1673, n2542);
    let n2545: ZB = zb_and(n1674, n2542);
    let n2546: ZB = zb_or(n2544, n2545);
    let n2547: ZN = zsel_n(n2539, r_c87, n2136);
    let n2548: ZN = zsel_n(n2539, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2550: ZB = zb_and(n1210, n1907);
    let n2551: ZB = zb_and(n1210, n2414);
    let n2552: ZB = zb_not(n2550);
    let n2553: ZB = zb_or(n2550, n2551);
    let n2554: ZB = zsel_b(n2550, n1032, n2061);
    let n2555: ZB = zb_and(n1673, n2553);
    let n2556: ZB = zb_and(n1674, n2553);
    let n2557: ZB = zb_or(n2555, n2556);
    let n2558: ZN = zsel_n(n2550, r_c87, n2136);
    let n2559: ZN = zsel_n(n2550, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2561: ZB = zb_and(n1210, n1938);
    let n2562: ZB = zb_and(n1210, n2423);
    let n2563: ZB = zb_not(n2561);
    let n2564: ZB = zb_or(n2561, n2562);
    let n2565: ZB = zsel_b(n2561, n1032, n2061);
    let n2566: ZB = zb_and(n1673, n2564);
    let n2567: ZB = zb_and(n1674, n2564);
    let n2568: ZB = zb_or(n2566, n2567);
    let n2569: ZN = zsel_n(n2561, r_c87, n2136);
    let n2570: ZN = zsel_n(n2561, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2572: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n2573: ZN = zn_sub(n1061, zn_splat(P8::from_raw(65536i32)));
    let n2574: ZB = zb_not(n1238);
    let n2575: ZB = zb_and(n1237, n2574);
    let n2576: ZN = zsel_n(n1240, n2572, r_c20);
    let n2577: ZN = zsel_n(n1240, r_c236, n1064);
    let n2578: ZN = zsel_n(n1240, r_c238, n1205);
    let n2579: ZN = zsel_n(n1240, r_c239, n1061);
    let n2580: ZN = zsel_n(n1240, r_c241, n1062);
    let n2581: ZB = zb_and(r_c248, n1240);
    let n2582: ZB = zb_and(r_c249, n1240);
    let n2583: ZN = zsel_n(n1240, r_c255, n361);
    let n2584: ZN = zsel_n(n1240, r_c256, n362);
    let n2585: ZN = zsel_n(n1240, r_c267, n2127);
    let n2586: ZN = zsel_n(n1240, r_c275, n2131);
    let n2587: ZB = zsel_b(n1240, r_c304, n1206);
    let n2588: ZN = zsel_n(n1240, r_c310, n363);
    let n2589: ZN = zsel_n(n1240, r_c311, n364);
    let n2590: ZN = zsel_n(n1240, r_c312, n1207);
    let n2591: ZN = zsel_n(n1240, r_c313, n1208);
    let n2592: ZB = zb_or(n1240, n2575);
    let n2593: ZB = zb_or(n1032, n1240);
    let n2594: ZB = zn_gt(n2576, zn_splat(P8::from_raw(0i32)));
    let n2595: ZB = zn_le(n2576, zn_splat(P8::from_raw(0i32)));
    let n2596: ZB = zb_and(n2592, n2594);
    let n2597: ZB = zb_and(n2592, n2595);
    let n2598: ZB = zn_lt(n2583, zn_splat(P8::from_raw(-65536i32)));
    let n2599: ZB = zn_ge(n2583, zn_splat(P8::from_raw(-65536i32)));
    let n2600: ZB = zb_and(n2597, n2599);
    let n2601: ZB = zb_and(n2597, n2598);
    let n2602: ZB = zn_gt(n2583, zn_splat(P8::from_raw(7929856i32)));
    let n2603: ZB = zb_or(n2600, n2601);
    let n2604: ZB = zb_or(n2598, n2602);
    let n2605: ZB = zb_not(n2604);
    let n2606: ZB = zb_and(n2603, n2604);
    let n2607: ZB = zb_and(n2603, n2605);
    let n2608: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n2583);
    let n2609: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n2608);
    let n2610: ZN = zsel_n(n2604, n2609, n2583);
    let n2611: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2590);
    let n2612: ZB = zb_or(n2606, n2607);
    let n2613: ZN = zsel_n(n2594, n2583, n2610);
    let n2614: ZN = zsel_n(n2594, n2590, n2611);
    let n2615: ZB = zb_or(n2596, n2612);
    let n2617: ZB = zb_and(n1354, n2574);
    let n2618: ZB = zsel_b(n1240, r_c304, n1338);
    let n2619: ZN = zsel_n(n1240, r_c312, n1339);
    let n2620: ZN = zsel_n(n1240, r_c313, n1340);
    let n2621: ZB = zb_or(n1240, n2617);
    let n2622: ZB = zb_and(n2594, n2621);
    let n2623: ZB = zb_and(n2595, n2621);
    let n2624: ZB = zb_and(n2599, n2623);
    let n2625: ZB = zb_and(n2598, n2623);
    let n2626: ZB = zb_or(n2624, n2625);
    let n2627: ZB = zb_and(n2604, n2626);
    let n2628: ZB = zb_and(n2605, n2626);
    let n2629: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2619);
    let n2630: ZB = zb_or(n2627, n2628);
    let n2631: ZN = zsel_n(n2594, n2619, n2629);
    let n2632: ZB = zb_or(n2622, n2630);
    let n2633: ZB = zb_and(n1434, n2574);
    let n2634: ZB = zsel_b(n1240, r_c304, n1418);
    let n2635: ZN = zsel_n(n1240, r_c312, n1419);
    let n2636: ZN = zsel_n(n1240, r_c313, n1420);
    let n2637: ZB = zb_or(n1240, n2633);
    let n2638: ZB = zb_and(n2594, n2637);
    let n2639: ZB = zb_and(n2595, n2637);
    let n2640: ZB = zb_and(n2599, n2639);
    let n2641: ZB = zb_and(n2598, n2639);
    let n2642: ZB = zb_or(n2640, n2641);
    let n2643: ZB = zb_and(n2604, n2642);
    let n2644: ZB = zb_and(n2605, n2642);
    let n2645: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2635);
    let n2646: ZB = zb_or(n2643, n2644);
    let n2647: ZN = zsel_n(n2594, n2635, n2645);
    let n2648: ZB = zb_or(n2638, n2646);
    let n2649: ZB = zb_and(n1494, n2574);
    let n2650: ZN = zsel_n(n1240, r_c241, n1478);
    let n2651: ZB = zb_or(r_c249, n99);
    let n2652: ZN = zsel_n(n1240, r_c312, n1479);
    let n2653: ZN = zsel_n(n1240, r_c313, n1480);
    let n2654: ZB = zb_or(n1240, n2649);
    let n2655: ZB = zb_and(n2594, n2654);
    let n2656: ZB = zb_and(n2595, n2654);
    let n2657: ZB = zb_and(n2599, n2656);
    let n2658: ZB = zb_and(n2598, n2656);
    let n2659: ZB = zb_or(n2657, n2658);
    let n2660: ZB = zb_and(n2604, n2659);
    let n2661: ZB = zb_and(n2605, n2659);
    let n2662: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2652);
    let n2663: ZB = zb_or(n2660, n2661);
    let n2664: ZN = zsel_n(n2594, n2652, n2662);
    let n2665: ZB = zb_or(n2655, n2663);
    let n2666: ZB = zb_and(n1552, n2574);
    let n2667: ZN = zsel_n(n1240, r_c312, n1537);
    let n2668: ZN = zsel_n(n1240, r_c313, n1538);
    let n2669: ZB = zb_or(n1240, n2666);
    let n2670: ZB = zb_and(n2594, n2669);
    let n2671: ZB = zb_and(n2595, n2669);
    let n2672: ZB = zb_and(n2599, n2671);
    let n2673: ZB = zb_and(n2598, n2671);
    let n2674: ZB = zb_or(n2672, n2673);
    let n2675: ZB = zb_and(n2604, n2674);
    let n2676: ZB = zb_and(n2605, n2674);
    let n2677: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2667);
    let n2678: ZB = zb_or(n2675, n2676);
    let n2679: ZN = zsel_n(n2594, n2667, n2677);
    let n2680: ZB = zb_or(n2670, n2678);
    let n2681: ZB = zb_and(n1610, n2574);
    let n2682: ZN = zsel_n(n1240, r_c312, n1595);
    let n2683: ZN = zsel_n(n1240, r_c313, n1596);
    let n2684: ZB = zb_or(n1240, n2681);
    let n2685: ZB = zb_and(n2594, n2684);
    let n2686: ZB = zb_and(n2595, n2684);
    let n2687: ZB = zb_and(n2599, n2686);
    let n2688: ZB = zb_and(n2598, n2686);
    let n2689: ZB = zb_or(n2687, n2688);
    let n2690: ZB = zb_and(n2604, n2689);
    let n2691: ZB = zb_and(n2605, n2689);
    let n2692: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2682);
    let n2693: ZB = zb_or(n2690, n2691);
    let n2694: ZN = zsel_n(n2594, n2682, n2692);
    let n2695: ZB = zb_or(n2685, n2693);
    let n2696: ZN = zsel_n(n1621, n2573, n1061);
    let n2697: ZN = zsel_n(n1065, n1061, n2696);
    let n2698: ZB = zb_and(n1671, n2574);
    let n2699: ZN = zsel_n(n1240, n2572, n1648);
    let n2700: ZB = zsel_b(n1240, r_c41, n1649);
    let n2701: ZN = zsel_n(n1240, r_c236, n1650);
    let n2702: ZN = zsel_n(n1240, r_c238, n1651);
    let n2703: ZN = zsel_n(n1240, r_c239, n2697);
    let n2704: ZB = zb_or(r_c248, n99);
    let n2705: ZN = zsel_n(n1240, r_c300, n1652);
    let n2706: ZN = zsel_n(n1240, r_c301, n1653);
    let n2707: ZN = zsel_n(n1240, r_c302, n1654);
    let n2708: ZN = zsel_n(n1240, r_c303, n1655);
    let n2709: ZN = zsel_n(n1240, r_c312, n1656);
    let n2710: ZN = zsel_n(n1240, r_c313, n1657);
    let n2711: ZB = zb_or(n1240, n2698);
    let n2712: ZB = zn_gt(n2699, zn_splat(P8::from_raw(0i32)));
    let n2713: ZB = zn_le(n2699, zn_splat(P8::from_raw(0i32)));
    let n2714: ZB = zb_and(n2711, n2712);
    let n2715: ZB = zb_and(n2711, n2713);
    let n2716: ZB = zb_and(n2599, n2715);
    let n2717: ZB = zb_and(n2598, n2715);
    let n2718: ZB = zb_or(n2716, n2717);
    let n2719: ZB = zb_and(n2604, n2718);
    let n2720: ZB = zb_and(n2605, n2718);
    let n2721: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2709);
    let n2722: ZB = zb_or(n2719, n2720);
    let n2723: ZN = zsel_n(n2712, n2583, n2610);
    let n2724: ZN = zsel_n(n2712, n2709, n2721);
    let n2725: ZB = zb_or(n2714, n2722);
    let n2726: ZB = zb_and(n1711, n2574);
    let n2727: ZN = zsel_n(n1240, r_c301, n1694);
    let n2728: ZN = zsel_n(n1240, r_c302, n1695);
    let n2729: ZN = zsel_n(n1240, r_c312, n1696);
    let n2730: ZN = zsel_n(n1240, r_c313, n1697);
    let n2731: ZB = zb_or(n1240, n2726);
    let n2732: ZB = zb_and(n2712, n2731);
    let n2733: ZB = zb_and(n2713, n2731);
    let n2734: ZB = zb_and(n2599, n2733);
    let n2735: ZB = zb_and(n2598, n2733);
    let n2736: ZB = zb_or(n2734, n2735);
    let n2737: ZB = zb_and(n2604, n2736);
    let n2738: ZB = zb_and(n2605, n2736);
    let n2739: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2729);
    let n2740: ZB = zb_or(n2737, n2738);
    let n2741: ZN = zsel_n(n2712, n2729, n2739);
    let n2742: ZB = zb_or(n2732, n2740);
    let n2743: ZB = zb_and(n1746, n2574);
    let n2744: ZN = zsel_n(n1240, r_c302, n1730);
    let n2745: ZN = zsel_n(n1240, r_c312, n1731);
    let n2746: ZN = zsel_n(n1240, r_c313, n1732);
    let n2747: ZB = zb_or(n1240, n2743);
    let n2748: ZB = zb_and(n2712, n2747);
    let n2749: ZB = zb_and(n2713, n2747);
    let n2750: ZB = zb_and(n2599, n2749);
    let n2751: ZB = zb_and(n2598, n2749);
    let n2752: ZB = zb_or(n2750, n2751);
    let n2753: ZB = zb_and(n2604, n2752);
    let n2754: ZB = zb_and(n2605, n2752);
    let n2755: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2745);
    let n2756: ZB = zb_or(n2753, n2754);
    let n2757: ZN = zsel_n(n2712, n2745, n2755);
    let n2758: ZB = zb_or(n2748, n2756);
    let n2759: ZB = zb_and(n1786, n2574);
    let n2760: ZN = zsel_n(n1240, r_c300, n1767);
    let n2761: ZN = zsel_n(n1240, r_c301, n1768);
    let n2762: ZN = zsel_n(n1240, r_c302, n1769);
    let n2763: ZN = zsel_n(n1240, r_c303, n1770);
    let n2764: ZN = zsel_n(n1240, r_c312, n1771);
    let n2765: ZN = zsel_n(n1240, r_c313, n1772);
    let n2766: ZB = zb_or(n1240, n2759);
    let n2767: ZB = zb_and(n2712, n2766);
    let n2768: ZB = zb_and(n2713, n2766);
    let n2769: ZB = zb_and(n2599, n2768);
    let n2770: ZB = zb_and(n2598, n2768);
    let n2771: ZB = zb_or(n2769, n2770);
    let n2772: ZB = zb_and(n2604, n2771);
    let n2773: ZB = zb_and(n2605, n2771);
    let n2774: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2764);
    let n2775: ZB = zb_or(n2772, n2773);
    let n2776: ZN = zsel_n(n2712, n2764, n2774);
    let n2777: ZB = zb_or(n2767, n2775);
    let n2778: ZN = zsel_n(n1240, r_c312, n1801);
    let n2779: ZN = zsel_n(n1240, r_c313, n1802);
    let n2780: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2778);
    let n2781: ZN = zsel_n(n2712, n2778, n2780);
    let n2782: ZN = zsel_n(n1240, r_c312, n1807);
    let n2783: ZN = zsel_n(n1240, r_c313, n1808);
    let n2784: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2782);
    let n2785: ZN = zsel_n(n2712, n2782, n2784);
    let n2786: ZN = zsel_n(n1240, r_c303, n1813);
    let n2787: ZN = zsel_n(n1240, r_c313, n1814);
    let n2788: ZN = zsel_n(n1240, r_c313, n1816);
    let n2789: ZN = zsel_n(n1240, r_c313, n1818);
    let n2790: ZB = zb_and(n1851, n2574);
    let n2791: ZN = zsel_n(n1240, r_c312, n1836);
    let n2792: ZN = zsel_n(n1240, r_c313, n1837);
    let n2793: ZB = zb_or(n1240, n2790);
    let n2794: ZB = zb_and(n2712, n2793);
    let n2795: ZB = zb_and(n2713, n2793);
    let n2796: ZB = zb_and(n2599, n2795);
    let n2797: ZB = zb_and(n2598, n2795);
    let n2798: ZB = zb_or(n2796, n2797);
    let n2799: ZB = zb_and(n2604, n2798);
    let n2800: ZB = zb_and(n2605, n2798);
    let n2801: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2791);
    let n2802: ZB = zb_or(n2799, n2800);
    let n2803: ZN = zsel_n(n2712, n2791, n2801);
    let n2804: ZB = zb_or(n2794, n2802);
    let n2805: ZB = zb_and(n1884, n2574);
    let n2806: ZN = zsel_n(n1240, r_c312, n1869);
    let n2807: ZN = zsel_n(n1240, r_c313, n1870);
    let n2808: ZB = zb_or(n1240, n2805);
    let n2809: ZB = zb_and(n2712, n2808);
    let n2810: ZB = zb_and(n2713, n2808);
    let n2811: ZB = zb_and(n2599, n2810);
    let n2812: ZB = zb_and(n2598, n2810);
    let n2813: ZB = zb_or(n2811, n2812);
    let n2814: ZB = zb_and(n2604, n2813);
    let n2815: ZB = zb_and(n2605, n2813);
    let n2816: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2806);
    let n2817: ZB = zb_or(n2814, n2815);
    let n2818: ZN = zsel_n(n2712, n2806, n2816);
    let n2819: ZB = zb_or(n2809, n2817);
    let n2820: ZB = zb_and(n1917, n2574);
    let n2821: ZN = zsel_n(n1240, r_c312, n1902);
    let n2822: ZN = zsel_n(n1240, r_c313, n1903);
    let n2823: ZB = zb_or(n1240, n2820);
    let n2824: ZB = zb_and(n2712, n2823);
    let n2825: ZB = zb_and(n2713, n2823);
    let n2826: ZB = zb_and(n2599, n2825);
    let n2827: ZB = zb_and(n2598, n2825);
    let n2828: ZB = zb_or(n2826, n2827);
    let n2829: ZB = zb_and(n2604, n2828);
    let n2830: ZB = zb_and(n2605, n2828);
    let n2831: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2821);
    let n2832: ZB = zb_or(n2829, n2830);
    let n2833: ZN = zsel_n(n2712, n2821, n2831);
    let n2834: ZB = zb_or(n2824, n2832);
    let n2835: ZB = zb_and(n1948, n2574);
    let n2836: ZN = zsel_n(n1240, r_c312, n1933);
    let n2837: ZN = zsel_n(n1240, r_c313, n1934);
    let n2838: ZB = zb_or(n1240, n2835);
    let n2839: ZB = zb_and(n2712, n2838);
    let n2840: ZB = zb_and(n2713, n2838);
    let n2841: ZB = zb_and(n2599, n2840);
    let n2842: ZB = zb_and(n2598, n2840);
    let n2843: ZB = zb_or(n2841, n2842);
    let n2844: ZB = zb_and(n2604, n2843);
    let n2845: ZB = zb_and(n2605, n2843);
    let n2846: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2836);
    let n2847: ZB = zb_or(n2844, n2845);
    let n2848: ZN = zsel_n(n2712, n2836, n2846);
    let n2849: ZB = zb_or(n2839, n2847);
    let n2850: ZN = zsel_n(n1240, r_c312, n1963);
    let n2851: ZN = zsel_n(n1240, r_c313, n1964);
    let n2852: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2850);
    let n2853: ZN = zsel_n(n2712, n2850, n2852);
    let n2854: ZN = zsel_n(n1240, r_c312, n1969);
    let n2855: ZN = zsel_n(n1240, r_c313, n1970);
    let n2856: ZN = zsel_n(n2604, zn_splat(P8::from_raw(0i32)), n2854);
    let n2857: ZN = zsel_n(n2712, n2854, n2856);
    let n2858: ZN = zsel_n(n1240, r_c313, n1974);
    let n2859: ZN = zsel_n(n1240, r_c313, n1976);
    let n2860: ZN = zsel_n(n1240, r_c313, n1978);
    let n2863: ZW = zw_bits_n(r_c39);
    let n2864: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2863, 39u64);
    let n2865: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2863, 39u64);
    let n2866: ZW = zw_bits_n(n80);
    let n2867: ZW = zw_mix1(n2864, n2866, 84u64);
    let n2868: ZW = zw_mix2(n2865, n2866, 84u64);
    let n2869: ZW = zw_bits_n(n131);
    let n2870: ZW = zw_mix1(n2867, n2869, 85u64);
    let n2871: ZW = zw_mix2(n2868, n2869, 85u64);
    let n2872: ZW = zw_bits_n(n130);
    let n2873: ZW = zw_mix1(n2870, n2872, 86u64);
    let n2874: ZW = zw_mix2(n2871, n2872, 86u64);
    let n2875: ZW = zw_bits_n(r_c87);
    let n2876: ZW = zw_mix1(n2873, n2875, 87u64);
    let n2877: ZW = zw_mix2(n2874, n2875, 87u64);
    let n2878: ZW = zw_bits_n(n362);
    let n2879: ZW = zw_mix1(n2876, n2878, 256u64);
    let n2880: ZW = zw_mix2(n2877, n2878, 256u64);
    let n2881: ZW = zw_bits_n(n363);
    let n2882: ZW = zw_mix1(n2879, n2881, 280u64);
    let n2883: ZW = zw_mix2(n2880, n2881, 280u64);
    let n2884: ZW = zw_bits_n(n364);
    let n2885: ZW = zw_mix1(n2882, n2884, 281u64);
    let n2886: ZW = zw_mix2(n2883, n2884, 281u64);
    let n2887: ZW = zw_bits_n(r_c20);
    let n2888: ZW = zw_mix1(n2885, n2887, 20u64);
    let n2889: ZW = zw_mix2(n2886, n2887, 20u64);
    let n2890: ZW = zw_bits_b(r_c41);
    let n2891: ZW = zw_mix1(n2888, n2890, 41u64);
    let n2892: ZW = zw_mix2(n2889, n2890, 41u64);
    let n2893: ZW = zw_bits_n(n1064);
    let n2894: ZW = zw_mix1(n2891, n2893, 236u64);
    let n2895: ZW = zw_mix2(n2892, n2893, 236u64);
    let n2896: ZW = zw_bits_n(n1205);
    let n2897: ZW = zw_mix1(n2894, n2896, 238u64);
    let n2898: ZW = zw_mix2(n2895, n2896, 238u64);
    let n2899: ZW = zw_bits_n(n1062);
    let n2900: ZW = zw_mix1(n2897, n2899, 241u64);
    let n2901: ZW = zw_mix2(n2898, n2899, 241u64);
    let n2902: u64 = false as u64;
    let n2903: ZW = zw_mix1(n2900, zw_splat(n2902), 248u64);
    let n2904: ZW = zw_mix2(n2901, zw_splat(n2902), 248u64);
    let n2905: ZW = zw_mix1(n2903, zw_splat(n2902), 249u64);
    let n2906: ZW = zw_mix2(n2904, zw_splat(n2902), 249u64);
    let n2907: ZW = zw_bits_n(n1257);
    let n2908: ZW = zw_mix1(n2905, n2907, 255u64);
    let n2909: ZW = zw_mix2(n2906, n2907, 255u64);
    let n2910: ZW = zw_bits_n(r_c300);
    let n2911: ZW = zw_mix1(n2908, n2910, 270u64);
    let n2912: ZW = zw_mix2(n2909, n2910, 270u64);
    let n2913: ZW = zw_bits_n(r_c301);
    let n2914: ZW = zw_mix1(n2911, n2913, 271u64);
    let n2915: ZW = zw_mix2(n2912, n2913, 271u64);
    let n2916: ZW = zw_bits_n(r_c302);
    let n2917: ZW = zw_mix1(n2914, n2916, 272u64);
    let n2918: ZW = zw_mix2(n2915, n2916, 272u64);
    let n2919: ZW = zw_bits_n(r_c303);
    let n2920: ZW = zw_mix1(n2917, n2919, 273u64);
    let n2921: ZW = zw_mix2(n2918, n2919, 273u64);
    let n2922: ZW = zw_bits_b(n1206);
    let n2923: ZW = zw_mix1(n2920, n2922, 274u64);
    let n2924: ZW = zw_mix2(n2921, n2922, 274u64);
    let n2925: ZW = zw_bits_n(n1258);
    let n2926: ZW = zw_mix1(n2923, n2925, 282u64);
    let n2927: ZW = zw_mix2(n2924, n2925, 282u64);
    let n2928: ZW = zw_bits_n(n1208);
    let n2929: ZW = zw_mix1(n2926, n2928, 283u64);
    let n2930: ZW = zw_mix2(n2927, n2928, 283u64);
    let n2931: ZW = zw_bits_b(n1338);
    let n2932: ZW = zw_mix1(n2920, n2931, 274u64);
    let n2933: ZW = zw_mix2(n2921, n2931, 274u64);
    let n2934: ZW = zw_bits_n(n1364);
    let n2935: ZW = zw_mix1(n2932, n2934, 282u64);
    let n2936: ZW = zw_mix2(n2933, n2934, 282u64);
    let n2937: ZW = zw_bits_n(n1340);
    let n2938: ZW = zw_mix1(n2935, n2937, 283u64);
    let n2939: ZW = zw_mix2(n2936, n2937, 283u64);
    let n2940: ZW = zw_bits_b(n1418);
    let n2941: ZW = zw_mix1(n2920, n2940, 274u64);
    let n2942: ZW = zw_mix2(n2921, n2940, 274u64);
    let n2943: ZW = zw_bits_n(n1444);
    let n2944: ZW = zw_mix1(n2941, n2943, 282u64);
    let n2945: ZW = zw_mix2(n2942, n2943, 282u64);
    let n2946: ZW = zw_bits_n(n1420);
    let n2947: ZW = zw_mix1(n2944, n2946, 283u64);
    let n2948: ZW = zw_mix2(n2945, n2946, 283u64);
    let n2949: ZW = zw_bits_n(n1478);
    let n2950: ZW = zw_mix1(n2897, n2949, 241u64);
    let n2951: ZW = zw_mix2(n2898, n2949, 241u64);
    let n2952: ZW = zw_mix1(n2950, zw_splat(n2902), 248u64);
    let n2953: ZW = zw_mix2(n2951, zw_splat(n2902), 248u64);
    let n2954: u64 = true as u64;
    let n2955: ZW = zw_mix1(n2952, zw_splat(n2954), 249u64);
    let n2956: ZW = zw_mix2(n2953, zw_splat(n2954), 249u64);
    let n2957: ZW = zw_mix1(n2955, n2907, 255u64);
    let n2958: ZW = zw_mix2(n2956, n2907, 255u64);
    let n2959: ZW = zw_mix1(n2957, n2910, 270u64);
    let n2960: ZW = zw_mix2(n2958, n2910, 270u64);
    let n2961: ZW = zw_mix1(n2959, n2913, 271u64);
    let n2962: ZW = zw_mix2(n2960, n2913, 271u64);
    let n2963: ZW = zw_mix1(n2961, n2916, 272u64);
    let n2964: ZW = zw_mix2(n2962, n2916, 272u64);
    let n2965: ZW = zw_mix1(n2963, n2919, 273u64);
    let n2966: ZW = zw_mix2(n2964, n2919, 273u64);
    let n2967: ZW = zw_mix1(n2965, n2922, 274u64);
    let n2968: ZW = zw_mix2(n2966, n2922, 274u64);
    let n2969: ZW = zw_bits_n(n1504);
    let n2970: ZW = zw_mix1(n2967, n2969, 282u64);
    let n2971: ZW = zw_mix2(n2968, n2969, 282u64);
    let n2972: ZW = zw_bits_n(n1480);
    let n2973: ZW = zw_mix1(n2970, n2972, 283u64);
    let n2974: ZW = zw_mix2(n2971, n2972, 283u64);
    let n2975: ZW = zw_mix1(n2965, n2931, 274u64);
    let n2976: ZW = zw_mix2(n2966, n2931, 274u64);
    let n2977: ZW = zw_bits_n(n1562);
    let n2978: ZW = zw_mix1(n2975, n2977, 282u64);
    let n2979: ZW = zw_mix2(n2976, n2977, 282u64);
    let n2980: ZW = zw_bits_n(n1538);
    let n2981: ZW = zw_mix1(n2978, n2980, 283u64);
    let n2982: ZW = zw_mix2(n2979, n2980, 283u64);
    let n2983: ZW = zw_mix1(n2965, n2940, 274u64);
    let n2984: ZW = zw_mix2(n2966, n2940, 274u64);
    let n2985: ZW = zw_bits_n(n1620);
    let n2986: ZW = zw_mix1(n2983, n2985, 282u64);
    let n2987: ZW = zw_mix2(n2984, n2985, 282u64);
    let n2988: ZW = zw_bits_n(n1596);
    let n2989: ZW = zw_mix1(n2986, n2988, 283u64);
    let n2990: ZW = zw_mix2(n2987, n2988, 283u64);
    let n2991: ZW = zw_bits_n(n1648);
    let n2992: ZW = zw_mix1(n2885, n2991, 20u64);
    let n2993: ZW = zw_mix2(n2886, n2991, 20u64);
    let n2994: ZW = zw_bits_b(n1649);
    let n2995: ZW = zw_mix1(n2992, n2994, 41u64);
    let n2996: ZW = zw_mix2(n2993, n2994, 41u64);
    let n2997: ZW = zw_bits_n(n1650);
    let n2998: ZW = zw_mix1(n2995, n2997, 236u64);
    let n2999: ZW = zw_mix2(n2996, n2997, 236u64);
    let n3000: ZW = zw_bits_n(n1651);
    let n3001: ZW = zw_mix1(n2998, n3000, 238u64);
    let n3002: ZW = zw_mix2(n2999, n3000, 238u64);
    let n3003: ZW = zw_mix1(n3001, n2899, 241u64);
    let n3004: ZW = zw_mix2(n3002, n2899, 241u64);
    let n3005: ZW = zw_mix1(n3003, zw_splat(n2954), 248u64);
    let n3006: ZW = zw_mix2(n3004, zw_splat(n2954), 248u64);
    let n3007: ZW = zw_mix1(n3005, zw_splat(n2902), 249u64);
    let n3008: ZW = zw_mix2(n3006, zw_splat(n2902), 249u64);
    let n3009: ZW = zw_bits_n(n1684);
    let n3010: ZW = zw_mix1(n3007, n3009, 255u64);
    let n3011: ZW = zw_mix2(n3008, n3009, 255u64);
    let n3012: ZW = zw_bits_n(n1652);
    let n3013: ZW = zw_mix1(n3010, n3012, 270u64);
    let n3014: ZW = zw_mix2(n3011, n3012, 270u64);
    let n3015: ZW = zw_bits_n(n1653);
    let n3016: ZW = zw_mix1(n3013, n3015, 271u64);
    let n3017: ZW = zw_mix2(n3014, n3015, 271u64);
    let n3018: ZW = zw_bits_n(n1654);
    let n3019: ZW = zw_mix1(n3016, n3018, 272u64);
    let n3020: ZW = zw_mix2(n3017, n3018, 272u64);
    let n3021: ZW = zw_bits_n(n1655);
    let n3022: ZW = zw_mix1(n3019, n3021, 273u64);
    let n3023: ZW = zw_mix2(n3020, n3021, 273u64);
    let n3024: ZW = zw_mix1(n3022, n2922, 274u64);
    let n3025: ZW = zw_mix2(n3023, n2922, 274u64);
    let n3026: ZW = zw_bits_n(n1685);
    let n3027: ZW = zw_mix1(n3024, n3026, 282u64);
    let n3028: ZW = zw_mix2(n3025, n3026, 282u64);
    let n3029: ZW = zw_bits_n(n1657);
    let n3030: ZW = zw_mix1(n3027, n3029, 283u64);
    let n3031: ZW = zw_mix2(n3028, n3029, 283u64);
    let n3032: ZW = zw_bits_n(n1694);
    let n3033: ZW = zw_mix1(n3013, n3032, 271u64);
    let n3034: ZW = zw_mix2(n3014, n3032, 271u64);
    let n3035: ZW = zw_bits_n(n1695);
    let n3036: ZW = zw_mix1(n3033, n3035, 272u64);
    let n3037: ZW = zw_mix2(n3034, n3035, 272u64);
    let n3038: ZW = zw_mix1(n3036, n3021, 273u64);
    let n3039: ZW = zw_mix2(n3037, n3021, 273u64);
    let n3040: ZW = zw_mix1(n3038, n2931, 274u64);
    let n3041: ZW = zw_mix2(n3039, n2931, 274u64);
    let n3042: ZW = zw_bits_n(n1722);
    let n3043: ZW = zw_mix1(n3040, n3042, 282u64);
    let n3044: ZW = zw_mix2(n3041, n3042, 282u64);
    let n3045: ZW = zw_bits_n(n1697);
    let n3046: ZW = zw_mix1(n3043, n3045, 283u64);
    let n3047: ZW = zw_mix2(n3044, n3045, 283u64);
    let n3048: ZW = zw_bits_n(n1730);
    let n3049: ZW = zw_mix1(n3033, n3048, 272u64);
    let n3050: ZW = zw_mix2(n3034, n3048, 272u64);
    let n3051: ZW = zw_mix1(n3049, n3021, 273u64);
    let n3052: ZW = zw_mix2(n3050, n3021, 273u64);
    let n3053: ZW = zw_mix1(n3051, n2940, 274u64);
    let n3054: ZW = zw_mix2(n3052, n2940, 274u64);
    let n3055: ZW = zw_bits_n(n1757);
    let n3056: ZW = zw_mix1(n3053, n3055, 282u64);
    let n3057: ZW = zw_mix2(n3054, n3055, 282u64);
    let n3058: ZW = zw_bits_n(n1732);
    let n3059: ZW = zw_mix1(n3056, n3058, 283u64);
    let n3060: ZW = zw_mix2(n3057, n3058, 283u64);
    let n3061: ZW = zw_bits_n(n1767);
    let n3062: ZW = zw_mix1(n3010, n3061, 270u64);
    let n3063: ZW = zw_mix2(n3011, n3061, 270u64);
    let n3064: ZW = zw_bits_n(n1768);
    let n3065: ZW = zw_mix1(n3062, n3064, 271u64);
    let n3066: ZW = zw_mix2(n3063, n3064, 271u64);
    let n3067: ZW = zw_bits_n(n1769);
    let n3068: ZW = zw_mix1(n3065, n3067, 272u64);
    let n3069: ZW = zw_mix2(n3066, n3067, 272u64);
    let n3070: ZW = zw_bits_n(n1770);
    let n3071: ZW = zw_mix1(n3068, n3070, 273u64);
    let n3072: ZW = zw_mix2(n3069, n3070, 273u64);
    let n3073: ZW = zw_mix1(n3071, n2922, 274u64);
    let n3074: ZW = zw_mix2(n3072, n2922, 274u64);
    let n3075: ZW = zw_bits_n(n1797);
    let n3076: ZW = zw_mix1(n3073, n3075, 282u64);
    let n3077: ZW = zw_mix2(n3074, n3075, 282u64);
    let n3078: ZW = zw_bits_n(n1772);
    let n3079: ZW = zw_mix1(n3076, n3078, 283u64);
    let n3080: ZW = zw_mix2(n3077, n3078, 283u64);
    let n3081: ZW = zw_mix1(n3062, n3032, 271u64);
    let n3082: ZW = zw_mix2(n3063, n3032, 271u64);
    let n3083: ZW = zw_mix1(n3081, n3035, 272u64);
    let n3084: ZW = zw_mix2(n3082, n3035, 272u64);
    let n3085: ZW = zw_mix1(n3083, n3070, 273u64);
    let n3086: ZW = zw_mix2(n3084, n3070, 273u64);
    let n3087: ZW = zw_mix1(n3085, n2931, 274u64);
    let n3088: ZW = zw_mix2(n3086, n2931, 274u64);
    let n3089: ZW = zw_bits_n(n1804);
    let n3090: ZW = zw_mix1(n3087, n3089, 282u64);
    let n3091: ZW = zw_mix2(n3088, n3089, 282u64);
    let n3092: ZW = zw_bits_n(n1802);
    let n3093: ZW = zw_mix1(n3090, n3092, 283u64);
    let n3094: ZW = zw_mix2(n3091, n3092, 283u64);
    let n3095: ZW = zw_mix1(n3081, n3048, 272u64);
    let n3096: ZW = zw_mix2(n3082, n3048, 272u64);
    let n3097: ZW = zw_mix1(n3095, n3070, 273u64);
    let n3098: ZW = zw_mix2(n3096, n3070, 273u64);
    let n3099: ZW = zw_mix1(n3097, n2940, 274u64);
    let n3100: ZW = zw_mix2(n3098, n2940, 274u64);
    let n3101: ZW = zw_bits_n(n1810);
    let n3102: ZW = zw_mix1(n3099, n3101, 282u64);
    let n3103: ZW = zw_mix2(n3100, n3101, 282u64);
    let n3104: ZW = zw_bits_n(n1808);
    let n3105: ZW = zw_mix1(n3102, n3104, 283u64);
    let n3106: ZW = zw_mix2(n3103, n3104, 283u64);
    let n3107: ZW = zw_bits_n(n1813);
    let n3108: ZW = zw_mix1(n3068, n3107, 273u64);
    let n3109: ZW = zw_mix2(n3069, n3107, 273u64);
    let n3110: ZW = zw_mix1(n3108, n2922, 274u64);
    let n3111: ZW = zw_mix2(n3109, n2922, 274u64);
    let n3112: ZW = zw_mix1(n3110, n3075, 282u64);
    let n3113: ZW = zw_mix2(n3111, n3075, 282u64);
    let n3114: ZW = zw_bits_n(n1814);
    let n3115: ZW = zw_mix1(n3112, n3114, 283u64);
    let n3116: ZW = zw_mix2(n3113, n3114, 283u64);
    let n3117: ZW = zw_mix1(n3083, n3107, 273u64);
    let n3118: ZW = zw_mix2(n3084, n3107, 273u64);
    let n3119: ZW = zw_mix1(n3117, n2931, 274u64);
    let n3120: ZW = zw_mix2(n3118, n2931, 274u64);
    let n3121: ZW = zw_mix1(n3119, n3089, 282u64);
    let n3122: ZW = zw_mix2(n3120, n3089, 282u64);
    let n3123: ZW = zw_bits_n(n1816);
    let n3124: ZW = zw_mix1(n3121, n3123, 283u64);
    let n3125: ZW = zw_mix2(n3122, n3123, 283u64);
    let n3126: ZW = zw_mix1(n3095, n3107, 273u64);
    let n3127: ZW = zw_mix2(n3096, n3107, 273u64);
    let n3128: ZW = zw_mix1(n3126, n2940, 274u64);
    let n3129: ZW = zw_mix2(n3127, n2940, 274u64);
    let n3130: ZW = zw_mix1(n3128, n3101, 282u64);
    let n3131: ZW = zw_mix2(n3129, n3101, 282u64);
    let n3132: ZW = zw_bits_n(n1818);
    let n3133: ZW = zw_mix1(n3130, n3132, 283u64);
    let n3134: ZW = zw_mix2(n3131, n3132, 283u64);
    let n3135: ZW = zw_mix1(n3001, n2949, 241u64);
    let n3136: ZW = zw_mix2(n3002, n2949, 241u64);
    let n3137: ZW = zw_mix1(n3135, zw_splat(n2954), 248u64);
    let n3138: ZW = zw_mix2(n3136, zw_splat(n2954), 248u64);
    let n3139: ZW = zw_mix1(n3137, zw_splat(n2954), 249u64);
    let n3140: ZW = zw_mix2(n3138, zw_splat(n2954), 249u64);
    let n3141: ZW = zw_mix1(n3139, n3009, 255u64);
    let n3142: ZW = zw_mix2(n3140, n3009, 255u64);
    let n3143: ZW = zw_mix1(n3141, n3012, 270u64);
    let n3144: ZW = zw_mix2(n3142, n3012, 270u64);
    let n3145: ZW = zw_mix1(n3143, n3015, 271u64);
    let n3146: ZW = zw_mix2(n3144, n3015, 271u64);
    let n3147: ZW = zw_mix1(n3145, n3018, 272u64);
    let n3148: ZW = zw_mix2(n3146, n3018, 272u64);
    let n3149: ZW = zw_mix1(n3147, n3021, 273u64);
    let n3150: ZW = zw_mix2(n3148, n3021, 273u64);
    let n3151: ZW = zw_mix1(n3149, n2922, 274u64);
    let n3152: ZW = zw_mix2(n3150, n2922, 274u64);
    let n3153: ZW = zw_bits_n(n1862);
    let n3154: ZW = zw_mix1(n3151, n3153, 282u64);
    let n3155: ZW = zw_mix2(n3152, n3153, 282u64);
    let n3156: ZW = zw_bits_n(n1837);
    let n3157: ZW = zw_mix1(n3154, n3156, 283u64);
    let n3158: ZW = zw_mix2(n3155, n3156, 283u64);
    let n3159: ZW = zw_mix1(n3143, n3032, 271u64);
    let n3160: ZW = zw_mix2(n3144, n3032, 271u64);
    let n3161: ZW = zw_mix1(n3159, n3035, 272u64);
    let n3162: ZW = zw_mix2(n3160, n3035, 272u64);
    let n3163: ZW = zw_mix1(n3161, n3021, 273u64);
    let n3164: ZW = zw_mix2(n3162, n3021, 273u64);
    let n3165: ZW = zw_mix1(n3163, n2931, 274u64);
    let n3166: ZW = zw_mix2(n3164, n2931, 274u64);
    let n3167: ZW = zw_bits_n(n1895);
    let n3168: ZW = zw_mix1(n3165, n3167, 282u64);
    let n3169: ZW = zw_mix2(n3166, n3167, 282u64);
    let n3170: ZW = zw_bits_n(n1870);
    let n3171: ZW = zw_mix1(n3168, n3170, 283u64);
    let n3172: ZW = zw_mix2(n3169, n3170, 283u64);
    let n3173: ZW = zw_mix1(n3159, n3048, 272u64);
    let n3174: ZW = zw_mix2(n3160, n3048, 272u64);
    let n3175: ZW = zw_mix1(n3173, n3021, 273u64);
    let n3176: ZW = zw_mix2(n3174, n3021, 273u64);
    let n3177: ZW = zw_mix1(n3175, n2940, 274u64);
    let n3178: ZW = zw_mix2(n3176, n2940, 274u64);
    let n3179: ZW = zw_bits_n(n1928);
    let n3180: ZW = zw_mix1(n3177, n3179, 282u64);
    let n3181: ZW = zw_mix2(n3178, n3179, 282u64);
    let n3182: ZW = zw_bits_n(n1903);
    let n3183: ZW = zw_mix1(n3180, n3182, 283u64);
    let n3184: ZW = zw_mix2(n3181, n3182, 283u64);
    let n3185: ZW = zw_mix1(n3141, n3061, 270u64);
    let n3186: ZW = zw_mix2(n3142, n3061, 270u64);
    let n3187: ZW = zw_mix1(n3185, n3064, 271u64);
    let n3188: ZW = zw_mix2(n3186, n3064, 271u64);
    let n3189: ZW = zw_mix1(n3187, n3067, 272u64);
    let n3190: ZW = zw_mix2(n3188, n3067, 272u64);
    let n3191: ZW = zw_mix1(n3189, n3070, 273u64);
    let n3192: ZW = zw_mix2(n3190, n3070, 273u64);
    let n3193: ZW = zw_mix1(n3191, n2922, 274u64);
    let n3194: ZW = zw_mix2(n3192, n2922, 274u64);
    let n3195: ZW = zw_bits_n(n1959);
    let n3196: ZW = zw_mix1(n3193, n3195, 282u64);
    let n3197: ZW = zw_mix2(n3194, n3195, 282u64);
    let n3198: ZW = zw_bits_n(n1934);
    let n3199: ZW = zw_mix1(n3196, n3198, 283u64);
    let n3200: ZW = zw_mix2(n3197, n3198, 283u64);
    let n3201: ZW = zw_mix1(n3185, n3032, 271u64);
    let n3202: ZW = zw_mix2(n3186, n3032, 271u64);
    let n3203: ZW = zw_mix1(n3201, n3035, 272u64);
    let n3204: ZW = zw_mix2(n3202, n3035, 272u64);
    let n3205: ZW = zw_mix1(n3203, n3070, 273u64);
    let n3206: ZW = zw_mix2(n3204, n3070, 273u64);
    let n3207: ZW = zw_mix1(n3205, n2931, 274u64);
    let n3208: ZW = zw_mix2(n3206, n2931, 274u64);
    let n3209: ZW = zw_bits_n(n1966);
    let n3210: ZW = zw_mix1(n3207, n3209, 282u64);
    let n3211: ZW = zw_mix2(n3208, n3209, 282u64);
    let n3212: ZW = zw_bits_n(n1964);
    let n3213: ZW = zw_mix1(n3210, n3212, 283u64);
    let n3214: ZW = zw_mix2(n3211, n3212, 283u64);
    let n3215: ZW = zw_mix1(n3201, n3048, 272u64);
    let n3216: ZW = zw_mix2(n3202, n3048, 272u64);
    let n3217: ZW = zw_mix1(n3215, n3070, 273u64);
    let n3218: ZW = zw_mix2(n3216, n3070, 273u64);
    let n3219: ZW = zw_mix1(n3217, n2940, 274u64);
    let n3220: ZW = zw_mix2(n3218, n2940, 274u64);
    let n3221: ZW = zw_bits_n(n1972);
    let n3222: ZW = zw_mix1(n3219, n3221, 282u64);
    let n3223: ZW = zw_mix2(n3220, n3221, 282u64);
    let n3224: ZW = zw_bits_n(n1970);
    let n3225: ZW = zw_mix1(n3222, n3224, 283u64);
    let n3226: ZW = zw_mix2(n3223, n3224, 283u64);
    let n3227: ZW = zw_mix1(n3189, n3107, 273u64);
    let n3228: ZW = zw_mix2(n3190, n3107, 273u64);
    let n3229: ZW = zw_mix1(n3227, n2922, 274u64);
    let n3230: ZW = zw_mix2(n3228, n2922, 274u64);
    let n3231: ZW = zw_mix1(n3229, n3195, 282u64);
    let n3232: ZW = zw_mix2(n3230, n3195, 282u64);
    let n3233: ZW = zw_bits_n(n1974);
    let n3234: ZW = zw_mix1(n3231, n3233, 283u64);
    let n3235: ZW = zw_mix2(n3232, n3233, 283u64);
    let n3236: ZW = zw_mix1(n3203, n3107, 273u64);
    let n3237: ZW = zw_mix2(n3204, n3107, 273u64);
    let n3238: ZW = zw_mix1(n3236, n2931, 274u64);
    let n3239: ZW = zw_mix2(n3237, n2931, 274u64);
    let n3240: ZW = zw_mix1(n3238, n3209, 282u64);
    let n3241: ZW = zw_mix2(n3239, n3209, 282u64);
    let n3242: ZW = zw_bits_n(n1976);
    let n3243: ZW = zw_mix1(n3240, n3242, 283u64);
    let n3244: ZW = zw_mix2(n3241, n3242, 283u64);
    let n3245: ZW = zw_mix1(n3215, n3107, 273u64);
    let n3246: ZW = zw_mix2(n3216, n3107, 273u64);
    let n3247: ZW = zw_mix1(n3245, n2940, 274u64);
    let n3248: ZW = zw_mix2(n3246, n2940, 274u64);
    let n3249: ZW = zw_mix1(n3247, n3221, 282u64);
    let n3250: ZW = zw_mix2(n3248, n3221, 282u64);
    let n3251: ZW = zw_bits_n(n1978);
    let n3252: ZW = zw_mix1(n3249, n3251, 283u64);
    let n3253: ZW = zw_mix2(n3250, n3251, 283u64);
    let n3254: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2866, 84u64);
    let n3255: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2866, 84u64);
    let n3256: ZW = zw_mix1(n3254, n2869, 85u64);
    let n3257: ZW = zw_mix2(n3255, n2869, 85u64);
    let n3258: ZW = zw_mix1(n3256, n2872, 86u64);
    let n3259: ZW = zw_mix2(n3257, n2872, 86u64);
    let n3260: ZW = zw_bits_n(n2136);
    let n3261: ZW = zw_mix1(n3258, n3260, 87u64);
    let n3262: ZW = zw_mix2(n3259, n3260, 87u64);
    let n3263: ZW = zw_bits_n(n2127);
    let n3264: ZW = zw_mix1(n3261, n3263, 241u64);
    let n3265: ZW = zw_mix2(n3262, n3263, 241u64);
    let n3266: ZW = zw_bits_n(n2131);
    let n3267: ZW = zw_mix1(n3264, n3266, 249u64);
    let n3268: ZW = zw_mix2(n3265, n3266, 249u64);
    let n3269: ZW = zw_mix1(n3267, n2887, 20u64);
    let n3270: ZW = zw_mix2(n3268, n2887, 20u64);
    let n3271: ZW = zw_mix1(n3269, n2890, 41u64);
    let n3272: ZW = zw_mix2(n3270, n2890, 41u64);
    let n3273: ZW = zw_mix1(n3267, n2991, 20u64);
    let n3274: ZW = zw_mix2(n3268, n2991, 20u64);
    let n3275: ZW = zw_mix1(n3273, n2994, 41u64);
    let n3276: ZW = zw_mix2(n3274, n2994, 41u64);
    let n3277: ZW = zw_mix1(n3258, n2887, 20u64);
    let n3278: ZW = zw_mix2(n3259, n2887, 20u64);
    let n3279: ZW = zw_bits_b(n2432);
    let n3280: ZW = zw_mix1(n3277, n3279, 38u64);
    let n3281: ZW = zw_mix2(n3278, n3279, 38u64);
    let n3282: ZW = zw_bits_n(n2437);
    let n3283: ZW = zw_mix1(n3280, n3282, 39u64);
    let n3284: ZW = zw_mix2(n3281, n3282, 39u64);
    let n3285: ZW = zw_bits_n(n2436);
    let n3286: ZW = zw_mix1(n3283, n3285, 87u64);
    let n3287: ZW = zw_mix2(n3284, n3285, 87u64);
    let n3288: ZW = zw_bits_b(n2441);
    let n3289: ZW = zw_mix1(n3277, n3288, 38u64);
    let n3290: ZW = zw_mix2(n3278, n3288, 38u64);
    let n3291: ZW = zw_bits_n(n2446);
    let n3292: ZW = zw_mix1(n3289, n3291, 39u64);
    let n3293: ZW = zw_mix2(n3290, n3291, 39u64);
    let n3294: ZW = zw_bits_n(n2445);
    let n3295: ZW = zw_mix1(n3292, n3294, 87u64);
    let n3296: ZW = zw_mix2(n3293, n3294, 87u64);
    let n3297: ZW = zw_bits_b(n2450);
    let n3298: ZW = zw_mix1(n3277, n3297, 38u64);
    let n3299: ZW = zw_mix2(n3278, n3297, 38u64);
    let n3300: ZW = zw_bits_n(n2455);
    let n3301: ZW = zw_mix1(n3298, n3300, 39u64);
    let n3302: ZW = zw_mix2(n3299, n3300, 39u64);
    let n3303: ZW = zw_bits_n(n2454);
    let n3304: ZW = zw_mix1(n3301, n3303, 87u64);
    let n3305: ZW = zw_mix2(n3302, n3303, 87u64);
    let n3306: ZW = zw_bits_b(n2459);
    let n3307: ZW = zw_mix1(n3277, n3306, 38u64);
    let n3308: ZW = zw_mix2(n3278, n3306, 38u64);
    let n3309: ZW = zw_bits_n(n2464);
    let n3310: ZW = zw_mix1(n3307, n3309, 39u64);
    let n3311: ZW = zw_mix2(n3308, n3309, 39u64);
    let n3312: ZW = zw_bits_n(n2463);
    let n3313: ZW = zw_mix1(n3310, n3312, 87u64);
    let n3314: ZW = zw_mix2(n3311, n3312, 87u64);
    let n3315: ZW = zw_bits_b(n2468);
    let n3316: ZW = zw_mix1(n3277, n3315, 38u64);
    let n3317: ZW = zw_mix2(n3278, n3315, 38u64);
    let n3318: ZW = zw_bits_n(n2473);
    let n3319: ZW = zw_mix1(n3316, n3318, 39u64);
    let n3320: ZW = zw_mix2(n3317, n3318, 39u64);
    let n3321: ZW = zw_bits_n(n2472);
    let n3322: ZW = zw_mix1(n3319, n3321, 87u64);
    let n3323: ZW = zw_mix2(n3320, n3321, 87u64);
    let n3324: ZW = zw_bits_b(n2477);
    let n3325: ZW = zw_mix1(n3277, n3324, 38u64);
    let n3326: ZW = zw_mix2(n3278, n3324, 38u64);
    let n3327: ZW = zw_bits_n(n2482);
    let n3328: ZW = zw_mix1(n3325, n3327, 39u64);
    let n3329: ZW = zw_mix2(n3326, n3327, 39u64);
    let n3330: ZW = zw_bits_n(n2481);
    let n3331: ZW = zw_mix1(n3328, n3330, 87u64);
    let n3332: ZW = zw_mix2(n3329, n3330, 87u64);
    let n3333: ZW = zw_mix1(n3258, n2991, 20u64);
    let n3334: ZW = zw_mix2(n3259, n2991, 20u64);
    let n3335: ZW = zw_bits_b(n2486);
    let n3336: ZW = zw_mix1(n3333, n3335, 38u64);
    let n3337: ZW = zw_mix2(n3334, n3335, 38u64);
    let n3338: ZW = zw_bits_n(n2493);
    let n3339: ZW = zw_mix1(n3336, n3338, 39u64);
    let n3340: ZW = zw_mix2(n3337, n3338, 39u64);
    let n3341: ZW = zw_bits_n(n2492);
    let n3342: ZW = zw_mix1(n3339, n3341, 87u64);
    let n3343: ZW = zw_mix2(n3340, n3341, 87u64);
    let n3344: ZW = zw_bits_b(n2497);
    let n3345: ZW = zw_mix1(n3333, n3344, 38u64);
    let n3346: ZW = zw_mix2(n3334, n3344, 38u64);
    let n3347: ZW = zw_bits_n(n2504);
    let n3348: ZW = zw_mix1(n3345, n3347, 39u64);
    let n3349: ZW = zw_mix2(n3346, n3347, 39u64);
    let n3350: ZW = zw_bits_n(n2503);
    let n3351: ZW = zw_mix1(n3348, n3350, 87u64);
    let n3352: ZW = zw_mix2(n3349, n3350, 87u64);
    let n3353: ZW = zw_bits_b(n2508);
    let n3354: ZW = zw_mix1(n3333, n3353, 38u64);
    let n3355: ZW = zw_mix2(n3334, n3353, 38u64);
    let n3356: ZW = zw_bits_n(n2515);
    let n3357: ZW = zw_mix1(n3354, n3356, 39u64);
    let n3358: ZW = zw_mix2(n3355, n3356, 39u64);
    let n3359: ZW = zw_bits_n(n2514);
    let n3360: ZW = zw_mix1(n3357, n3359, 87u64);
    let n3361: ZW = zw_mix2(n3358, n3359, 87u64);
    let n3362: ZW = zw_bits_b(n2519);
    let n3363: ZW = zw_mix1(n3333, n3362, 38u64);
    let n3364: ZW = zw_mix2(n3334, n3362, 38u64);
    let n3365: ZW = zw_bits_n(n2526);
    let n3366: ZW = zw_mix1(n3363, n3365, 39u64);
    let n3367: ZW = zw_mix2(n3364, n3365, 39u64);
    let n3368: ZW = zw_bits_n(n2525);
    let n3369: ZW = zw_mix1(n3366, n3368, 87u64);
    let n3370: ZW = zw_mix2(n3367, n3368, 87u64);
    let n3371: ZW = zw_bits_b(n2530);
    let n3372: ZW = zw_mix1(n3333, n3371, 38u64);
    let n3373: ZW = zw_mix2(n3334, n3371, 38u64);
    let n3374: ZW = zw_bits_n(n2537);
    let n3375: ZW = zw_mix1(n3372, n3374, 39u64);
    let n3376: ZW = zw_mix2(n3373, n3374, 39u64);
    let n3377: ZW = zw_bits_n(n2536);
    let n3378: ZW = zw_mix1(n3375, n3377, 87u64);
    let n3379: ZW = zw_mix2(n3376, n3377, 87u64);
    let n3380: ZW = zw_bits_b(n2541);
    let n3381: ZW = zw_mix1(n3333, n3380, 38u64);
    let n3382: ZW = zw_mix2(n3334, n3380, 38u64);
    let n3383: ZW = zw_bits_n(n2548);
    let n3384: ZW = zw_mix1(n3381, n3383, 39u64);
    let n3385: ZW = zw_mix2(n3382, n3383, 39u64);
    let n3386: ZW = zw_bits_n(n2547);
    let n3387: ZW = zw_mix1(n3384, n3386, 87u64);
    let n3388: ZW = zw_mix2(n3385, n3386, 87u64);
    let n3389: ZW = zw_bits_b(n2552);
    let n3390: ZW = zw_mix1(n3333, n3389, 38u64);
    let n3391: ZW = zw_mix2(n3334, n3389, 38u64);
    let n3392: ZW = zw_bits_n(n2559);
    let n3393: ZW = zw_mix1(n3390, n3392, 39u64);
    let n3394: ZW = zw_mix2(n3391, n3392, 39u64);
    let n3395: ZW = zw_bits_n(n2558);
    let n3396: ZW = zw_mix1(n3393, n3395, 87u64);
    let n3397: ZW = zw_mix2(n3394, n3395, 87u64);
    let n3398: ZW = zw_bits_b(n2563);
    let n3399: ZW = zw_mix1(n3333, n3398, 38u64);
    let n3400: ZW = zw_mix2(n3334, n3398, 38u64);
    let n3401: ZW = zw_bits_n(n2570);
    let n3402: ZW = zw_mix1(n3399, n3401, 39u64);
    let n3403: ZW = zw_mix2(n3400, n3401, 39u64);
    let n3404: ZW = zw_bits_n(n2569);
    let n3405: ZW = zw_mix1(n3402, n3404, 87u64);
    let n3406: ZW = zw_mix2(n3403, n3404, 87u64);
    let n3407: ZW = zw_bits_n(n2584);
    let n3408: ZW = zw_mix1(n2876, n3407, 256u64);
    let n3409: ZW = zw_mix2(n2877, n3407, 256u64);
    let n3410: ZW = zw_bits_n(n2585);
    let n3411: ZW = zw_mix1(n3408, n3410, 267u64);
    let n3412: ZW = zw_mix2(n3409, n3410, 267u64);
    let n3413: ZW = zw_bits_n(n2586);
    let n3414: ZW = zw_mix1(n3411, n3413, 275u64);
    let n3415: ZW = zw_mix2(n3412, n3413, 275u64);
    let n3416: ZW = zw_bits_n(n2588);
    let n3417: ZW = zw_mix1(n3414, n3416, 310u64);
    let n3418: ZW = zw_mix2(n3415, n3416, 310u64);
    let n3419: ZW = zw_bits_n(n2589);
    let n3420: ZW = zw_mix1(n3417, n3419, 311u64);
    let n3421: ZW = zw_mix2(n3418, n3419, 311u64);
    let n3422: ZW = zw_bits_n(n2576);
    let n3423: ZW = zw_mix1(n3420, n3422, 20u64);
    let n3424: ZW = zw_mix2(n3421, n3422, 20u64);
    let n3425: ZW = zw_mix1(n3423, n2890, 41u64);
    let n3426: ZW = zw_mix2(n3424, n2890, 41u64);
    let n3427: ZW = zw_bits_n(n2577);
    let n3428: ZW = zw_mix1(n3425, n3427, 236u64);
    let n3429: ZW = zw_mix2(n3426, n3427, 236u64);
    let n3430: ZW = zw_bits_n(n2578);
    let n3431: ZW = zw_mix1(n3428, n3430, 238u64);
    let n3432: ZW = zw_mix2(n3429, n3430, 238u64);
    let n3433: ZW = zw_bits_n(n2579);
    let n3434: ZW = zw_mix1(n3431, n3433, 239u64);
    let n3435: ZW = zw_mix2(n3432, n3433, 239u64);
    let n3436: ZW = zw_bits_n(n2580);
    let n3437: ZW = zw_mix1(n3434, n3436, 241u64);
    let n3438: ZW = zw_mix2(n3435, n3436, 241u64);
    let n3439: ZW = zw_bits_b(n2581);
    let n3440: ZW = zw_mix1(n3437, n3439, 248u64);
    let n3441: ZW = zw_mix2(n3438, n3439, 248u64);
    let n3442: ZW = zw_bits_b(n2582);
    let n3443: ZW = zw_mix1(n3440, n3442, 249u64);
    let n3444: ZW = zw_mix2(n3441, n3442, 249u64);
    let n3445: ZW = zw_bits_n(n2613);
    let n3446: ZW = zw_mix1(n3443, n3445, 255u64);
    let n3447: ZW = zw_mix2(n3444, n3445, 255u64);
    let n3448: ZW = zw_mix1(n3446, n2910, 300u64);
    let n3449: ZW = zw_mix2(n3447, n2910, 300u64);
    let n3450: ZW = zw_mix1(n3448, n2913, 301u64);
    let n3451: ZW = zw_mix2(n3449, n2913, 301u64);
    let n3452: ZW = zw_mix1(n3450, n2916, 302u64);
    let n3453: ZW = zw_mix2(n3451, n2916, 302u64);
    let n3454: ZW = zw_mix1(n3452, n2919, 303u64);
    let n3455: ZW = zw_mix2(n3453, n2919, 303u64);
    let n3456: ZW = zw_bits_b(n2587);
    let n3457: ZW = zw_mix1(n3454, n3456, 304u64);
    let n3458: ZW = zw_mix2(n3455, n3456, 304u64);
    let n3459: ZW = zw_bits_n(n2614);
    let n3460: ZW = zw_mix1(n3457, n3459, 312u64);
    let n3461: ZW = zw_mix2(n3458, n3459, 312u64);
    let n3462: ZW = zw_bits_n(n2591);
    let n3463: ZW = zw_mix1(n3460, n3462, 313u64);
    let n3464: ZW = zw_mix2(n3461, n3462, 313u64);
    let n3465: ZW = zw_bits_b(n2618);
    let n3466: ZW = zw_mix1(n3454, n3465, 304u64);
    let n3467: ZW = zw_mix2(n3455, n3465, 304u64);
    let n3468: ZW = zw_bits_n(n2631);
    let n3469: ZW = zw_mix1(n3466, n3468, 312u64);
    let n3470: ZW = zw_mix2(n3467, n3468, 312u64);
    let n3471: ZW = zw_bits_n(n2620);
    let n3472: ZW = zw_mix1(n3469, n3471, 313u64);
    let n3473: ZW = zw_mix2(n3470, n3471, 313u64);
    let n3474: ZW = zw_bits_b(n2634);
    let n3475: ZW = zw_mix1(n3454, n3474, 304u64);
    let n3476: ZW = zw_mix2(n3455, n3474, 304u64);
    let n3477: ZW = zw_bits_n(n2647);
    let n3478: ZW = zw_mix1(n3475, n3477, 312u64);
    let n3479: ZW = zw_mix2(n3476, n3477, 312u64);
    let n3480: ZW = zw_bits_n(n2636);
    let n3481: ZW = zw_mix1(n3478, n3480, 313u64);
    let n3482: ZW = zw_mix2(n3479, n3480, 313u64);
    let n3483: ZW = zw_bits_n(n2650);
    let n3484: ZW = zw_mix1(n3434, n3483, 241u64);
    let n3485: ZW = zw_mix2(n3435, n3483, 241u64);
    let n3486: ZW = zw_mix1(n3484, n3439, 248u64);
    let n3487: ZW = zw_mix2(n3485, n3439, 248u64);
    let n3488: ZW = zw_bits_b(n2651);
    let n3489: ZW = zw_mix1(n3486, n3488, 249u64);
    let n3490: ZW = zw_mix2(n3487, n3488, 249u64);
    let n3491: ZW = zw_mix1(n3489, n3445, 255u64);
    let n3492: ZW = zw_mix2(n3490, n3445, 255u64);
    let n3493: ZW = zw_mix1(n3491, n2910, 300u64);
    let n3494: ZW = zw_mix2(n3492, n2910, 300u64);
    let n3495: ZW = zw_mix1(n3493, n2913, 301u64);
    let n3496: ZW = zw_mix2(n3494, n2913, 301u64);
    let n3497: ZW = zw_mix1(n3495, n2916, 302u64);
    let n3498: ZW = zw_mix2(n3496, n2916, 302u64);
    let n3499: ZW = zw_mix1(n3497, n2919, 303u64);
    let n3500: ZW = zw_mix2(n3498, n2919, 303u64);
    let n3501: ZW = zw_mix1(n3499, n3456, 304u64);
    let n3502: ZW = zw_mix2(n3500, n3456, 304u64);
    let n3503: ZW = zw_bits_n(n2664);
    let n3504: ZW = zw_mix1(n3501, n3503, 312u64);
    let n3505: ZW = zw_mix2(n3502, n3503, 312u64);
    let n3506: ZW = zw_bits_n(n2653);
    let n3507: ZW = zw_mix1(n3504, n3506, 313u64);
    let n3508: ZW = zw_mix2(n3505, n3506, 313u64);
    let n3509: ZW = zw_mix1(n3499, n3465, 304u64);
    let n3510: ZW = zw_mix2(n3500, n3465, 304u64);
    let n3511: ZW = zw_bits_n(n2679);
    let n3512: ZW = zw_mix1(n3509, n3511, 312u64);
    let n3513: ZW = zw_mix2(n3510, n3511, 312u64);
    let n3514: ZW = zw_bits_n(n2668);
    let n3515: ZW = zw_mix1(n3512, n3514, 313u64);
    let n3516: ZW = zw_mix2(n3513, n3514, 313u64);
    let n3517: ZW = zw_mix1(n3499, n3474, 304u64);
    let n3518: ZW = zw_mix2(n3500, n3474, 304u64);
    let n3519: ZW = zw_bits_n(n2694);
    let n3520: ZW = zw_mix1(n3517, n3519, 312u64);
    let n3521: ZW = zw_mix2(n3518, n3519, 312u64);
    let n3522: ZW = zw_bits_n(n2683);
    let n3523: ZW = zw_mix1(n3520, n3522, 313u64);
    let n3524: ZW = zw_mix2(n3521, n3522, 313u64);
    let n3525: ZW = zw_bits_n(n2699);
    let n3526: ZW = zw_mix1(n3420, n3525, 20u64);
    let n3527: ZW = zw_mix2(n3421, n3525, 20u64);
    let n3528: ZW = zw_bits_b(n2700);
    let n3529: ZW = zw_mix1(n3526, n3528, 41u64);
    let n3530: ZW = zw_mix2(n3527, n3528, 41u64);
    let n3531: ZW = zw_bits_n(n2701);
    let n3532: ZW = zw_mix1(n3529, n3531, 236u64);
    let n3533: ZW = zw_mix2(n3530, n3531, 236u64);
    let n3534: ZW = zw_bits_n(n2702);
    let n3535: ZW = zw_mix1(n3532, n3534, 238u64);
    let n3536: ZW = zw_mix2(n3533, n3534, 238u64);
    let n3537: ZW = zw_bits_n(n2703);
    let n3538: ZW = zw_mix1(n3535, n3537, 239u64);
    let n3539: ZW = zw_mix2(n3536, n3537, 239u64);
    let n3540: ZW = zw_mix1(n3538, n3436, 241u64);
    let n3541: ZW = zw_mix2(n3539, n3436, 241u64);
    let n3542: ZW = zw_bits_b(n2704);
    let n3543: ZW = zw_mix1(n3540, n3542, 248u64);
    let n3544: ZW = zw_mix2(n3541, n3542, 248u64);
    let n3545: ZW = zw_mix1(n3543, n3442, 249u64);
    let n3546: ZW = zw_mix2(n3544, n3442, 249u64);
    let n3547: ZW = zw_bits_n(n2723);
    let n3548: ZW = zw_mix1(n3545, n3547, 255u64);
    let n3549: ZW = zw_mix2(n3546, n3547, 255u64);
    let n3550: ZW = zw_bits_n(n2705);
    let n3551: ZW = zw_mix1(n3548, n3550, 300u64);
    let n3552: ZW = zw_mix2(n3549, n3550, 300u64);
    let n3553: ZW = zw_bits_n(n2706);
    let n3554: ZW = zw_mix1(n3551, n3553, 301u64);
    let n3555: ZW = zw_mix2(n3552, n3553, 301u64);
    let n3556: ZW = zw_bits_n(n2707);
    let n3557: ZW = zw_mix1(n3554, n3556, 302u64);
    let n3558: ZW = zw_mix2(n3555, n3556, 302u64);
    let n3559: ZW = zw_bits_n(n2708);
    let n3560: ZW = zw_mix1(n3557, n3559, 303u64);
    let n3561: ZW = zw_mix2(n3558, n3559, 303u64);
    let n3562: ZW = zw_mix1(n3560, n3456, 304u64);
    let n3563: ZW = zw_mix2(n3561, n3456, 304u64);
    let n3564: ZW = zw_bits_n(n2724);
    let n3565: ZW = zw_mix1(n3562, n3564, 312u64);
    let n3566: ZW = zw_mix2(n3563, n3564, 312u64);
    let n3567: ZW = zw_bits_n(n2710);
    let n3568: ZW = zw_mix1(n3565, n3567, 313u64);
    let n3569: ZW = zw_mix2(n3566, n3567, 313u64);
    let n3570: ZW = zw_bits_n(n2727);
    let n3571: ZW = zw_mix1(n3551, n3570, 301u64);
    let n3572: ZW = zw_mix2(n3552, n3570, 301u64);
    let n3573: ZW = zw_bits_n(n2728);
    let n3574: ZW = zw_mix1(n3571, n3573, 302u64);
    let n3575: ZW = zw_mix2(n3572, n3573, 302u64);
    let n3576: ZW = zw_mix1(n3574, n3559, 303u64);
    let n3577: ZW = zw_mix2(n3575, n3559, 303u64);
    let n3578: ZW = zw_mix1(n3576, n3465, 304u64);
    let n3579: ZW = zw_mix2(n3577, n3465, 304u64);
    let n3580: ZW = zw_bits_n(n2741);
    let n3581: ZW = zw_mix1(n3578, n3580, 312u64);
    let n3582: ZW = zw_mix2(n3579, n3580, 312u64);
    let n3583: ZW = zw_bits_n(n2730);
    let n3584: ZW = zw_mix1(n3581, n3583, 313u64);
    let n3585: ZW = zw_mix2(n3582, n3583, 313u64);
    let n3586: ZW = zw_bits_n(n2744);
    let n3587: ZW = zw_mix1(n3571, n3586, 302u64);
    let n3588: ZW = zw_mix2(n3572, n3586, 302u64);
    let n3589: ZW = zw_mix1(n3587, n3559, 303u64);
    let n3590: ZW = zw_mix2(n3588, n3559, 303u64);
    let n3591: ZW = zw_mix1(n3589, n3474, 304u64);
    let n3592: ZW = zw_mix2(n3590, n3474, 304u64);
    let n3593: ZW = zw_bits_n(n2757);
    let n3594: ZW = zw_mix1(n3591, n3593, 312u64);
    let n3595: ZW = zw_mix2(n3592, n3593, 312u64);
    let n3596: ZW = zw_bits_n(n2746);
    let n3597: ZW = zw_mix1(n3594, n3596, 313u64);
    let n3598: ZW = zw_mix2(n3595, n3596, 313u64);
    let n3599: ZW = zw_bits_n(n2760);
    let n3600: ZW = zw_mix1(n3548, n3599, 300u64);
    let n3601: ZW = zw_mix2(n3549, n3599, 300u64);
    let n3602: ZW = zw_bits_n(n2761);
    let n3603: ZW = zw_mix1(n3600, n3602, 301u64);
    let n3604: ZW = zw_mix2(n3601, n3602, 301u64);
    let n3605: ZW = zw_bits_n(n2762);
    let n3606: ZW = zw_mix1(n3603, n3605, 302u64);
    let n3607: ZW = zw_mix2(n3604, n3605, 302u64);
    let n3608: ZW = zw_bits_n(n2763);
    let n3609: ZW = zw_mix1(n3606, n3608, 303u64);
    let n3610: ZW = zw_mix2(n3607, n3608, 303u64);
    let n3611: ZW = zw_mix1(n3609, n3456, 304u64);
    let n3612: ZW = zw_mix2(n3610, n3456, 304u64);
    let n3613: ZW = zw_bits_n(n2776);
    let n3614: ZW = zw_mix1(n3611, n3613, 312u64);
    let n3615: ZW = zw_mix2(n3612, n3613, 312u64);
    let n3616: ZW = zw_bits_n(n2765);
    let n3617: ZW = zw_mix1(n3614, n3616, 313u64);
    let n3618: ZW = zw_mix2(n3615, n3616, 313u64);
    let n3619: ZW = zw_mix1(n3600, n3570, 301u64);
    let n3620: ZW = zw_mix2(n3601, n3570, 301u64);
    let n3621: ZW = zw_mix1(n3619, n3573, 302u64);
    let n3622: ZW = zw_mix2(n3620, n3573, 302u64);
    let n3623: ZW = zw_mix1(n3621, n3608, 303u64);
    let n3624: ZW = zw_mix2(n3622, n3608, 303u64);
    let n3625: ZW = zw_mix1(n3623, n3465, 304u64);
    let n3626: ZW = zw_mix2(n3624, n3465, 304u64);
    let n3627: ZW = zw_bits_n(n2781);
    let n3628: ZW = zw_mix1(n3625, n3627, 312u64);
    let n3629: ZW = zw_mix2(n3626, n3627, 312u64);
    let n3630: ZW = zw_bits_n(n2779);
    let n3631: ZW = zw_mix1(n3628, n3630, 313u64);
    let n3632: ZW = zw_mix2(n3629, n3630, 313u64);
    let n3633: ZW = zw_mix1(n3619, n3586, 302u64);
    let n3634: ZW = zw_mix2(n3620, n3586, 302u64);
    let n3635: ZW = zw_mix1(n3633, n3608, 303u64);
    let n3636: ZW = zw_mix2(n3634, n3608, 303u64);
    let n3637: ZW = zw_mix1(n3635, n3474, 304u64);
    let n3638: ZW = zw_mix2(n3636, n3474, 304u64);
    let n3639: ZW = zw_bits_n(n2785);
    let n3640: ZW = zw_mix1(n3637, n3639, 312u64);
    let n3641: ZW = zw_mix2(n3638, n3639, 312u64);
    let n3642: ZW = zw_bits_n(n2783);
    let n3643: ZW = zw_mix1(n3640, n3642, 313u64);
    let n3644: ZW = zw_mix2(n3641, n3642, 313u64);
    let n3645: ZW = zw_bits_n(n2786);
    let n3646: ZW = zw_mix1(n3606, n3645, 303u64);
    let n3647: ZW = zw_mix2(n3607, n3645, 303u64);
    let n3648: ZW = zw_mix1(n3646, n3456, 304u64);
    let n3649: ZW = zw_mix2(n3647, n3456, 304u64);
    let n3650: ZW = zw_mix1(n3648, n3613, 312u64);
    let n3651: ZW = zw_mix2(n3649, n3613, 312u64);
    let n3652: ZW = zw_bits_n(n2787);
    let n3653: ZW = zw_mix1(n3650, n3652, 313u64);
    let n3654: ZW = zw_mix2(n3651, n3652, 313u64);
    let n3655: ZW = zw_mix1(n3621, n3645, 303u64);
    let n3656: ZW = zw_mix2(n3622, n3645, 303u64);
    let n3657: ZW = zw_mix1(n3655, n3465, 304u64);
    let n3658: ZW = zw_mix2(n3656, n3465, 304u64);
    let n3659: ZW = zw_mix1(n3657, n3627, 312u64);
    let n3660: ZW = zw_mix2(n3658, n3627, 312u64);
    let n3661: ZW = zw_bits_n(n2788);
    let n3662: ZW = zw_mix1(n3659, n3661, 313u64);
    let n3663: ZW = zw_mix2(n3660, n3661, 313u64);
    let n3664: ZW = zw_mix1(n3633, n3645, 303u64);
    let n3665: ZW = zw_mix2(n3634, n3645, 303u64);
    let n3666: ZW = zw_mix1(n3664, n3474, 304u64);
    let n3667: ZW = zw_mix2(n3665, n3474, 304u64);
    let n3668: ZW = zw_mix1(n3666, n3639, 312u64);
    let n3669: ZW = zw_mix2(n3667, n3639, 312u64);
    let n3670: ZW = zw_bits_n(n2789);
    let n3671: ZW = zw_mix1(n3668, n3670, 313u64);
    let n3672: ZW = zw_mix2(n3669, n3670, 313u64);
    let n3673: ZW = zw_mix1(n3538, n3483, 241u64);
    let n3674: ZW = zw_mix2(n3539, n3483, 241u64);
    let n3675: ZW = zw_mix1(n3673, n3542, 248u64);
    let n3676: ZW = zw_mix2(n3674, n3542, 248u64);
    let n3677: ZW = zw_mix1(n3675, n3488, 249u64);
    let n3678: ZW = zw_mix2(n3676, n3488, 249u64);
    let n3679: ZW = zw_mix1(n3677, n3547, 255u64);
    let n3680: ZW = zw_mix2(n3678, n3547, 255u64);
    let n3681: ZW = zw_mix1(n3679, n3550, 300u64);
    let n3682: ZW = zw_mix2(n3680, n3550, 300u64);
    let n3683: ZW = zw_mix1(n3681, n3553, 301u64);
    let n3684: ZW = zw_mix2(n3682, n3553, 301u64);
    let n3685: ZW = zw_mix1(n3683, n3556, 302u64);
    let n3686: ZW = zw_mix2(n3684, n3556, 302u64);
    let n3687: ZW = zw_mix1(n3685, n3559, 303u64);
    let n3688: ZW = zw_mix2(n3686, n3559, 303u64);
    let n3689: ZW = zw_mix1(n3687, n3456, 304u64);
    let n3690: ZW = zw_mix2(n3688, n3456, 304u64);
    let n3691: ZW = zw_bits_n(n2803);
    let n3692: ZW = zw_mix1(n3689, n3691, 312u64);
    let n3693: ZW = zw_mix2(n3690, n3691, 312u64);
    let n3694: ZW = zw_bits_n(n2792);
    let n3695: ZW = zw_mix1(n3692, n3694, 313u64);
    let n3696: ZW = zw_mix2(n3693, n3694, 313u64);
    let n3697: ZW = zw_mix1(n3681, n3570, 301u64);
    let n3698: ZW = zw_mix2(n3682, n3570, 301u64);
    let n3699: ZW = zw_mix1(n3697, n3573, 302u64);
    let n3700: ZW = zw_mix2(n3698, n3573, 302u64);
    let n3701: ZW = zw_mix1(n3699, n3559, 303u64);
    let n3702: ZW = zw_mix2(n3700, n3559, 303u64);
    let n3703: ZW = zw_mix1(n3701, n3465, 304u64);
    let n3704: ZW = zw_mix2(n3702, n3465, 304u64);
    let n3705: ZW = zw_bits_n(n2818);
    let n3706: ZW = zw_mix1(n3703, n3705, 312u64);
    let n3707: ZW = zw_mix2(n3704, n3705, 312u64);
    let n3708: ZW = zw_bits_n(n2807);
    let n3709: ZW = zw_mix1(n3706, n3708, 313u64);
    let n3710: ZW = zw_mix2(n3707, n3708, 313u64);
    let n3711: ZW = zw_mix1(n3697, n3586, 302u64);
    let n3712: ZW = zw_mix2(n3698, n3586, 302u64);
    let n3713: ZW = zw_mix1(n3711, n3559, 303u64);
    let n3714: ZW = zw_mix2(n3712, n3559, 303u64);
    let n3715: ZW = zw_mix1(n3713, n3474, 304u64);
    let n3716: ZW = zw_mix2(n3714, n3474, 304u64);
    let n3717: ZW = zw_bits_n(n2833);
    let n3718: ZW = zw_mix1(n3715, n3717, 312u64);
    let n3719: ZW = zw_mix2(n3716, n3717, 312u64);
    let n3720: ZW = zw_bits_n(n2822);
    let n3721: ZW = zw_mix1(n3718, n3720, 313u64);
    let n3722: ZW = zw_mix2(n3719, n3720, 313u64);
    let n3723: ZW = zw_mix1(n3679, n3599, 300u64);
    let n3724: ZW = zw_mix2(n3680, n3599, 300u64);
    let n3725: ZW = zw_mix1(n3723, n3602, 301u64);
    let n3726: ZW = zw_mix2(n3724, n3602, 301u64);
    let n3727: ZW = zw_mix1(n3725, n3605, 302u64);
    let n3728: ZW = zw_mix2(n3726, n3605, 302u64);
    let n3729: ZW = zw_mix1(n3727, n3608, 303u64);
    let n3730: ZW = zw_mix2(n3728, n3608, 303u64);
    let n3731: ZW = zw_mix1(n3729, n3456, 304u64);
    let n3732: ZW = zw_mix2(n3730, n3456, 304u64);
    let n3733: ZW = zw_bits_n(n2848);
    let n3734: ZW = zw_mix1(n3731, n3733, 312u64);
    let n3735: ZW = zw_mix2(n3732, n3733, 312u64);
    let n3736: ZW = zw_bits_n(n2837);
    let n3737: ZW = zw_mix1(n3734, n3736, 313u64);
    let n3738: ZW = zw_mix2(n3735, n3736, 313u64);
    let n3739: ZW = zw_mix1(n3723, n3570, 301u64);
    let n3740: ZW = zw_mix2(n3724, n3570, 301u64);
    let n3741: ZW = zw_mix1(n3739, n3573, 302u64);
    let n3742: ZW = zw_mix2(n3740, n3573, 302u64);
    let n3743: ZW = zw_mix1(n3741, n3608, 303u64);
    let n3744: ZW = zw_mix2(n3742, n3608, 303u64);
    let n3745: ZW = zw_mix1(n3743, n3465, 304u64);
    let n3746: ZW = zw_mix2(n3744, n3465, 304u64);
    let n3747: ZW = zw_bits_n(n2853);
    let n3748: ZW = zw_mix1(n3745, n3747, 312u64);
    let n3749: ZW = zw_mix2(n3746, n3747, 312u64);
    let n3750: ZW = zw_bits_n(n2851);
    let n3751: ZW = zw_mix1(n3748, n3750, 313u64);
    let n3752: ZW = zw_mix2(n3749, n3750, 313u64);
    let n3753: ZW = zw_mix1(n3739, n3586, 302u64);
    let n3754: ZW = zw_mix2(n3740, n3586, 302u64);
    let n3755: ZW = zw_mix1(n3753, n3608, 303u64);
    let n3756: ZW = zw_mix2(n3754, n3608, 303u64);
    let n3757: ZW = zw_mix1(n3755, n3474, 304u64);
    let n3758: ZW = zw_mix2(n3756, n3474, 304u64);
    let n3759: ZW = zw_bits_n(n2857);
    let n3760: ZW = zw_mix1(n3757, n3759, 312u64);
    let n3761: ZW = zw_mix2(n3758, n3759, 312u64);
    let n3762: ZW = zw_bits_n(n2855);
    let n3763: ZW = zw_mix1(n3760, n3762, 313u64);
    let n3764: ZW = zw_mix2(n3761, n3762, 313u64);
    let n3765: ZW = zw_mix1(n3727, n3645, 303u64);
    let n3766: ZW = zw_mix2(n3728, n3645, 303u64);
    let n3767: ZW = zw_mix1(n3765, n3456, 304u64);
    let n3768: ZW = zw_mix2(n3766, n3456, 304u64);
    let n3769: ZW = zw_mix1(n3767, n3733, 312u64);
    let n3770: ZW = zw_mix2(n3768, n3733, 312u64);
    let n3771: ZW = zw_bits_n(n2858);
    let n3772: ZW = zw_mix1(n3769, n3771, 313u64);
    let n3773: ZW = zw_mix2(n3770, n3771, 313u64);
    let n3774: ZW = zw_mix1(n3741, n3645, 303u64);
    let n3775: ZW = zw_mix2(n3742, n3645, 303u64);
    let n3776: ZW = zw_mix1(n3774, n3465, 304u64);
    let n3777: ZW = zw_mix2(n3775, n3465, 304u64);
    let n3778: ZW = zw_mix1(n3776, n3747, 312u64);
    let n3779: ZW = zw_mix2(n3777, n3747, 312u64);
    let n3780: ZW = zw_bits_n(n2859);
    let n3781: ZW = zw_mix1(n3778, n3780, 313u64);
    let n3782: ZW = zw_mix2(n3779, n3780, 313u64);
    let n3783: ZW = zw_mix1(n3753, n3645, 303u64);
    let n3784: ZW = zw_mix2(n3754, n3645, 303u64);
    let n3785: ZW = zw_mix1(n3783, n3474, 304u64);
    let n3786: ZW = zw_mix2(n3784, n3474, 304u64);
    let n3787: ZW = zw_mix1(n3785, n3759, 312u64);
    let n3788: ZW = zw_mix2(n3786, n3759, 312u64);
    let n3789: ZW = zw_bits_n(n2860);
    let n3790: ZW = zw_mix1(n3787, n3789, 313u64);
    let n3791: ZW = zw_mix2(n3788, n3789, 313u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v0_b0: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v0_b0: u16 = ALL & zb_holds(n1256);
    let ok_v1_b1: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v1_b1: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v1_b1: u16 = ALL & zb_holds(n1363);
    let ok_v2_b2: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v2_b2: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v2_b2: u16 = ALL & zb_holds(n1443);
    let ok_v16_b3: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v16_b3: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v16_b3: u16 = ALL & zb_holds(n1503);
    let ok_v17_b4: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v17_b4: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v17_b4: u16 = ALL & zb_holds(n1561);
    let ok_v18_b5: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v18_b5: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v18_b5: u16 = ALL & zb_holds(n1619);
    let ok_v32_b6: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v32_b6: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v32_b6: u16 = ALL & zb_holds(n1686);
    let ok_v33_b7: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v33_b7: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v33_b7: u16 = ALL & zb_holds(n1723);
    let ok_v34_b8: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v34_b8: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v34_b8: u16 = ALL & zb_holds(n1758);
    let ok_v36_b9: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v36_b9: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v36_b9: u16 = ALL & zb_holds(n1798);
    let ok_v37_b10: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v37_b10: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v37_b10: u16 = ALL & zb_holds(n1723);
    let ok_v38_b11: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v38_b11: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v38_b11: u16 = ALL & zb_holds(n1758);
    let ok_v40_b12: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v40_b12: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v40_b12: u16 = ALL & zb_holds(n1798);
    let ok_v41_b13: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v41_b13: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v41_b13: u16 = ALL & zb_holds(n1723);
    let ok_v42_b14: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v42_b14: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v42_b14: u16 = ALL & zb_holds(n1758);
    let ok_v48_b15: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v48_b15: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v48_b15: u16 = ALL & zb_holds(n1863);
    let ok_v49_b16: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v49_b16: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v49_b16: u16 = ALL & zb_holds(n1896);
    let ok_v50_b17: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v50_b17: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v50_b17: u16 = ALL & zb_holds(n1929);
    let ok_v52_b18: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v52_b18: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v52_b18: u16 = ALL & zb_holds(n1960);
    let ok_v53_b19: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v53_b19: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v53_b19: u16 = ALL & zb_holds(n1896);
    let ok_v54_b20: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v54_b20: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v54_b20: u16 = ALL & zb_holds(n1929);
    let ok_v56_b21: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v56_b21: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v56_b21: u16 = ALL & zb_holds(n1960);
    let ok_v57_b22: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v57_b22: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v57_b22: u16 = ALL & zb_holds(n1896);
    let ok_v58_b23: u16 = ALL & zb_holds(n1032) & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111);
    let bd_v58_b23: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v58_b23: u16 = ALL & zb_holds(n1929);
    let ok_v0_b24: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2061);
    let bd_v0_b24: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v0_b24: u16 = ALL & zb_holds(n99) & zb_holds(n1211) & zb_holds(n2125);
    let ok_v1_b25: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2061);
    let bd_v1_b25: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v1_b25: u16 = ALL & zb_holds(n99) & zb_holds(n1211) & zb_holds(n2172);
    let ok_v2_b26: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2061);
    let bd_v2_b26: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v2_b26: u16 = ALL & zb_holds(n99) & zb_holds(n1211) & zb_holds(n2209);
    let ok_v16_b27: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2061);
    let bd_v16_b27: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v16_b27: u16 = ALL & zb_holds(n99) & zb_holds(n1211) & zb_holds(n2245);
    let ok_v17_b28: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2061);
    let bd_v17_b28: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v17_b28: u16 = ALL & zb_holds(n99) & zb_holds(n1211) & zb_holds(n2281);
    let ok_v18_b29: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2061);
    let bd_v18_b29: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v18_b29: u16 = ALL & zb_holds(n99) & zb_holds(n1211) & zb_holds(n2317);
    let ok_v32_b30: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2061);
    let bd_v32_b30: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v32_b30: u16 = ALL & zb_holds(n2342);
    let ok_v33_b31: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2061);
    let bd_v33_b31: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v33_b31: u16 = ALL & zb_holds(n2353);
    let ok_v34_b32: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2061);
    let bd_v34_b32: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v34_b32: u16 = ALL & zb_holds(n2364);
    let ok_v36_b33: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2061);
    let bd_v36_b33: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v36_b33: u16 = ALL & zb_holds(n2373);
    let ok_v48_b34: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2061);
    let bd_v48_b34: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v48_b34: u16 = ALL & zb_holds(n2396);
    let ok_v49_b35: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2061);
    let bd_v49_b35: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v49_b35: u16 = ALL & zb_holds(n2407);
    let ok_v50_b36: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2061);
    let bd_v50_b36: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v50_b36: u16 = ALL & zb_holds(n2418);
    let ok_v52_b37: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2061);
    let bd_v52_b37: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v52_b37: u16 = ALL & zb_holds(n2427);
    let ok_v0_b38: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2434);
    let bd_v0_b38: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v0_b38: u16 = ALL & zb_holds(n99) & zb_holds(n2433);
    let ok_v1_b39: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2443);
    let bd_v1_b39: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v1_b39: u16 = ALL & zb_holds(n99) & zb_holds(n2442);
    let ok_v2_b40: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2452);
    let bd_v2_b40: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v2_b40: u16 = ALL & zb_holds(n99) & zb_holds(n2451);
    let ok_v16_b41: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2461);
    let bd_v16_b41: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v16_b41: u16 = ALL & zb_holds(n99) & zb_holds(n2460);
    let ok_v17_b42: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2470);
    let bd_v17_b42: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v17_b42: u16 = ALL & zb_holds(n99) & zb_holds(n2469);
    let ok_v18_b43: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2479);
    let bd_v18_b43: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v18_b43: u16 = ALL & zb_holds(n99) & zb_holds(n2478);
    let ok_v32_b44: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2488);
    let bd_v32_b44: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v32_b44: u16 = ALL & zb_holds(n2491);
    let ok_v33_b45: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2499);
    let bd_v33_b45: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v33_b45: u16 = ALL & zb_holds(n2502);
    let ok_v34_b46: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2510);
    let bd_v34_b46: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v34_b46: u16 = ALL & zb_holds(n2513);
    let ok_v36_b47: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2521);
    let bd_v36_b47: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v36_b47: u16 = ALL & zb_holds(n2524);
    let ok_v48_b48: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2532);
    let bd_v48_b48: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v48_b48: u16 = ALL & zb_holds(n2535);
    let ok_v49_b49: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2543);
    let bd_v49_b49: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v49_b49: u16 = ALL & zb_holds(n2546);
    let ok_v50_b50: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2554);
    let bd_v50_b50: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v50_b50: u16 = ALL & zb_holds(n2557);
    let ok_v52_b51: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2565);
    let bd_v52_b51: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v52_b51: u16 = ALL & zb_holds(n2568);
    let ok_v0_b52: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v0_b52: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v0_b52: u16 = ALL & zb_holds(n2615);
    let ok_v1_b53: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v1_b53: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v1_b53: u16 = ALL & zb_holds(n2632);
    let ok_v2_b54: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v2_b54: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v2_b54: u16 = ALL & zb_holds(n2648);
    let ok_v16_b55: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v16_b55: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v16_b55: u16 = ALL & zb_holds(n2665);
    let ok_v17_b56: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v17_b56: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v17_b56: u16 = ALL & zb_holds(n2680);
    let ok_v18_b57: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v18_b57: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v18_b57: u16 = ALL & zb_holds(n2695);
    let ok_v32_b58: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v32_b58: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v32_b58: u16 = ALL & zb_holds(n2725);
    let ok_v33_b59: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v33_b59: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v33_b59: u16 = ALL & zb_holds(n2742);
    let ok_v34_b60: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v34_b60: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v34_b60: u16 = ALL & zb_holds(n2758);
    let ok_v36_b61: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v36_b61: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v36_b61: u16 = ALL & zb_holds(n2777);
    let ok_v37_b62: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v37_b62: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v37_b62: u16 = ALL & zb_holds(n2742);
    let ok_v38_b63: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v38_b63: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v38_b63: u16 = ALL & zb_holds(n2758);
    let ok_v40_b64: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v40_b64: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v40_b64: u16 = ALL & zb_holds(n2777);
    let ok_v41_b65: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v41_b65: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v41_b65: u16 = ALL & zb_holds(n2742);
    let ok_v42_b66: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v42_b66: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v42_b66: u16 = ALL & zb_holds(n2758);
    let ok_v48_b67: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v48_b67: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v48_b67: u16 = ALL & zb_holds(n2804);
    let ok_v49_b68: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v49_b68: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v49_b68: u16 = ALL & zb_holds(n2819);
    let ok_v50_b69: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v50_b69: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v50_b69: u16 = ALL & zb_holds(n2834);
    let ok_v52_b70: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v52_b70: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v52_b70: u16 = ALL & zb_holds(n2849);
    let ok_v53_b71: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v53_b71: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v53_b71: u16 = ALL & zb_holds(n2819);
    let ok_v54_b72: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v54_b72: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v54_b72: u16 = ALL & zb_holds(n2834);
    let ok_v56_b73: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v56_b73: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v56_b73: u16 = ALL & zb_holds(n2849);
    let ok_v57_b74: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v57_b74: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v57_b74: u16 = ALL & zb_holds(n2819);
    let ok_v58_b75: u16 = ALL & zb_holds(n125) & zb_holds(n107) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n108) & zb_holds(n75) & zb_holds(r_c269) & zb_holds(n121) & zb_holds(n120) & zb_holds(n116) & zb_holds(n73) & zb_holds(r_c259) & zb_holds(r_c251) & zb_holds(r_c234) & zb_holds(n70) & zb_holds(n71) & zb_holds(n111) & zb_holds(n2593);
    let bd_v58_b75: bool = !n119 || !n74 || !n118 || !n117 || !n115 || !n114 || !n113 || !n112;
    let live_v58_b75: u16 = ALL & zb_holds(n2834);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n80,
        c86: n130,
        c280: n363,
        c281: n364,
        c256: n362,
        c85: n131,
    };
    let sh1 = KShared1 {
        c87: n2136,
        c84: n80,
        c86: n130,
        c241: n2127,
        c249: n2131,
        c85: n131,
    };
    let sh2 = KShared2 {
        c84: n80,
        c86: n130,
        c85: n131,
    };
    let sh3 = KShared3 {
        c87: r_c87,
        c39: r_c39,
        c84: n80,
        c86: n130,
        c310: n2588,
        c311: n2589,
        c256: n2584,
        c267: n2585,
        c275: n2586,
        c85: n131,
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
    // 76 distinct button assignments; per outcome they fall
    // into [24, 2, 14, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c300,
        c271: r_c301,
        c236: n1064,
        c272: r_c302,
        c273: r_c303,
        c238: n1205,
        c274: n1206,
        c241: n1062,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n1258,
        c283: n1208,
        c255: n1257,
        h1: n2929, h2: n2930,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c300,
        c271: r_c301,
        c236: n1064,
        c272: r_c302,
        c273: r_c303,
        c238: n1205,
        c274: n1338,
        c241: n1062,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n1364,
        c283: n1340,
        c255: n1257,
        h1: n2938, h2: n2939,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c300,
        c271: r_c301,
        c236: n1064,
        c272: r_c302,
        c273: r_c303,
        c238: n1205,
        c274: n1418,
        c241: n1062,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n1444,
        c283: n1420,
        c255: n1257,
        h1: n2947, h2: n2948,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c300,
        c271: r_c301,
        c236: n1064,
        c272: r_c302,
        c273: r_c303,
        c238: n1205,
        c274: n1206,
        c241: n1478,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n1504,
        c283: n1480,
        c255: n1257,
        h1: n2973, h2: n2974,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c300,
        c271: r_c301,
        c236: n1064,
        c272: r_c302,
        c273: r_c303,
        c238: n1205,
        c274: n1338,
        c241: n1478,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n1562,
        c283: n1538,
        c255: n1257,
        h1: n2981, h2: n2982,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c270: r_c300,
        c271: r_c301,
        c236: n1064,
        c272: r_c302,
        c273: r_c303,
        c238: n1205,
        c274: n1418,
        c241: n1478,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n1620,
        c283: n1596,
        c255: n1257,
        h1: n2989, h2: n2990,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1652,
        c271: n1653,
        c236: n1650,
        c272: n1654,
        c273: n1655,
        c238: n1651,
        c274: n1206,
        c241: n1062,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1685,
        c283: n1657,
        c255: n1684,
        h1: n3030, h2: n3031,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1652,
        c271: n1694,
        c236: n1650,
        c272: n1695,
        c273: n1655,
        c238: n1651,
        c274: n1338,
        c241: n1062,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1722,
        c283: n1697,
        c255: n1684,
        h1: n3046, h2: n3047,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1652,
        c271: n1694,
        c236: n1650,
        c272: n1730,
        c273: n1655,
        c238: n1651,
        c274: n1418,
        c241: n1062,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1757,
        c283: n1732,
        c255: n1684,
        h1: n3059, h2: n3060,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1767,
        c271: n1768,
        c236: n1650,
        c272: n1769,
        c273: n1770,
        c238: n1651,
        c274: n1206,
        c241: n1062,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1797,
        c283: n1772,
        c255: n1684,
        h1: n3079, h2: n3080,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1767,
        c271: n1694,
        c236: n1650,
        c272: n1695,
        c273: n1770,
        c238: n1651,
        c274: n1338,
        c241: n1062,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1804,
        c283: n1802,
        c255: n1684,
        h1: n3093, h2: n3094,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1767,
        c271: n1694,
        c236: n1650,
        c272: n1730,
        c273: n1770,
        c238: n1651,
        c274: n1418,
        c241: n1062,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1810,
        c283: n1808,
        c255: n1684,
        h1: n3105, h2: n3106,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1767,
        c271: n1768,
        c236: n1650,
        c272: n1769,
        c273: n1813,
        c238: n1651,
        c274: n1206,
        c241: n1062,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1797,
        c283: n1814,
        c255: n1684,
        h1: n3115, h2: n3116,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1767,
        c271: n1694,
        c236: n1650,
        c272: n1695,
        c273: n1813,
        c238: n1651,
        c274: n1338,
        c241: n1062,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1804,
        c283: n1816,
        c255: n1684,
        h1: n3124, h2: n3125,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1767,
        c271: n1694,
        c236: n1650,
        c272: n1730,
        c273: n1813,
        c238: n1651,
        c274: n1418,
        c241: n1062,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: n1810,
        c283: n1818,
        c255: n1684,
        h1: n3133, h2: n3134,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1652,
        c271: n1653,
        c236: n1650,
        c272: n1654,
        c273: n1655,
        c238: n1651,
        c274: n1206,
        c241: n1478,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1862,
        c283: n1837,
        c255: n1684,
        h1: n3157, h2: n3158,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1652,
        c271: n1694,
        c236: n1650,
        c272: n1695,
        c273: n1655,
        c238: n1651,
        c274: n1338,
        c241: n1478,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1895,
        c283: n1870,
        c255: n1684,
        h1: n3171, h2: n3172,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1652,
        c271: n1694,
        c236: n1650,
        c272: n1730,
        c273: n1655,
        c238: n1651,
        c274: n1418,
        c241: n1478,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1928,
        c283: n1903,
        c255: n1684,
        h1: n3183, h2: n3184,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1767,
        c271: n1768,
        c236: n1650,
        c272: n1769,
        c273: n1770,
        c238: n1651,
        c274: n1206,
        c241: n1478,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1959,
        c283: n1934,
        c255: n1684,
        h1: n3199, h2: n3200,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1767,
        c271: n1694,
        c236: n1650,
        c272: n1695,
        c273: n1770,
        c238: n1651,
        c274: n1338,
        c241: n1478,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1966,
        c283: n1964,
        c255: n1684,
        h1: n3213, h2: n3214,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1767,
        c271: n1694,
        c236: n1650,
        c272: n1730,
        c273: n1770,
        c238: n1651,
        c274: n1418,
        c241: n1478,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1972,
        c283: n1970,
        c255: n1684,
        h1: n3225, h2: n3226,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1767,
        c271: n1768,
        c236: n1650,
        c272: n1769,
        c273: n1813,
        c238: n1651,
        c274: n1206,
        c241: n1478,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1959,
        c283: n1974,
        c255: n1684,
        h1: n3234, h2: n3235,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1767,
        c271: n1694,
        c236: n1650,
        c272: n1695,
        c273: n1813,
        c238: n1651,
        c274: n1338,
        c241: n1478,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1966,
        c283: n1976,
        c255: n1684,
        h1: n3243, h2: n3244,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1648,
        c41: n1649,
        c270: n1767,
        c271: n1694,
        c236: n1650,
        c272: n1730,
        c273: n1813,
        c238: n1651,
        c274: n1418,
        c241: n1478,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: n1972,
        c283: n1978,
        c255: n1684,
        h1: n3252, h2: n3253,
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
        c41: r_c41,
        h1: n3271, h2: n3272,
    };
    // body 29: buttons 0x12, forks 0x0
    sink.o1(18, take_1_0, &sh1, &o1);
    declined |= live_v32_b30 & (if bd_v32_b30 { ALL } else { !ok_v32_b30 });
    take_1_1 |= live_v32_b30 & ok_v32_b30 & (if bd_v32_b30 { 0 } else { ALL });
    declined |= live_v33_b31 & (if bd_v33_b31 { ALL } else { !ok_v33_b31 });
    take_1_1 |= live_v33_b31 & ok_v33_b31 & (if bd_v33_b31 { 0 } else { ALL });
    declined |= live_v34_b32 & (if bd_v34_b32 { ALL } else { !ok_v34_b32 });
    take_1_1 |= live_v34_b32 & ok_v34_b32 & (if bd_v34_b32 { 0 } else { ALL });
    declined |= live_v36_b33 & (if bd_v36_b33 { ALL } else { !ok_v36_b33 });
    take_1_1 |= live_v36_b33 & ok_v36_b33 & (if bd_v36_b33 { 0 } else { ALL });
    declined |= live_v48_b34 & (if bd_v48_b34 { ALL } else { !ok_v48_b34 });
    take_1_1 |= live_v48_b34 & ok_v48_b34 & (if bd_v48_b34 { 0 } else { ALL });
    declined |= live_v49_b35 & (if bd_v49_b35 { ALL } else { !ok_v49_b35 });
    take_1_1 |= live_v49_b35 & ok_v49_b35 & (if bd_v49_b35 { 0 } else { ALL });
    declined |= live_v50_b36 & (if bd_v50_b36 { ALL } else { !ok_v50_b36 });
    take_1_1 |= live_v50_b36 & ok_v50_b36 & (if bd_v50_b36 { 0 } else { ALL });
    declined |= live_v52_b37 & (if bd_v52_b37 { ALL } else { !ok_v52_b37 });
    take_1_1 |= live_v52_b37 & ok_v52_b37 & (if bd_v52_b37 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1648,
        c41: n1649,
        h1: n3275, h2: n3276,
    };
    // body 37: buttons 0x34, forks 0x0
    sink.o1(52, take_1_1, &sh1, &o1);
    declined |= live_v0_b38 & (if bd_v0_b38 { ALL } else { !ok_v0_b38 });
    take_2_0 |= live_v0_b38 & ok_v0_b38 & (if bd_v0_b38 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n2436,
        c39: n2437,
        c20: r_c20,
        c38: n2432,
        h1: n3286, h2: n3287,
    };
    // body 38: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b39 & (if bd_v1_b39 { ALL } else { !ok_v1_b39 });
    take_2_1 |= live_v1_b39 & ok_v1_b39 & (if bd_v1_b39 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n2445,
        c39: n2446,
        c20: r_c20,
        c38: n2441,
        h1: n3295, h2: n3296,
    };
    // body 39: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b40 & (if bd_v2_b40 { ALL } else { !ok_v2_b40 });
    take_2_2 |= live_v2_b40 & ok_v2_b40 & (if bd_v2_b40 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n2454,
        c39: n2455,
        c20: r_c20,
        c38: n2450,
        h1: n3304, h2: n3305,
    };
    // body 40: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b41 & (if bd_v16_b41 { ALL } else { !ok_v16_b41 });
    take_2_3 |= live_v16_b41 & ok_v16_b41 & (if bd_v16_b41 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n2463,
        c39: n2464,
        c20: r_c20,
        c38: n2459,
        h1: n3313, h2: n3314,
    };
    // body 41: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b42 & (if bd_v17_b42 { ALL } else { !ok_v17_b42 });
    take_2_4 |= live_v17_b42 & ok_v17_b42 & (if bd_v17_b42 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n2472,
        c39: n2473,
        c20: r_c20,
        c38: n2468,
        h1: n3322, h2: n3323,
    };
    // body 42: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b43 & (if bd_v18_b43 { ALL } else { !ok_v18_b43 });
    take_2_5 |= live_v18_b43 & ok_v18_b43 & (if bd_v18_b43 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n2481,
        c39: n2482,
        c20: r_c20,
        c38: n2477,
        h1: n3331, h2: n3332,
    };
    // body 43: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b44 & (if bd_v32_b44 { ALL } else { !ok_v32_b44 });
    take_2_6 |= live_v32_b44 & ok_v32_b44 & (if bd_v32_b44 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n2492,
        c39: n2493,
        c20: n1648,
        c38: n2486,
        h1: n3342, h2: n3343,
    };
    // body 44: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b45 & (if bd_v33_b45 { ALL } else { !ok_v33_b45 });
    take_2_7 |= live_v33_b45 & ok_v33_b45 & (if bd_v33_b45 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n2503,
        c39: n2504,
        c20: n1648,
        c38: n2497,
        h1: n3351, h2: n3352,
    };
    // body 45: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b46 & (if bd_v34_b46 { ALL } else { !ok_v34_b46 });
    take_2_8 |= live_v34_b46 & ok_v34_b46 & (if bd_v34_b46 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n2514,
        c39: n2515,
        c20: n1648,
        c38: n2508,
        h1: n3360, h2: n3361,
    };
    // body 46: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v36_b47 & (if bd_v36_b47 { ALL } else { !ok_v36_b47 });
    take_2_9 |= live_v36_b47 & ok_v36_b47 & (if bd_v36_b47 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n2525,
        c39: n2526,
        c20: n1648,
        c38: n2519,
        h1: n3369, h2: n3370,
    };
    // body 47: buttons 0x24, forks 0x0
    sink.o2(36, take_2_9, &sh2, &o2);
    declined |= live_v48_b48 & (if bd_v48_b48 { ALL } else { !ok_v48_b48 });
    take_2_10 |= live_v48_b48 & ok_v48_b48 & (if bd_v48_b48 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n2536,
        c39: n2537,
        c20: n1648,
        c38: n2530,
        h1: n3378, h2: n3379,
    };
    // body 48: buttons 0x30, forks 0x0
    sink.o2(48, take_2_10, &sh2, &o2);
    declined |= live_v49_b49 & (if bd_v49_b49 { ALL } else { !ok_v49_b49 });
    take_2_11 |= live_v49_b49 & ok_v49_b49 & (if bd_v49_b49 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n2547,
        c39: n2548,
        c20: n1648,
        c38: n2541,
        h1: n3387, h2: n3388,
    };
    // body 49: buttons 0x31, forks 0x0
    sink.o2(49, take_2_11, &sh2, &o2);
    declined |= live_v50_b50 & (if bd_v50_b50 { ALL } else { !ok_v50_b50 });
    take_2_12 |= live_v50_b50 & ok_v50_b50 & (if bd_v50_b50 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n2558,
        c39: n2559,
        c20: n1648,
        c38: n2552,
        h1: n3396, h2: n3397,
    };
    // body 50: buttons 0x32, forks 0x0
    sink.o2(50, take_2_12, &sh2, &o2);
    declined |= live_v52_b51 & (if bd_v52_b51 { ALL } else { !ok_v52_b51 });
    take_2_13 |= live_v52_b51 & ok_v52_b51 & (if bd_v52_b51 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n2569,
        c39: n2570,
        c20: n1648,
        c38: n2563,
        h1: n3405, h2: n3406,
    };
    // body 51: buttons 0x34, forks 0x0
    sink.o2(52, take_2_13, &sh2, &o2);
    declined |= live_v0_b52 & (if bd_v0_b52 { ALL } else { !ok_v0_b52 });
    take_3_0 |= live_v0_b52 & ok_v0_b52 & (if bd_v0_b52 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2576,
        c41: r_c41,
        c300: r_c300,
        c301: r_c301,
        c236: n2577,
        c302: r_c302,
        c303: r_c303,
        c238: n2578,
        c239: n2579,
        c304: n2587,
        c241: n2580,
        c248: n2581,
        c249: n2582,
        c312: n2614,
        c313: n2591,
        c255: n2613,
        h1: n3463, h2: n3464,
    };
    // body 52: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined |= live_v1_b53 & (if bd_v1_b53 { ALL } else { !ok_v1_b53 });
    take_3_1 |= live_v1_b53 & ok_v1_b53 & (if bd_v1_b53 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2576,
        c41: r_c41,
        c300: r_c300,
        c301: r_c301,
        c236: n2577,
        c302: r_c302,
        c303: r_c303,
        c238: n2578,
        c239: n2579,
        c304: n2618,
        c241: n2580,
        c248: n2581,
        c249: n2582,
        c312: n2631,
        c313: n2620,
        c255: n2613,
        h1: n3472, h2: n3473,
    };
    // body 53: buttons 0x01, forks 0x0
    sink.o3(1, take_3_1, &sh3, &o3);
    declined |= live_v2_b54 & (if bd_v2_b54 { ALL } else { !ok_v2_b54 });
    take_3_2 |= live_v2_b54 & ok_v2_b54 & (if bd_v2_b54 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2576,
        c41: r_c41,
        c300: r_c300,
        c301: r_c301,
        c236: n2577,
        c302: r_c302,
        c303: r_c303,
        c238: n2578,
        c239: n2579,
        c304: n2634,
        c241: n2580,
        c248: n2581,
        c249: n2582,
        c312: n2647,
        c313: n2636,
        c255: n2613,
        h1: n3481, h2: n3482,
    };
    // body 54: buttons 0x02, forks 0x0
    sink.o3(2, take_3_2, &sh3, &o3);
    declined |= live_v16_b55 & (if bd_v16_b55 { ALL } else { !ok_v16_b55 });
    take_3_3 |= live_v16_b55 & ok_v16_b55 & (if bd_v16_b55 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2576,
        c41: r_c41,
        c300: r_c300,
        c301: r_c301,
        c236: n2577,
        c302: r_c302,
        c303: r_c303,
        c238: n2578,
        c239: n2579,
        c304: n2587,
        c241: n2650,
        c248: n2581,
        c249: n2651,
        c312: n2664,
        c313: n2653,
        c255: n2613,
        h1: n3507, h2: n3508,
    };
    // body 55: buttons 0x10, forks 0x0
    sink.o3(16, take_3_3, &sh3, &o3);
    declined |= live_v17_b56 & (if bd_v17_b56 { ALL } else { !ok_v17_b56 });
    take_3_4 |= live_v17_b56 & ok_v17_b56 & (if bd_v17_b56 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2576,
        c41: r_c41,
        c300: r_c300,
        c301: r_c301,
        c236: n2577,
        c302: r_c302,
        c303: r_c303,
        c238: n2578,
        c239: n2579,
        c304: n2618,
        c241: n2650,
        c248: n2581,
        c249: n2651,
        c312: n2679,
        c313: n2668,
        c255: n2613,
        h1: n3515, h2: n3516,
    };
    // body 56: buttons 0x11, forks 0x0
    sink.o3(17, take_3_4, &sh3, &o3);
    declined |= live_v18_b57 & (if bd_v18_b57 { ALL } else { !ok_v18_b57 });
    take_3_5 |= live_v18_b57 & ok_v18_b57 & (if bd_v18_b57 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2576,
        c41: r_c41,
        c300: r_c300,
        c301: r_c301,
        c236: n2577,
        c302: r_c302,
        c303: r_c303,
        c238: n2578,
        c239: n2579,
        c304: n2634,
        c241: n2650,
        c248: n2581,
        c249: n2651,
        c312: n2694,
        c313: n2683,
        c255: n2613,
        h1: n3523, h2: n3524,
    };
    // body 57: buttons 0x12, forks 0x0
    sink.o3(18, take_3_5, &sh3, &o3);
    declined |= live_v32_b58 & (if bd_v32_b58 { ALL } else { !ok_v32_b58 });
    take_3_6 |= live_v32_b58 & ok_v32_b58 & (if bd_v32_b58 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2705,
        c301: n2706,
        c236: n2701,
        c302: n2707,
        c303: n2708,
        c238: n2702,
        c239: n2703,
        c304: n2587,
        c241: n2580,
        c248: n2704,
        c249: n2582,
        c312: n2724,
        c313: n2710,
        c255: n2723,
        h1: n3568, h2: n3569,
    };
    // body 58: buttons 0x20, forks 0x0
    sink.o3(32, take_3_6, &sh3, &o3);
    declined |= live_v33_b59 & (if bd_v33_b59 { ALL } else { !ok_v33_b59 });
    take_3_7 |= live_v33_b59 & ok_v33_b59 & (if bd_v33_b59 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2705,
        c301: n2727,
        c236: n2701,
        c302: n2728,
        c303: n2708,
        c238: n2702,
        c239: n2703,
        c304: n2618,
        c241: n2580,
        c248: n2704,
        c249: n2582,
        c312: n2741,
        c313: n2730,
        c255: n2723,
        h1: n3584, h2: n3585,
    };
    // body 59: buttons 0x21, forks 0x0
    sink.o3(33, take_3_7, &sh3, &o3);
    declined |= live_v34_b60 & (if bd_v34_b60 { ALL } else { !ok_v34_b60 });
    take_3_8 |= live_v34_b60 & ok_v34_b60 & (if bd_v34_b60 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2705,
        c301: n2727,
        c236: n2701,
        c302: n2744,
        c303: n2708,
        c238: n2702,
        c239: n2703,
        c304: n2634,
        c241: n2580,
        c248: n2704,
        c249: n2582,
        c312: n2757,
        c313: n2746,
        c255: n2723,
        h1: n3597, h2: n3598,
    };
    // body 60: buttons 0x22, forks 0x0
    sink.o3(34, take_3_8, &sh3, &o3);
    declined |= live_v36_b61 & (if bd_v36_b61 { ALL } else { !ok_v36_b61 });
    take_3_9 |= live_v36_b61 & ok_v36_b61 & (if bd_v36_b61 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2760,
        c301: n2761,
        c236: n2701,
        c302: n2762,
        c303: n2763,
        c238: n2702,
        c239: n2703,
        c304: n2587,
        c241: n2580,
        c248: n2704,
        c249: n2582,
        c312: n2776,
        c313: n2765,
        c255: n2723,
        h1: n3617, h2: n3618,
    };
    // body 61: buttons 0x24, forks 0x0
    sink.o3(36, take_3_9, &sh3, &o3);
    declined |= live_v37_b62 & (if bd_v37_b62 { ALL } else { !ok_v37_b62 });
    take_3_10 |= live_v37_b62 & ok_v37_b62 & (if bd_v37_b62 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2760,
        c301: n2727,
        c236: n2701,
        c302: n2728,
        c303: n2763,
        c238: n2702,
        c239: n2703,
        c304: n2618,
        c241: n2580,
        c248: n2704,
        c249: n2582,
        c312: n2781,
        c313: n2779,
        c255: n2723,
        h1: n3631, h2: n3632,
    };
    // body 62: buttons 0x25, forks 0x0
    sink.o3(37, take_3_10, &sh3, &o3);
    declined |= live_v38_b63 & (if bd_v38_b63 { ALL } else { !ok_v38_b63 });
    take_3_11 |= live_v38_b63 & ok_v38_b63 & (if bd_v38_b63 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2760,
        c301: n2727,
        c236: n2701,
        c302: n2744,
        c303: n2763,
        c238: n2702,
        c239: n2703,
        c304: n2634,
        c241: n2580,
        c248: n2704,
        c249: n2582,
        c312: n2785,
        c313: n2783,
        c255: n2723,
        h1: n3643, h2: n3644,
    };
    // body 63: buttons 0x26, forks 0x0
    sink.o3(38, take_3_11, &sh3, &o3);
    declined |= live_v40_b64 & (if bd_v40_b64 { ALL } else { !ok_v40_b64 });
    take_3_12 |= live_v40_b64 & ok_v40_b64 & (if bd_v40_b64 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2760,
        c301: n2761,
        c236: n2701,
        c302: n2762,
        c303: n2786,
        c238: n2702,
        c239: n2703,
        c304: n2587,
        c241: n2580,
        c248: n2704,
        c249: n2582,
        c312: n2776,
        c313: n2787,
        c255: n2723,
        h1: n3653, h2: n3654,
    };
    // body 64: buttons 0x28, forks 0x0
    sink.o3(40, take_3_12, &sh3, &o3);
    declined |= live_v41_b65 & (if bd_v41_b65 { ALL } else { !ok_v41_b65 });
    take_3_13 |= live_v41_b65 & ok_v41_b65 & (if bd_v41_b65 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2760,
        c301: n2727,
        c236: n2701,
        c302: n2728,
        c303: n2786,
        c238: n2702,
        c239: n2703,
        c304: n2618,
        c241: n2580,
        c248: n2704,
        c249: n2582,
        c312: n2781,
        c313: n2788,
        c255: n2723,
        h1: n3662, h2: n3663,
    };
    // body 65: buttons 0x29, forks 0x0
    sink.o3(41, take_3_13, &sh3, &o3);
    declined |= live_v42_b66 & (if bd_v42_b66 { ALL } else { !ok_v42_b66 });
    take_3_14 |= live_v42_b66 & ok_v42_b66 & (if bd_v42_b66 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2760,
        c301: n2727,
        c236: n2701,
        c302: n2744,
        c303: n2786,
        c238: n2702,
        c239: n2703,
        c304: n2634,
        c241: n2580,
        c248: n2704,
        c249: n2582,
        c312: n2785,
        c313: n2789,
        c255: n2723,
        h1: n3671, h2: n3672,
    };
    // body 66: buttons 0x2a, forks 0x0
    sink.o3(42, take_3_14, &sh3, &o3);
    declined |= live_v48_b67 & (if bd_v48_b67 { ALL } else { !ok_v48_b67 });
    take_3_15 |= live_v48_b67 & ok_v48_b67 & (if bd_v48_b67 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2705,
        c301: n2706,
        c236: n2701,
        c302: n2707,
        c303: n2708,
        c238: n2702,
        c239: n2703,
        c304: n2587,
        c241: n2650,
        c248: n2704,
        c249: n2651,
        c312: n2803,
        c313: n2792,
        c255: n2723,
        h1: n3695, h2: n3696,
    };
    // body 67: buttons 0x30, forks 0x0
    sink.o3(48, take_3_15, &sh3, &o3);
    declined |= live_v49_b68 & (if bd_v49_b68 { ALL } else { !ok_v49_b68 });
    take_3_16 |= live_v49_b68 & ok_v49_b68 & (if bd_v49_b68 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2705,
        c301: n2727,
        c236: n2701,
        c302: n2728,
        c303: n2708,
        c238: n2702,
        c239: n2703,
        c304: n2618,
        c241: n2650,
        c248: n2704,
        c249: n2651,
        c312: n2818,
        c313: n2807,
        c255: n2723,
        h1: n3709, h2: n3710,
    };
    // body 68: buttons 0x31, forks 0x0
    sink.o3(49, take_3_16, &sh3, &o3);
    declined |= live_v50_b69 & (if bd_v50_b69 { ALL } else { !ok_v50_b69 });
    take_3_17 |= live_v50_b69 & ok_v50_b69 & (if bd_v50_b69 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2705,
        c301: n2727,
        c236: n2701,
        c302: n2744,
        c303: n2708,
        c238: n2702,
        c239: n2703,
        c304: n2634,
        c241: n2650,
        c248: n2704,
        c249: n2651,
        c312: n2833,
        c313: n2822,
        c255: n2723,
        h1: n3721, h2: n3722,
    };
    // body 69: buttons 0x32, forks 0x0
    sink.o3(50, take_3_17, &sh3, &o3);
    declined |= live_v52_b70 & (if bd_v52_b70 { ALL } else { !ok_v52_b70 });
    take_3_18 |= live_v52_b70 & ok_v52_b70 & (if bd_v52_b70 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2760,
        c301: n2761,
        c236: n2701,
        c302: n2762,
        c303: n2763,
        c238: n2702,
        c239: n2703,
        c304: n2587,
        c241: n2650,
        c248: n2704,
        c249: n2651,
        c312: n2848,
        c313: n2837,
        c255: n2723,
        h1: n3737, h2: n3738,
    };
    // body 70: buttons 0x34, forks 0x0
    sink.o3(52, take_3_18, &sh3, &o3);
    declined |= live_v53_b71 & (if bd_v53_b71 { ALL } else { !ok_v53_b71 });
    take_3_19 |= live_v53_b71 & ok_v53_b71 & (if bd_v53_b71 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2760,
        c301: n2727,
        c236: n2701,
        c302: n2728,
        c303: n2763,
        c238: n2702,
        c239: n2703,
        c304: n2618,
        c241: n2650,
        c248: n2704,
        c249: n2651,
        c312: n2853,
        c313: n2851,
        c255: n2723,
        h1: n3751, h2: n3752,
    };
    // body 71: buttons 0x35, forks 0x0
    sink.o3(53, take_3_19, &sh3, &o3);
    declined |= live_v54_b72 & (if bd_v54_b72 { ALL } else { !ok_v54_b72 });
    take_3_20 |= live_v54_b72 & ok_v54_b72 & (if bd_v54_b72 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2760,
        c301: n2727,
        c236: n2701,
        c302: n2744,
        c303: n2763,
        c238: n2702,
        c239: n2703,
        c304: n2634,
        c241: n2650,
        c248: n2704,
        c249: n2651,
        c312: n2857,
        c313: n2855,
        c255: n2723,
        h1: n3763, h2: n3764,
    };
    // body 72: buttons 0x36, forks 0x0
    sink.o3(54, take_3_20, &sh3, &o3);
    declined |= live_v56_b73 & (if bd_v56_b73 { ALL } else { !ok_v56_b73 });
    take_3_21 |= live_v56_b73 & ok_v56_b73 & (if bd_v56_b73 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2760,
        c301: n2761,
        c236: n2701,
        c302: n2762,
        c303: n2786,
        c238: n2702,
        c239: n2703,
        c304: n2587,
        c241: n2650,
        c248: n2704,
        c249: n2651,
        c312: n2848,
        c313: n2858,
        c255: n2723,
        h1: n3772, h2: n3773,
    };
    // body 73: buttons 0x38, forks 0x0
    sink.o3(56, take_3_21, &sh3, &o3);
    declined |= live_v57_b74 & (if bd_v57_b74 { ALL } else { !ok_v57_b74 });
    take_3_22 |= live_v57_b74 & ok_v57_b74 & (if bd_v57_b74 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2760,
        c301: n2727,
        c236: n2701,
        c302: n2728,
        c303: n2786,
        c238: n2702,
        c239: n2703,
        c304: n2618,
        c241: n2650,
        c248: n2704,
        c249: n2651,
        c312: n2853,
        c313: n2859,
        c255: n2723,
        h1: n3781, h2: n3782,
    };
    // body 74: buttons 0x39, forks 0x0
    sink.o3(57, take_3_22, &sh3, &o3);
    declined |= live_v58_b75 & (if bd_v58_b75 { ALL } else { !ok_v58_b75 });
    take_3_23 |= live_v58_b75 & ok_v58_b75 & (if bd_v58_b75 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n2699,
        c41: n2700,
        c300: n2760,
        c301: n2727,
        c236: n2701,
        c302: n2744,
        c303: n2786,
        c238: n2702,
        c239: n2703,
        c304: n2634,
        c241: n2650,
        c248: n2704,
        c249: n2651,
        c312: n2857,
        c313: n2860,
        c255: n2723,
        h1: n3790, h2: n3791,
    };
    // body 75: buttons 0x3a, forks 0x0
    sink.o3(58, take_3_23, &sh3, &o3);
    declined
}
