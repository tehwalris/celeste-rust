//! Render a whole compilable kernel from a TRACED frame.
//!
//! `transpile::kernel::render` does this for the WALK - the recipe
//! pipeline's emitter - and this is deliberately not that function
//! generalized. The walk's renderer carries machinery a traced frame
//! never produces: `Op::Split` fork configurations, `SplitValid`,
//! `fork_depth`, `valid_expr`, and a shape witness to bind against.
//! Those exist because `zi_fork_flr` splits one interval lane into two.
//! A tracer's outcomes come from real branches instead, so generalizing
//! the walk's renderer would mean inheriting assumptions and then
//! noticing them one at a time.
//!
//! Two differences from the walk's generated interface are the point
//! rather than an accident.
//!
//! INPUTS ARE NAMED BY PATH. The walk binds by canonical cell id
//! against a shape witness; this emits the path each input came from, so
//! the binder resolves against whatever block it is handed
//! (`trace::bind`). The cell ids in the struct field names are just
//! names - they agree with the traced input block's numbering, which
//! makes them readable, and nothing depends on that.
//!
//! AN OUTPUT BLOCK IS BUILT, NOT PATCHED. The walk's `acc_init` clones
//! the chunk's structure and overwrites the columns the frame wrote,
//! because in that pipeline the output shape IS the input shape. A
//! traced frame ends in several shapes - room (0,0) f40 has four, and
//! only one of them is the input's - so each outcome carries its own
//! structure and the kernel fills it. Measured there: about 105 pointer
//! cells that the structure carries, ~53 scalars the frame computes, and
//! nothing that passes through from the input block.

use std::collections::BTreeMap;
use std::fmt::Write;

use anyhow::Result;

use super::emit::{Bound, Lowered};
use super::verify::Frame;
use crate::transpile::kernel::{render_lines, OutField};

/// The Rust type of an input of this kind, at block-uniform width.
fn uni_ty(kind: &str) -> &'static str {
    match kind {
        "num" => "P8",
        "bool" => "bool",
        _ => "IV",
    }
}

/// The Rust type of an input of this kind, per lane.
fn row_ty(kind: &str) -> &'static str {
    match kind {
        "num" => "ZN",
        _ => "u16",
    }
}

/// Render the kernel for one traced frame.
pub fn render(f: &Frame, b: &Bound, l: &Lowered, title: &str) -> Result<String> {
    let mut o = String::new();

    // cell -> the path it was read from, for the input side.
    let mut in_path: BTreeMap<u32, String> = BTreeMap::new();
    for (i, cell) in f.in_cells.iter().enumerate() {
        in_path.insert(*cell, super::iface::show(&f.iface.slots[i]));
    }

    writeln!(
        o,
        "// GENERATED from a TRACED frame ({}). Do not edit.\n\
         //\n\
         // One input shape, {} output shapes, {} distinct button\n\
         // assignments. See `trace::kernel` for what this interface is\n\
         // and why it is not the walk's.\n\
         #![allow(unused_variables, unused_mut, unused_imports, clippy::all)]\n\
         use celeste_engine::kernel::*;\n\
         use celeste_core::pico8_num::{{Pico8Num as P8, Pico8NumInterval as IV}};\n\
         use celeste_core::cart_data::CartData;\n\
         use celeste_core::collision_cache::CollisionCache;\n\
         \n\
         pub struct G<'a> {{ pub cart: &'a CartData, pub cache: &'a CollisionCache }}\n",
        title,
        b.outcomes.len(),
        l.variants.len()
    )?;

    // ---- inputs ----
    let slot_list = |o: &mut String, name: &str, cells: &[(u32, &'static str)]| -> Result<()> {
        writeln!(o, "/// (path, kind) - resolved against a block at bind time.")?;
        writeln!(o, "pub const {}: &[(&str, &str)] = &[", name)?;
        for (cell, kind) in cells {
            writeln!(o, "    ({:?}, {:?}),", in_path[cell].as_str(), kind)?;
        }
        writeln!(o, "];\n")?;
        Ok(())
    };
    slot_list(&mut o, "UNI_SLOTS", &b.uni)?;
    writeln!(o, "/// Block-uniform inputs, in `UNI_SLOTS` order.")?;
    writeln!(o, "pub struct Uni {{")?;
    for (cell, kind) in &b.uni {
        writeln!(o, "    pub c{}: {},", cell, uni_ty(kind))?;
    }
    writeln!(o, "}}\n")?;

    slot_list(&mut o, "ROW_SLOTS", &b.inputs)?;
    writeln!(o, "/// Per-lane inputs, in `ROW_SLOTS` order.")?;
    writeln!(o, "pub struct RowsIn {{")?;
    for (cell, kind) in &b.inputs {
        writeln!(o, "    pub c{}: {},", cell, row_ty(kind))?;
    }
    writeln!(o, "}}\n")?;

    // ---- one output shape per outcome ----
    for (i, out) in l.outs.iter().enumerate() {
        let paths: BTreeMap<u32, String> = f.outs[i]
            .fields
            .iter()
            .zip(f.outs[i].cells.iter())
            .map(|((p, _, _), c)| (*c, super::iface::show(p)))
            .collect();
        writeln!(o, "// ---------------- outcome {} ----------------", i)?;
        writeln!(o, "/// (cell, path) - where each computed value goes.")?;
        writeln!(o, "pub const OUT_SLOTS_{}: &[(u32, &str)] = &[", i)?;
        for OutField { cell, .. } in &out.fields {
            writeln!(o, "    ({}, {:?}),", cell, paths[cell].as_str())?;
        }
        writeln!(o, "];\n")?;
        writeln!(
            o,
            "/// Cells that end the frame as a fresh UnknownBool - next\n\
             /// frame's button inputs. The boundary writes UBool, no data."
        )?;
        writeln!(o, "pub const OUT_UBOOL_{}: &[(u32, &str)] = &[", i)?;
        for (c, p) in f.outs[i].ubool_cells.iter().zip(f.outs[i].ubool.iter()) {
            writeln!(o, "    ({}, {:?}),", c, super::iface::show(p).as_str())?;
        }
        writeln!(o, "];\n")?;

        writeln!(o, "/// Outcome {}'s button-INDEPENDENT values.", i)?;
        writeln!(o, "pub struct KShared{} {{", i)?;
        for OutField { cell, ty, tainted, .. } in &out.fields {
            if !*tainted {
                writeln!(o, "    pub c{}: {},", cell, ty)?;
            }
        }
        writeln!(o, "}}\n")?;
        writeln!(o, "/// Outcome {}'s per-assignment values and lane masks.", i)?;
        writeln!(o, "pub struct KOut{} {{", i)?;
        writeln!(o, "    /// Lanes that take THIS successor.")?;
        writeln!(o, "    pub live: u16,")?;
        writeln!(o, "    /// Lanes the kernel declines - they go to the interpreter.")?;
        writeln!(o, "    pub deopt: u16,")?;
        writeln!(o, "    /// The whole block is undecidable here.")?;
        writeln!(o, "    pub bd: bool,")?;
        for OutField { cell, ty, tainted, .. } in &out.fields {
            if *tainted {
                writeln!(o, "    pub c{}: {},", cell, ty)?;
            }
        }
        writeln!(o, "}}\n")?;
    }

    // One struct so `frame` has a fixed arity whatever the outcome count.
    writeln!(o, "/// Every outcome's result for one button assignment.")?;
    writeln!(o, "pub struct KOuts<'a> {{")?;
    for i in 0..l.outs.len() {
        writeln!(o, "    pub sh{i}: &'a KShared{i},", i = i)?;
        writeln!(o, "    pub v{i}: &'a KOut{i},", i = i)?;
    }
    writeln!(o, "}}\n")?;

    // ---- the body ----
    writeln!(
        o,
        "#[inline(never)]\n\
         pub fn frame(u: &Uni, rin: &RowsIn, g: &G, out: &mut impl FnMut(u8, &KOuts)) {{"
    )?;
    o.push_str(&render_lines(&l.body));
    for (i, out) in l.outs.iter().enumerate() {
        writeln!(o, "    let sh{} = KShared{} {{", i, i)?;
        for OutField { cell, expr, tainted, .. } in &out.fields {
            if !*tainted {
                writeln!(o, "        c{}: {},", cell, expr)?;
            }
        }
        writeln!(o, "    }};")?;
    }
    writeln!(
        o,
        "    // {} of 64 button assignments are distinct successors",
        l.variants.len()
    )?;
    for v in &l.variants {
        for (i, out) in l.outs.iter().enumerate() {
            let p = &v.per[i];
            writeln!(o, "    let o{} = KOut{} {{", i, i)?;
            writeln!(o, "        live: {},", p.live)?;
            writeln!(o, "        deopt: !{},", p.ok)?;
            writeln!(o, "        bd: {},", p.bd)?;
            for OutField { cell, tainted, .. } in &out.fields {
                if *tainted {
                    writeln!(o, "        c{}: {},", cell, p.outputs[cell])?;
                }
            }
            writeln!(o, "    }};")?;
        }
        let args: Vec<String> = (0..l.outs.len())
            .map(|i| format!("sh{i}: &sh{i}, v{i}: &o{i}", i = i))
            .collect();
        writeln!(o, "    out({}, &KOuts {{ {} }});", v.mask, args.join(", "))?;
    }
    writeln!(o, "}}")?;
    Ok(o)
}
