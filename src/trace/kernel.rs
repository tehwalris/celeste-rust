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
//! traced frame ends in several shapes - room (1,0) f40 has four, and
//! only one of them is the input's - so each outcome carries its own
//! structure and the kernel fills it. Measured there: about 105 pointer
//! cells that the structure carries, ~53 scalars the frame computes, and
//! nothing that passes through from the input block.

use std::collections::BTreeMap;
use std::fmt::Write;

use anyhow::Result;

use celeste_engine::runtime2::{Cell2, Col, AV};

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
        "ival" => "ZI",
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
         use celeste_engine::runtime2::{{Rt2, Col, AV}};\n\
         use celeste_engine::slots::{{build_block, resolve_path, SCell}};\n\
         use std::sync::Arc;\n\
         \n\
         pub struct G<'a> {{ pub cart: &'a CartData, pub cache: &'a CollisionCache }}\n",
        title,
        b.outcomes.len(),
        l.variants.len()
    )?;

    // The block this kernel is FOR, as the engine hashes it.
    //
    // Binding by path is not a shape test: shape 2's paths are a subset
    // of shape 0's - it is the same room with an empty object list - so
    // its `bind` succeeds against a shape-0 block and then computes a
    // frame that assumed no objects. The hash is the whole canonical
    // structure, so it distinguishes them, and it is what the dispatcher
    // keys on. `structure_of` builds this block in canonical order
    // already, which is what makes the hash comparable with one taken
    // after the boundary.
    writeln!(
        o,
        "/// The canonical shape this kernel was traced for.\n\
         pub const SHAPE: u64 = {};\n",
        f.in_rt2.shape_hash_of()
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

    // Where each per-lane input LIVES in the block we were handed.
    // Resolved once per block, in `bind`, rather than once per 16-lane
    // slice: the path walk is cheap but it is not free, and the answer
    // cannot change between slices of one block.
    writeln!(o, "/// Cell ids for `ROW_SLOTS` in the bound block.")?;
    writeln!(o, "pub struct RowSlots {{")?;
    for (cell, _) in &b.inputs {
        writeln!(o, "    pub c{}: u32,", cell)?;
    }
    writeln!(o, "}}\n")?;

    writeln!(
        o,
        "/// Bind to a block by PATH. `None` means this block does not\n\
         /// have a slot the kernel needs, or holds it at the wrong kind -\n\
         /// either way the block takes the interpreter path.\n\
         pub fn bind(b: &Rt2) -> Option<(Uni, RowSlots)> {{\n\
         \x20   let cell = |p: &str| resolve_path(b, p).ok();\n\
         \x20   let u = Uni {{"
    )?;
    for (cell, kind) in &b.uni {
        let path = in_path[cell].as_str();
        let pat = match *kind {
            "num" => "Col::U(AV::Num(n)) => *n",
            "bool" => "Col::U(AV::Bool(v)) => *v",
            other => anyhow::bail!("uniform input kind {}", other),
        };
        writeln!(
            o,
            "        c{c}: match &b.cols[cell({p:?})? as usize] {{ {pat}, _ => return None }},",
            c = cell,
            p = path,
            pat = pat
        )?;
    }
    writeln!(o, "    }};")?;
    writeln!(o, "    let s = RowSlots {{")?;
    for (cell, _) in &b.inputs {
        writeln!(o, "        c{}: cell({:?})?,", cell, in_path[cell].as_str())?;
    }
    writeln!(o, "    }};")?;
    writeln!(o, "    Some((u, s))\n}}\n")?;

    // The SAME walk, reporting instead of refusing.
    //
    // `bind` returns `Option` because it is on the hot path and the
    // caller only has one thing to do with a failure. But under the
    // never-deopt doctrine that failure STOPS the run, and "did not
    // bind" is not a report anyone can act on - which slot, and holding
    // what, is. Kept in step by construction: both are generated from
    // the same two slot lists.
    writeln!(
        o,
        "/// Which slot stopped `bind`. `None` means it would have bound.\n\
         pub fn bind_why(b: &Rt2) -> Option<String> {{\n\
         \x20   for (p, kind) in UNI_SLOTS {{\n\
         \x20       let Ok(c) = resolve_path(b, p) else {{\n\
         \x20           return Some(format!(\"{{}}: no such slot\", p));\n\
         \x20       }};\n\
         \x20       let col = &b.cols[c as usize];\n\
         \x20       let ok = matches!(\n\
         \x20           (*kind, col),\n\
         \x20           (\"num\", Col::U(AV::Num(_))) | (\"bool\", Col::U(AV::Bool(_)))\n\
         \x20       );\n\
         \x20       if !ok {{\n\
         \x20           return Some(format!(\n\
         \x20               \"{{}}: block-uniform {{}} slot holds {{:?}}\",\n\
         \x20               p, kind, col\n\
         \x20           ));\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   for (p, kind) in ROW_SLOTS {{\n\
         \x20       let Ok(c) = resolve_path(b, p) else {{\n\
         \x20           return Some(format!(\"{{}}: no such slot\", p));\n\
         \x20       }};\n\
         \x20       let col = &b.cols[c as usize];\n\
         \x20       let lanes = (0..b.width).map(|i| col.at(i));\n\
         \x20       let ok = match *kind {{\n\
         \x20           \"num\" => lanes.clone().all(|v| matches!(v, AV::Num(_))),\n\
         \x20           _ => lanes.clone().all(|v| matches!(v, AV::Bool(_))),\n\
         \x20       }};\n\
         \x20       if !ok {{\n\
         \x20           let bad = lanes.enumerate().find(|(_, v)| match *kind {{\n\
         \x20               \"num\" => !matches!(v, AV::Num(_)),\n\
         \x20               _ => !matches!(v, AV::Bool(_)),\n\
         \x20           }});\n\
         \x20           return Some(format!(\n\
         \x20               \"{{}}: per-lane {{}} slot holds {{:?}} at lane {{:?}}\",\n\
         \x20               p, kind, bad.map(|(_, v)| v), bad.map(|(i, _)| i)\n\
         \x20           ));\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   // Every slot resolves and every lane is the right kind, so\n\
         \x20   // what stopped `rows` is REPRESENTATION: a column whose\n\
         \x20   // values are right but whose storage the gather does not\n\
         \x20   // accept (a `Col::V` of numbers where it wants `Col::N`).\n\
         \x20   // `collapse_uniform_cols` is the usual missing step.\n\
         \x20   None\n\
         }}\n"
    )?;

    writeln!(
        o,
        "/// Gather rows [lo, lo+16) into lane arrays. Short slices pad by\n\
         /// repeating the last row; padded lanes are ignored by width.\n\
         pub fn rows(b: &Rt2, s: &RowSlots, lo: usize) -> Option<RowsIn> {{\n\
         \x20   let at = |i: usize| -> usize {{ (lo + i).min(b.width - 1) }};\n\
         \x20   Some(RowsIn {{"
    )?;
    for (cell, kind) in &b.inputs {
        match *kind {
            "num" => writeln!(
                o,
                "        c{c}: match &b.cols[s.c{c} as usize] {{\n\
                 \x20           Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),\n\
                 \x20           Col::U(AV::Num(n)) => zn_splat(*n),\n\
                 \x20           _ => return None,\n\
                 \x20       }},",
                c = cell
            )?,
            "bool" => writeln!(
                o,
                "        c{c}: match &b.cols[s.c{c} as usize] {{\n\
                 \x20           Col::V(v) => {{\n\
                 \x20               let mut m = 0u16;\n\
                 \x20               for i in 0..W {{\n\
                 \x20                   match v[at(i)] {{\n\
                 \x20                       AV::Bool(true) => m |= 1 << i,\n\
                 \x20                       AV::Bool(false) => {{}}\n\
                 \x20                       _ => return None,\n\
                 \x20                   }}\n\
                 \x20               }}\n\
                 \x20               m\n\
                 \x20           }}\n\
                 \x20           Col::U(AV::Bool(t)) => if *t {{ 0xffff }} else {{ 0 }},\n\
                 \x20           _ => return None,\n\
                 \x20       }},",
                c = cell
            )?,
            // A per-lane INTERVAL. `Col::I` is the raw (lo, hi) pair
            // form; `Col::U(AV::Ival)` is what a block whose lanes all
            // carry the same widened `rem` collapses to, which is every
            // block straight out of the boundary - the widening writes
            // one value for the whole column. A plain number is also
            // accepted, as the degenerate interval it is: a lane can
            // reach a kernel with `rem` exact, before the first
            // boundary widened it.
            "ival" => writeln!(
                o,
                "        c{c}: match &b.cols[s.c{c} as usize] {{\n\
                 \x20           Col::I(v) => ZI {{\n\
                 \x20               lo: ZN::from_array(core::array::from_fn(|i| v[at(i)].0)),\n\
                 \x20               hi: ZN::from_array(core::array::from_fn(|i| v[at(i)].1)),\n\
                 \x20           }},\n\
                 \x20           Col::U(AV::Ival(lo, hi)) => ZI {{ lo: zn_splat(*lo), hi: zn_splat(*hi) }},\n\
                 \x20           Col::N(v) => ZI {{\n\
                 \x20               lo: ZN::from_array(core::array::from_fn(|i| v[at(i)])),\n\
                 \x20               hi: ZN::from_array(core::array::from_fn(|i| v[at(i)])),\n\
                 \x20           }},\n\
                 \x20           Col::U(AV::Num(n)) => ZI {{ lo: zn_splat(*n), hi: zn_splat(*n) }},\n\
                 \x20           _ => return None,\n\
                 \x20       }},",
                c = cell
            )?,
            other => anyhow::bail!("row input kind {}", other),
        }
    }
    writeln!(o, "    }})\n}}\n")?;

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

        // THE OUTPUT SHAPE, as generated data. Everything the shape
        // determines - structure, globals, pointer columns - is a
        // compile-time constant of this outcome, so the kernel builds
        // its block rather than patching a copy of the input's.
        let rt2 = &f.outs[i].rt2;
        writeln!(o, "pub const OUT_SHAPE_{}: &[SCell] = &[", i)?;
        for cell in &rt2.structure {
            match cell {
                Cell2::Val => writeln!(o, "    SCell::Val,")?,
                Cell2::Obj(fields) => {
                    let items: Vec<String> =
                        fields.iter().map(|(k, c)| format!("({}, {})", k, c)).collect();
                    writeln!(o, "    SCell::Obj(&[{}]),", items.join(", "))?
                }
                Cell2::Arr(items) => {
                    let items: Vec<String> = items.iter().map(|c| c.to_string()).collect();
                    writeln!(o, "    SCell::Arr(&[{}]),", items.join(", "))?
                }
                Cell2::Clo(fid, caps) => {
                    // The captures are pointers by construction (a
                    // capture holds the object that owns the closure),
                    // and a shape can only carry what is fixed - so a
                    // capture that is not a pointer has no static form
                    // and is refused rather than dropped.
                    let mut ts = Vec::new();
                    for cap in caps.iter() {
                        match cap {
                            Col::U(AV::Ptr(t)) => ts.push(t.to_string()),
                            other => anyhow::bail!(
                                "outcome {} has a closure capture {:?}, which is not a pointer",
                                i,
                                other
                            ),
                        }
                    }
                    writeln!(o, "    SCell::Clo({}, &[{}]),", fid, ts.join(", "))?
                }
                Cell2::Bi(b) => writeln!(o, "    SCell::Bi({}),", b)?,
                Cell2::Unk => writeln!(o, "    SCell::Unk,")?,
            }
        }
        writeln!(o, "];\n")?;
        writeln!(o, "pub const OUT_GLOBALS_{}: &[u32] = &{:?};\n", i, rt2.globals)?;
        writeln!(o, "/// (cell, target) - the pointer topology, fixed by the shape.")?;
        writeln!(o, "pub const OUT_PTRS_{}: &[(u32, u32)] = &[", i)?;
        for (c, col) in rt2.cols.iter().enumerate() {
            if let Col::U(AV::Ptr(t)) = col {
                writeln!(o, "    ({}, {}),", c, t)?;
            }
        }
        writeln!(o, "];\n")?;

        writeln!(o, "/// Outcome {}'s button-INDEPENDENT values.", i)?;
        writeln!(o, "/// Values shared by every button assignment.\n\
                 /// COMPILE-TIME CONSTANTS are absent: their column is\n\
                 /// written once when the block is built, so there is\n\
                 /// nothing to carry per lane.")?;
        writeln!(o, "pub struct KShared{} {{", i)?;
        for OutField { cell, ty, tainted, konst, .. } in &out.fields {
            if !*tainted && konst.is_none() {
                writeln!(o, "    pub c{}: {},", cell, ty)?;
            }
        }
        writeln!(o, "}}\n")?;
        writeln!(o, "/// Outcome {}'s per-assignment values and lane masks.", i)?;
        writeln!(
            o,
            "/// The cells of outcome {i} that DIFFER between button\n\
             /// assignments. Everything else is either constant (written\n\
             /// once when the block is built) or shared (`KShared{i}`).\n\
             ///\n\
             /// No `live`/`deopt` here: which lanes a group writes is its\n\
             /// `take` argument, and declined lanes are accumulated by\n\
             /// `frame` itself.",
            i = i
        )?;
        writeln!(o, "pub struct KOut{} {{", i)?;
        for OutField { cell, ty, tainted, .. } in &out.fields {
            if *tainted {
                writeln!(o, "    pub c{}: {},", cell, ty)?;
            }
        }
        writeln!(
            o,
            "    /// This successor's ROW KEY, both halves, 16 lanes at\n\
             \x20   /// once. Folded by the graph rather than by `append`:\n\
             \x20   /// the fold is sequential over cells but every step is\n\
             \x20   /// a vector, and its button-independent prefix is one\n\
             \x20   /// shared chain across all the assignments instead of\n\
             \x20   /// being recomputed per candidate row.\n\
             \x20   pub h1: ZW,\n\
             \x20   pub h2: ZW,"
        )?;
        writeln!(o, "}}\n")?;
    }

    // ---- building an outcome's block, and filling it ----
    for (i, out) in l.outs.iter().enumerate() {
        writeln!(
            o,
            "/// An EMPTY accumulator with outcome {i}'s shape: structure,\n\
             /// globals and pointers from the constants above, computed\n\
             /// columns starting empty and growing by `append{i}`.\n\
             pub fn acc{i}(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {{\n\
             \x20   let mut b = build_block(OUT_SHAPE_{i}, OUT_GLOBALS_{i}, OUT_PTRS_{i}, 0, cart, cache);",
            i = i
        )?;
        for OutField { cell, ty, konst, .. } in &out.fields {
            // A constant column holds one value for the whole
            // accumulator, so it is written HERE, once, instead of being
            // pushed per row. 44 of outcome 0's 52 fields are like this.
            if let Some(av) = konst {
                writeln!(o, "    b.cols[{}] = Col::U({});", cell, av)?;
                continue;
            }
            let empty = match *ty {
                "ZN" => "Col::N(Vec::new())",
                "ZB" => "Col::V(Vec::new())",
                // A per-lane interval, in the raw (lo, hi) form.
                // `player.rem` leaves a frame as the fork fragment it
                // was narrowed to, which is an interval; the next
                // boundary widens it back.
                "ZI" => "Col::I(Vec::new())",
                other => anyhow::bail!("output type {}", other),
            };
            writeln!(o, "    b.cols[{}] = {};", cell, empty)?;
        }
        writeln!(
            o,
            "    for (cell, _) in OUT_UBOOL_{} {{\n\
             \x20       b.cols[*cell as usize] = Col::U(AV::UBool);\n\
             \x20   }}\n\
             \x20   b\n\
             }}\n",
            i
        )?;

        writeln!(
            o,
            "/// Append this assignment's lanes that TAKE outcome {i} and\n\
             /// that the kernel is willing to keep. A lane in `deopt` is\n\
             /// dropped here and belongs to the interpreter - the caller\n\
             /// has `kv.deopt` and must account for it.\n\
             /// SKIPS a row whose values another configuration already\n\
             /// wrote. The key is over the non-constant cells only - the\n\
             /// rest are one value for the whole accumulator and cannot\n\
             /// tell two rows apart - so it is a handful of mixes rather\n\
             /// than a hundred, computed from values already in\n\
             /// registers. A duplicate caught here costs nothing; one\n\
             /// caught at the boundary has already been written.\n\
             ///\n\
             /// 128-bit like the boundary's own key, because a collision\n\
             /// DROPS a successor rather than merely costing time.\n\
             pub fn append{i}(\n\
             \x20   acc: &mut Rt2, sh: &KShared{i}, kv: &KOut{i}, take: u16,\n\
             \x20   n: usize, seen: &mut RowSet,\n\
             ) -> u16 {{\n\
             \x20   // Returns the lanes actually WRITTEN, which is `take`\n\
             \x20   // minus the ones another configuration already wrote.\n\
             \x20   // A caller that needs to know which (assignment, lane)\n\
             \x20   // produced row k cannot infer it from `take`.\n\
             \x20   let mut wrote: u16 = 0;\n\
             \x20   let take = take & ((1u32 << n) - 1) as u16;\n\
             \x20   // The key columns come out of their registers ONCE.\n\
             \x20   // `ZW::lane` is a store plus a load, so calling it\n\
             \x20   // inside the loop would do that sixteen times for a\n\
             \x20   // value that does not change.\n\
             \x20   let (h1, h2) = (kv.h1.to_array(), kv.h2.to_array());\n\
             \x20   for i in 0..n {{\n\
             \x20       if take & (1 << i) == 0 {{ continue; }}",
            i = i
        )?;
        // The key is FOLDED BY THE GRAPH now (`Op::Bits` / `Op::Mix`),
        // 16 lanes at a time and with its button-independent prefix
        // shared across every assignment. All that is left here is the
        // table probe, which is inherently scalar: a hash table cannot
        // be vectorized, and at ~2M probes it does not need to be.
        writeln!(o, "        if !seen.insert((h1[i], h2[i])) {{ continue; }}")?;
        for OutField { cell, ty, tainted, konst, .. } in &out.fields {
            if konst.is_some() {
                continue; // written once by `acc`, not per row
            }
            let src = if *tainted { "kv" } else { "sh" };
            match *ty {
                "ZN" => writeln!(
                    o,
                    "        if let Col::N(v) = &mut acc.cols[{c}] {{ v.push({s}.c{c}.lane(i)); }}",
                    c = cell,
                    s = src
                )?,
                // An UNKNOWN boolean is a real abstract value, not a
                // broken one, so it goes in as `UBool` rather than being
                // coerced to a definite bit.
                "ZB" => writeln!(
                    o,
                    "        if let Col::V(v) = &mut acc.cols[{c}] {{\n\
                     \x20           v.push(if {s}.c{c}.known & (1 << i) != 0 {{\n\
                     \x20               AV::Bool({s}.c{c}.val & (1 << i) != 0)\n\
                     \x20           }} else {{ AV::UBool }});\n\
                     \x20       }}",
                    c = cell,
                    s = src
                )?,
                "ZI" => writeln!(
                    o,
                    "        if let Col::I(v) = &mut acc.cols[{c}] {{\n\
                     \x20           v.push(({s}.c{c}.lo.lane(i), {s}.c{c}.hi.lane(i)));\n\
                     \x20       }}",
                    c = cell,
                    s = src
                )?,
                other => anyhow::bail!("output type {}", other),
            }
        }
        writeln!(o, "        wrote |= 1 << i;\n        acc.width += 1;\n    }}\n    wrote\n}}\n")?;
    }

    // A small dynamic layer over the per-outcome items above. Rust
    // cannot index a struct or a function name by a runtime `usize`, and
    // a caller that loops over outcomes needs to. Generated rather than
    // written by hand so it cannot fall out of step with the count.
    writeln!(o, "pub const OUTCOMES: usize = {};\n", l.outs.len())?;
    let dispatch = |o: &mut String, sig: &str, arm: &str| -> Result<()> {
        writeln!(o, "{} {{", sig)?;
        writeln!(o, "    match i {{")?;
        for k in 0..l.outs.len() {
            writeln!(o, "        {} => {},", k, arm.replace('#', &k.to_string()))?;
        }
        writeln!(o, "        _ => panic!(\"outcome {{}} of {}\", i),", l.outs.len())?;
        writeln!(o, "    }}\n}}\n")?;
        Ok(())
    };
    dispatch(
        &mut o,
        "pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2",
        "acc#(cart, cache)",
    )?;
    dispatch(&mut o, "pub fn out_slots(i: usize) -> &'static [(u32, &'static str)]", "OUT_SLOTS_#")?;

    // ONE uniform entry point per kernel, so a dispatcher can hold
    // kernels for different shapes as plain function pointers. Every
    // shape's `Uni`, `RowsIn` and `KShared` are different types, so
    // anything that exposed them could not be uniform.
    //
    // `None` means this kernel is not for this block - a path that does
    // not resolve, or a slot at the wrong kind. `Some(mask)` is the
    // lanes DECLINED; under the never-deopt doctrine a non-zero mask
    // stops the run rather than falling back.
    writeln!(o, "struct Append<'a> {{ accs: &'a mut [Rt2], seen: &'a mut [RowSet], n: usize }}\n")?;
    writeln!(o, "impl<'a> Sink for Append<'a> {{")?;
    for i in 0..l.outs.len() {
        writeln!(
            o,
            "    fn o{i}(&mut self, _mask: u8, take: u16, sh: &KShared{i}, v: &KOut{i}) {{\n\
             \x20       append{i}(&mut self.accs[{i}], sh, v, take, self.n, &mut self.seen[{i}]);\n\
             \x20   }}",
            i = i
        )?;
    }
    writeln!(o, "}}\n")?;
    writeln!(
        o,
        "pub fn step(\n\
         \x20   b: &Rt2, lo: usize, n: usize, accs: &mut [Rt2], seen: &mut [RowSet],\n\
         ) -> Option<u16> {{\n\
         \x20   let (u, s) = bind(b)?;\n\
         \x20   let rin = rows(b, &s, lo)?;\n\
         \x20   let g = G {{ cart: &b.cart, cache: &b.cache }};\n\
         \x20   // The row sets are the CALLER's and reset in O(1) here.\n\
         \x20   // Slice-local because that is where the duplication is:\n\
         \x20   // it comes from configurations agreeing on one lane, and\n\
         \x20   // a lane lives in one slice.\n\
         \x20   seen.iter_mut().for_each(|s| s.next_slice());\n\
         \x20   let mut sink = Append {{ accs, seen, n }};\n\
         \x20   Some(frame(&u, &rin, &g, &mut sink))\n\
         }}\n"
    )?;

    // ---- the body ----
    //
    // GROUP-DRIVEN. Variants that write the same values to an outcome
    // append ONCE, with the union of their lanes, instead of appending
    // identical rows that a dedup then throws away. Outcome 0 has no
    // per-variant cells at all, so its 24 button assignments are one
    // group and produce one row per lane rather than 24.
    //
    // The union is accumulated INCREMENTALLY (`take |= ...` after each
    // variant) and the append is emitted at the group's LAST member, so
    // nothing has to stay live: a variant's masks die immediately, and
    // the group's values are computed once, at the end, from
    // expressions every member shares by definition. Building the union
    // as one expression instead kept 48 masks alive at once and took
    // the build from 70 s to 20+ minutes.
    // A block-undecidable condition zeroes `live`, which would drop
    // every lane of that outcome SILENTLY - the missing-successor
    // failure the whole boolean split exists to prevent. Nothing
    // consumes `bd` today because it is always the literal `false` here;
    // this refuses to emit a kernel where that stops being true, rather
    // than letting it become a quiet hole.
    {
        use crate::transpile::kernel::Line;
        for line in &l.body {
            if let Line::Let { name, expr, .. } = line {
                if name.starts_with("bd_v") && expr != "false" {
                    anyhow::bail!(
                        "{} is `{}`, not `false`: a block-undecidable condition zeroes \
                         `live` and would drop every lane of that outcome silently. It \
                         has to be routed as a deopt instead.",
                        name,
                        expr
                    );
                }
            }
        }
    }

    let mut groups: Vec<Vec<usize>> = Vec::new(); // per outcome: variant -> group
    let mut last: Vec<Vec<bool>> = Vec::new();    // per outcome: is last of its group
    let mut n_groups: Vec<usize> = Vec::new();
    // The group signature is the TAINTED cells only, and that is also
    // what makes it sound to emit ONE member's row key for the whole
    // group. A row is (const cells, shared cells, tainted cells); the
    // first two are identical across every assignment by construction,
    // so two assignments with textually equal tainted cells write equal
    // rows - and the key is a pure function of exactly those, folded by
    // the graph. Equal rows, equal key. The last member's key
    // expression names different nodes than the first member's might,
    // but they evaluate to the same word.
    for oi in 0..l.outs.len() {
        let key = |v: &crate::transpile::lower::Variant| -> String {
            let p = &v.per[oi];
            l.outs[oi]
                .fields
                .iter()
                .filter(|f| f.tainted)
                .map(|f| p.outputs[&f.cell].clone())
                .collect::<Vec<_>>()
                .join(";")
        };
        let mut seen: BTreeMap<String, usize> = BTreeMap::new();
        let mut g = Vec::new();
        for v in &l.variants {
            let k = key(v);
            let n = seen.len();
            g.push(*seen.entry(k).or_insert(n));
        }
        let ng = seen.len();
        let mut is_last = vec![false; g.len()];
        for gi in 0..ng {
            if let Some(pos) = g.iter().rposition(|x| *x == gi) {
                is_last[pos] = true;
            }
        }
        groups.push(g);
        last.push(is_last);
        n_groups.push(ng);
    }

    writeln!(
        o,
        "/// Where a frame's rows go. One call per (outcome, GROUP), not\n\
         /// per outcome per variant: variants that write identical\n\
         /// values are one call whose `take` is the union of their\n\
         /// lanes. `mask` is the group's REPRESENTATIVE assignment -\n\
         /// every member computes the same values, so any of them\n\
         /// identifies the row for a caller that wants to check it\n\
         /// against the graph.\n\
         pub trait Sink {{"
    )?;
    for i in 0..l.outs.len() {
        writeln!(o, "    fn o{i}(&mut self, mask: u8, take: u16, sh: &KShared{i}, v: &KOut{i});", i = i)?;
    }
    writeln!(o, "}}\n")?;

    writeln!(
        o,
        "/// Run one frame over 16 lanes. Returns the lanes DECLINED -\n\
         /// live but not `ok` - which the never-deopt doctrine turns\n\
         /// into a stopped run.\n\
         #[inline(never)]\n\
         pub fn frame(u: &Uni, rin: &RowsIn, g: &G, sink: &mut dyn Sink) -> u16 {{\n\
         \x20   let mut declined: u16 = 0;"
    )?;
    o.push_str(&render_lines(&l.body));
    for (i, out) in l.outs.iter().enumerate() {
        writeln!(o, "    let sh{} = KShared{} {{", i, i)?;
        for OutField { cell, expr, tainted, konst, .. } in &out.fields {
            if !*tainted && konst.is_none() {
                writeln!(o, "        c{}: {},", cell, expr)?;
            }
        }
        writeln!(o, "    }};")?;
    }
    for (i, ng) in n_groups.iter().enumerate() {
        for gi in 0..*ng {
            writeln!(o, "    let mut take_{}_{}: u16 = 0;", i, gi)?;
        }
    }
    writeln!(
        o,
        "    // {} distinct button assignments; per outcome they fall\n\
         \x20   // into {:?} groups that write identical values.",
        l.variants.len(),
        n_groups
    )?;
    for (vi, v) in l.variants.iter().enumerate() {
        for (i, out) in l.outs.iter().enumerate() {
            let p = &v.per[i];
            let gi = groups[i][vi];
            writeln!(o, "    declined |= {} & !{};", p.live, p.ok)?;
            writeln!(o, "    take_{}_{} |= {} & {};", i, gi, p.live, p.ok)?;
            if !last[i][vi] {
                continue;
            }
            writeln!(o, "    let o{} = KOut{} {{", i, i)?;
            for OutField { cell, tainted, .. } in &out.fields {
                if *tainted {
                    writeln!(o, "        c{}: {},", cell, p.outputs[cell])?;
                }
            }
            let (h1, h2) = p.key.as_ref().ok_or_else(|| {
                anyhow::anyhow!("the traced emitter needs `Emit::row_key`, which was off")
            })?;
            writeln!(o, "        h1: {}, h2: {},", h1, h2)?;
            writeln!(o, "    }};")?;
            writeln!(
                o,
                "    sink.o{i}({m}, take_{i}_{gi}, &sh{i}, &o{i});",
                i = i,
                gi = gi,
                m = v.mask
            )?;
        }
    }
    // Close the FORK loops the body opened. Everything above is inside
    // them, which is the point: each fork configuration is its own set
    // of rows for the same input lane.
    for _ in 0..b.forks {
        writeln!(o, "    }}")?;
    }
    writeln!(o, "    declined")?;
    writeln!(o, "}}")?;
    Ok(o)
}

/// TYPE-CHECK a rendered kernel, by handing it to rustc.
///
/// `render` returning 6,000 lines only says the emitter emitted
/// something. Whether those lines are Rust - whether they use an engine
/// API that still exists, with the types it still has - is a separate
/// question, and rustc is the only thing that answers it.
///
/// That gap was not theoretical. On 2026-08-23 the lane type became a
/// register, the walk's emitter was updated, `generated_is_current`
/// passed, the whole 279-test gate went green - and the TRACED emitter
/// was still building intervals out of arrays and indexing the row-key
/// columns per lane. Nothing noticed until `check-traced-kernel.sh` ran
/// by hand, because that script is the only thing that compiled it and
/// it is not in the gate.
///
/// `--emit=metadata` so this is type-checking and not codegen: the
/// point is the API surface, and codegen of a 6,000-line body is the
/// expensive part we cannot afford on every commit.
///
/// Links against the rlibs the TEST BINARY was built with, found next to
/// it in `deps/`. Not finding them is a hard failure, not a skip - a
/// check that silently does nothing is worse than no check, because it
/// reads as green.
pub fn typecheck_rendered(src: &str) -> Result<()> {
    use anyhow::Context;
    let exe = std::env::current_exe().context("locating the test binary")?;
    let deps = exe.parent().ok_or_else(|| anyhow::anyhow!("test binary has no parent dir"))?;

    // The newest matching rlib: a `target/` accumulates stale ones, and
    // linking an old `celeste-engine` would check against an API that is
    // no longer there - which is the exact thing this exists to catch.
    let newest = |stem: &str| -> Result<std::path::PathBuf> {
        let mut best: Option<(std::time::SystemTime, std::path::PathBuf)> = None;
        let entries =
            std::fs::read_dir(deps).with_context(|| format!("reading {}", deps.display()))?;
        for e in entries {
            let p = e?.path();
            let name = p.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if name.starts_with(&format!("lib{}-", stem)) && name.ends_with(".rlib") {
                let t = p.metadata()?.modified()?;
                if best.as_ref().is_none_or(|(bt, _)| t > *bt) {
                    best = Some((t, p));
                }
            }
        }
        best.map(|(_, p)| p).ok_or_else(|| {
            anyhow::anyhow!(
                "no lib{}-*.rlib in {} - the type-check cannot run, and skipping it \
                 would read as a pass",
                stem,
                deps.display()
            )
        })
    };

    let dir = std::env::temp_dir().join(format!("celeste-typecheck-{}", std::process::id()));
    std::fs::create_dir_all(&dir)?;
    let file = dir.join("kernel.rs");
    std::fs::write(&file, src)?;

    let mut cmd = std::process::Command::new("rustc");
    cmd.arg("--edition=2021")
        .arg("--crate-type=lib")
        .arg("--crate-name=traced_kernel")
        .arg("--emit=metadata")
        .arg("-o")
        .arg(dir.join("kernel.rmeta"))
        .arg("-L")
        .arg(format!("dependency={}", deps.display()));
    for stem in ["celeste_engine", "celeste_core", "celeste_names"] {
        cmd.arg("--extern").arg(format!("{}={}", stem, newest(stem)?.display()));
    }
    cmd.arg(&file);

    let out = cmd.output().context("running rustc on the rendered kernel")?;
    let _ = std::fs::remove_dir_all(&dir);
    if !out.status.success() {
        let err = String::from_utf8_lossy(&out.stderr);
        // The first few diagnostics; the whole thing is thousands of
        // lines when a template is wrong in every cell.
        let head: Vec<&str> = err.lines().take(40).collect();
        anyhow::bail!(
            "the rendered kernel does not type-check ({} lines of it):\n{}",
            src.lines().count(),
            head.join("\n")
        );
    }
    Ok(())
}

/// One traced frame, everything owned, ready to render or to run.
///
/// The tracer's `Interp` borrows the parsed ASTs, so a caller outside
/// this crate cannot easily set one up. Nothing in here borrows them -
/// `Frame`, `Graph` and `Rt2` are all owned - so the whole thing can be
/// handed across a crate boundary, which is what the out-of-tree check
/// harness needs.
pub struct Reference {
    pub frame: Frame,
    pub graph: crate::transpile::graph::Graph,
    pub bound: Bound,
    pub lowered: Lowered,
    pub cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    pub cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
}

/// Trace the frame this kernel is rendered from: room (1,0), warmed up
/// to the first frame that has a player, pinned to its pm1 key.
pub fn reference_frame() -> Result<Reference> {
    reference_frame_in(std::path::Path::new("."))
}

/// As `reference_frame`, with the repo root given explicitly - the run
/// check is a crate outside the workspace, so its working directory is
/// not the repo root.
pub fn reference_frame_in(root: &std::path::Path) -> Result<Reference> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::verify::{pm1_key, run_one, trace_frame};
    use super::{cart, iface};
    use anyhow::{anyhow, bail};

    let src = cart::sources_in(root)?;
    let top = full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?;
    let init = full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?;
    let reset = full_moon::parse("__reset_button_states()")
        .map_err(|e| anyhow!("parse reset: {:?}", e))?;
    let fr = full_moon::parse(cart::FRAME_CODE).map_err(|e| anyhow!("parse frame: {:?}", e))?;

    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = celeste_interp::game_runner::start_room();
    let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(
        &cart_data, rx, ry,
    )?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());

    let st = cart::fresh_state::<Symbolic>(&mut it.d);
    let mut st = run_one(&mut it, &top, st)?;
    cart::inject_tile_flag_at(&mut st);
    let mut st = run_one(&mut it, &init, st)?;
    let mut player: Option<super::iface::Path> = None;
    for _ in 0..40 {
        if let Some(p) = super::verify::find_player(&st) {
            player = Some(p);
            break;
        }
        st = run_one(&mut it, &fr, st)?;
    }
    let Some(player) = player else { bail!("no player after 40 frames") };

    let mut roots = vec![player.clone()];
    for g in ["freeze", "has_dashed", "frames", "will_restart", "delay_restart", "max_djump"] {
        roots.push(vec![iface::key(g)]);
    }
    let pin = pm1_key(&player, &st, &it.d)?;
    let frame = trace_frame(&mut it, &reset, &fr, st, &roots, &pin, &[], false)?;

    let graph = std::mem::take(&mut it.d.graph);
    let bound = super::emit::bind(&frame, &graph)?;
    let room = crate::transpile::graph::Room { cart: cart_data.clone(), cache: cache.clone() };
    let lowered = super::emit::lower_frame(
        &bound.graph,
        &bound.inputs,
        &bound.uni,
        &bound.outcomes,
        Some(room),
        bound.forks,
    )?;
    Ok(Reference { frame, graph, bound, lowered, cart: cart_data, cache })
}

/// One kernel per SHAPE the room reaches.
///
/// No pm1 pin: a kernel covers every key of its shape. The pin is worth
/// -6.3% (T13) and costs a kernel per key, and a fully symbolic frame is
/// 2,341 graph nodes against ~1,720 for the largest single outcome of a
/// pinned one - so un-pinning is close to free.
///
/// REFUSES rather than returns a partial set. A shape the walk could not
/// trace, or one it dropped at the cap, is a kernel that will not exist,
/// and under the never-deopt doctrine that is a run that stops. Better
/// to fail here, where the reason is in hand.
/// Every shape's traced frame, BOUND to the engine's numbering, plus the
/// arena they share. The half of `room_kernels_in` that does not lower -
/// separate so a diagnostic can look at the traced graph itself, which
/// is where the question "did the tracer build this node or did a fold"
/// gets answered.
pub(crate) fn room_shapes_in(
    root: &std::path::Path,
) -> Result<(
    Vec<super::shapes::Shape>,
    crate::transpile::graph::Graph,
    std::sync::Arc<celeste_core::cart_data::CartData>,
    std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
)> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::verify::run_one;
    use super::{cart, shapes};
    use anyhow::{anyhow, bail};

    let src = cart::sources_in(root)?;
    let top = full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?;
    let init = full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?;
    let reset = full_moon::parse("__reset_button_states()")
        .map_err(|e| anyhow!("parse reset: {:?}", e))?;
    let fr = full_moon::parse(cart::FRAME_CODE).map_err(|e| anyhow!("parse frame: {:?}", e))?;

    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = celeste_interp::game_runner::start_room();
    let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(
        &cart_data, rx, ry,
    )?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());

    let st = cart::fresh_state::<Symbolic>(&mut it.d);
    let mut st = run_one(&mut it, &top, st)?;
    cart::inject_tile_flag_at(&mut st);
    let st = run_one(&mut it, &init, st)?;

    let w = shapes::walk(&mut it, &reset, &fr, st, 400)?;
    if !w.refused.is_empty() {
        let why: Vec<String> =
            w.refused.iter().map(|(e, n)| format!("{} x {}", n, e)).collect();
        // The COUNTS matter as much as the reasons: "1 refusal against 12
        // traced shapes" is a hole to fill, "1 against 0" is a tracer
        // that never got started, and the message used to read the same
        // either way.
        bail!(
            "the shape walk could not trace every shape ({} traced, {} left the room, \
             {} unreachable):\n{}",
            w.shapes.len(),
            w.left_room,
            w.unreachable,
            why.join("\n")
        );
    }
    if w.dropped > 0 {
        bail!("the shape walk hit its cap with {} outcomes left", w.dropped);
    }

    let graph = std::mem::take(&mut it.d.graph);
    Ok((w.shapes, graph, cart_data, cache))
}

/// One kernel per heap shape the room reaches.
pub fn room_kernels_in(root: &std::path::Path) -> Result<Vec<Reference>> {
    let (shapes, graph, cart_data, cache) = room_shapes_in(root)?;
    let room = crate::transpile::graph::Room { cart: cart_data.clone(), cache: cache.clone() };
    let mut out = Vec::new();
    for sh in shapes {
        let frame = sh.frame;
        let bound = super::emit::bind(&frame, &graph)?;
        let lowered = super::emit::lower_frame(
            &bound.graph,
            &bound.inputs,
            &bound.uni,
            &bound.outcomes,
            Some(room.clone()),
            bound.forks,
        )
        .map_err(|e| name_cells(&frame, e))?;
        out.push(Reference {
            frame,
            // Every shape was traced into ONE arena so they share
            // subexpressions; each reference keeps a copy because the
            // check harness evaluates against it. Cheap - the whole
            // room's traced graph is a few thousand nodes.
            graph: graph.clone(),
            bound,
            lowered,
            cart: cart_data.clone(),
            cache: cache.clone(),
        });
    }
    Ok(out)
}

/// A block holding `rows` concrete input assignments, one per lane.
///
/// The kernel reads its inputs off a block by path, so checking it
/// against the graph needs a block that holds exactly the values the
/// graph is evaluated at. This builds one: the traced input shape, with
/// each row's values written into the cells the interface named.
pub fn input_block(r: &Reference, rows: &[Vec<super::iface::Conc>]) -> Result<celeste_engine::Rt2> {
    use super::iface::Conc;

    let mut b = celeste_engine::slots::reshape(&r.frame.in_rt2, rows.len());
    // The BLOCK-UNIFORM inputs go in as uniform columns, because that is
    // what the kernel's `bind` accepts - it takes them as `P8`, not as a
    // lane array, and a per-lane column there is a narrowing it refuses.
    // Every row therefore has to agree about them, and this says so
    // rather than silently taking row 0.
    let uni: std::collections::BTreeSet<u32> = r.bound.uni.iter().map(|(c, _)| *c).collect();
    for (i, cell) in r.frame.in_cells.iter().enumerate() {
        if !uni.contains(cell) {
            continue;
        }
        for (k, row) in rows.iter().enumerate() {
            if row[i] != rows[0][i] {
                anyhow::bail!(
                    "row {} disagrees with row 0 about the block-uniform slot {}",
                    k,
                    super::iface::show(&r.frame.iface.slots[i])
                );
            }
        }
        b.cols[*cell as usize] = Col::U(match rows[0][i] {
            Conc::Num(v) => AV::Num(v),
            Conc::Bool(v) => AV::Bool(v),
        });
    }
    for (i, cell) in r.frame.in_cells.iter().enumerate() {
        if uni.contains(cell) {
            continue;
        }
        let col = match rows[0].get(i) {
            Some(Conc::Num(_)) => Col::N(
                rows.iter()
                    .map(|row| match row[i] {
                        Conc::Num(v) => v,
                        Conc::Bool(_) => celeste_core::pico8_num::Pico8Num::from_i16(0),
                    })
                    .collect(),
            ),
            Some(Conc::Bool(_)) => Col::V(
                rows.iter()
                    .map(|row| match row[i] {
                        Conc::Bool(v) => AV::Bool(v),
                        Conc::Num(_) => AV::Nil,
                    })
                    .collect(),
            ),
            None => anyhow::bail!("row is shorter than the interface"),
        };
        b.cols[*cell as usize] = col;
    }
    Ok(b)
}

/// Write a kernel per shape, plus the table a dispatcher indexes.
///
/// Into a directory rather than a file because each shape's `Uni`,
/// `RowsIn` and `KOuts` are different types with the same names - they

/// Restate a lowering failure with its `Cell(n)`s NAMED.
///
/// The emitter works on the bound graph, where an input is a canonical
/// engine cell number and nothing downstream of the tracer knows what a
/// cell means. The interface does, so translating the numbers back is a
/// lookup - and the difference between "arms disagree in Sel(.., Cell(272))"
/// and knowing 272 is a particular object's field is the whole diagnosis.
fn name_cells(f: &super::verify::Frame, e: anyhow::Error) -> anyhow::Error {
    let msg = format!("{:#}", e);
    let mut seen: std::collections::BTreeSet<u32> = Default::default();
    let mut at = msg.as_str();
    while let Some(i) = at.find("Cell(") {
        at = &at[i + 5..];
        let end = at.find(')').unwrap_or(0);
        if let Ok(n) = at[..end].parse::<u32>() {
            seen.insert(n);
        }
    }
    let mut lines = Vec::new();
    for c in seen {
        let name = f
            .in_cells
            .iter()
            .position(|x| *x == c)
            .map(|i| super::iface::show(&f.iface.slots[i]))
            .unwrap_or_else(|| "not an input of this frame".to_string());
        lines.push(format!("  Cell({}) = {}", c, name));
    }
    anyhow::anyhow!("{}\nwhere\n{}", msg, lines.join("\n"))
}

/// coexist only as separate modules.
pub fn write_room_kernels(root: &std::path::Path, dir: &std::path::Path) -> Result<Vec<usize>> {
    let refs = room_kernels_in(root)?;
    std::fs::create_dir_all(dir)?;
    let mut sizes = Vec::new();
    let mut sources: Vec<String> = Vec::new();
    let mut decl = String::new();
    let mut table = String::new();
    for (i, r) in refs.iter().enumerate() {
        let src = render(&r.frame, &r.bound, &r.lowered, &format!("shape {}", i))?;
        sizes.push(src.lines().count());
        std::fs::write(dir.join(format!("kernel{}.rs", i)), &src)?;
        sources.push(src);
        writeln!(decl, "#[path = \"kernel{i}.rs\"]\npub mod k{i};", i = i)?;
        writeln!(
            table,
            "    Kernel {{ name: \"shape {i}\", shape: k{i}::SHAPE, \
             outcomes: k{i}::OUTCOMES, acc: k{i}::acc, step: k{i}::step, \
             why: k{i}::bind_why }},",
            i = i
        )?;
    }
    // The set's own content hash, the way the fused artifact carries
    // one. The class kernels get away without it because the compile
    // recipe TEXT is a faithful proxy for them and the campaign
    // fingerprint hashes that; nothing upstream of a traced kernel is a
    // file the fingerprint can read, so the kernels hash themselves.
    // Without it two campaigns with different traced sets would share
    // checkpoints.
    let fingerprint = {
        use std::hash::Hasher;
        let mut h = rustc_hash::FxHasher::default();
        for src in &sources {
            std::hash::Hash::hash(src, &mut h);
        }
        h.finish()
    };
    let mut m = String::new();
    writeln!(
        m,
        "// GENERATED by `trace::kernel::write_room_kernels`. Do not edit.\n\
         //\n\
         // One module per heap SHAPE the room reaches. Each shape's\n\
         // `Uni`, `RowsIn` and `KOuts` are different types with the same\n\
         // names, so they coexist only as separate modules - which is\n\
         // why `step` and `acc` exist: they are the uniform surface a\n\
         // dispatcher can hold as function pointers.\n\
         //\n\
         // `Kernel` is NOT declared here. It lives in\n\
         // `celeste_engine::traced` - a generated copy would have to be\n\
         // kept in step with the frame loops that consume it by hand,\n\
         // and regenerating is not a good moment to discover a\n\
         // signature changed.\n\
         //\n\
         // Naming the ENGINE\'s copy and not `celeste_rust`\'s re-export\n\
         // is what lets this set be checked in below `celeste-rust`: a\n\
         // kernel set that mentions the crate holding the emitters can\n\
         // only live above them.\n\
         #![allow(clippy::all)]\n\
         pub use celeste_engine::traced::Kernel;\n\
         \n\
         {decl}\n\
         pub const KERNELS: &[Kernel] = &[\n\
         {table}];\n\
         \n\
         /// Content hash of every kernel source in this set. Hashed into\n\
         /// the campaign fingerprint so two different traced sets can\n\
         /// never share a checkpoint.\n\
         pub const FINGERPRINT: u64 = {fingerprint};\n",
        decl = decl,
        table = table,
        fingerprint = fingerprint
    )?;
    std::fs::write(dir.join("mod.rs"), m)?;
    Ok(sizes)
}

#[cfg(test)]
mod tests {
    /// The gate that makes the CHECKED-IN traced set safe.
    ///
    /// `crates/celeste-kernels/src/traced/` is committed, so nothing in
    /// the build forces it to match the tracer that claims to produce
    /// it - and a stale kernel there does not fail to compile, it
    /// computes a frame the tracer no longer agrees with. This
    /// regenerates the whole set from the cart, the same way
    /// `regen-generated.sh` does, and compares byte for byte.
    ///
    /// Not `#[ignore]`d. It costs ~6 s, against the ~200 s and ~44 s
    /// that put the other regeneration tests behind `--ignored`, and it
    /// guards exactly what `generated_is_current` guards for the class
    /// kernels - which runs on every commit.
    #[test]
    fn traced_kernels_are_current() {
        let dir = std::env::temp_dir().join(format!("celeste-traced-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let sizes = super::write_room_kernels(std::path::Path::new("."), &dir)
            .unwrap_or_else(|e| panic!("regenerate the room kernel set (run from the repo root): {:#}", e));

        let committed = std::path::Path::new("crates/celeste-kernels/src/traced");
        let listing = |d: &std::path::Path| -> std::collections::BTreeSet<String> {
            std::fs::read_dir(d)
                .unwrap_or_else(|e| panic!("read {}: {}", d.display(), e))
                .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
                .collect()
        };
        // Both directions. One shape FEWER than last time leaves a
        // stale `kernelN.rs` behind that still compiles and is still
        // reachable through nothing - comparing only the files the
        // tracer just wrote would pass over it.
        let (fresh_files, on_disk_files) = (listing(&dir), listing(committed));
        assert_eq!(
            fresh_files,
            on_disk_files,
            "the traced kernel set has a different FILE LIST than what is committed \
             ({} shapes now). Run ./regen-generated.sh.",
            sizes.len()
        );

        for name in &fresh_files {
            let a = std::fs::read_to_string(committed.join(name)).unwrap();
            let b = std::fs::read_to_string(dir.join(name)).unwrap();
            if a != b {
                let first = a.lines().zip(b.lines()).position(|(x, y)| x != y).map(|i| i + 1);
                panic!(
                    "crates/celeste-kernels/src/traced/{} is STALE: on disk {} lines, \
                     tracer says {} lines, first differing line {:?}. Run \
                     ./regen-generated.sh and read the diff.",
                    name,
                    a.lines().count(),
                    b.lines().count(),
                    first
                );
            }
        }
        let _ = std::fs::remove_dir_all(&dir);
    }



    /// How much of the row amplification is STATICALLY removable?
    ///
    /// `variants` dedups on the whole `(outputs, live, ok)` tuple, so two
    /// button assignments that write identical values and differ only in
    /// which lanes they claim stay separate - and each appends its own
    /// copy of the same row. Grouping PER OUTCOME on `(outputs, ok, bd)`
    /// and unioning the live masks would remove those before they are
    /// written.
    ///
    /// This counts what that would save, from the expressions alone, so
    /// the restructuring is sized before it is built rather than after.
    #[test]
    #[ignore]
    fn how_many_variants_write_the_same_row() {
        let refs = match super::room_kernels_in(std::path::Path::new(".")) {
            Ok(r) => r,
            Err(e) => panic!("{:#}", e),
        };
        for (si, r) in refs.iter().enumerate() {
            let n = r.lowered.variants.len();
            let outcomes = r.lowered.outs.len();
            let mut per: Vec<usize> = Vec::new();
            for oi in 0..outcomes {
                // OUTPUTS only. `ok` is a per-variant variable NAME
                // (`ok_v{mask}`), so including it groups nothing - it
                // was 1.0x on the first attempt for exactly that reason.
                //
                // Dropping it is not a shortcut: the rows a variant
                // appends are `live & !deopt`, and two variants with
                // identical output VALUES append identical rows, so the
                // union of their take masks is exact whatever their `ok`
                // says. Deopt REPORTING still ORs per variant, which is
                // a separate quantity.
                let mut groups: std::collections::BTreeSet<String> = Default::default();
                for v in &r.lowered.variants {
                    let p = &v.per[oi];
                    let outs: Vec<String> =
                        p.outputs.iter().map(|(c, e)| format!("{}={}", c, e)).collect();
                    groups.insert(outs.join(";"));
                }
                per.push(groups.len());
            }
            let total: usize = per.iter().sum();
            eprintln!(
                "[dedup] shape {}: {} variants x {} outcomes = {} appends; \
                 grouped {:?} = {} ({:.1}x fewer)",
                si,
                n,
                outcomes,
                n * outcomes,
                per,
                total,
                (n * outcomes) as f64 / total.max(1) as f64
            );
        }
    }

    /// How much of a frame exists only to compute values the boundary
    /// ERASES a moment later?
    ///
    /// `player.rem.x/y` are overwritten with the widened constant, and
    /// the four timers are pinned to 0. The frame computes all of them -
    /// through the fork, in `rem`'s case - writes them into the output
    /// block, and the boundary discards them.
    ///
    /// Philippe's proposal is to compile the widenings INTO the kernel,
    /// at which point those chains are dead and can be deleted. This
    /// counts what that would remove.
    #[test]
    #[ignore]
    fn how_much_of_a_frame_is_erased_immediately() {
        use crate::transpile::graph::NodeId;
        let refs = match super::room_kernels_in(std::path::Path::new(".")) {
            Ok(r) => r,
            Err(e) => panic!("{:#}", e),
        };
        let erased = ["rem.x", "rem.y", "frames", "seconds", "minutes", "deaths"];
        for (si, r) in refs.iter().enumerate() {
            let g = &r.bound.graph;
            let reach = |roots: &[NodeId]| -> usize {
                let mut seen = vec![false; g.len()];
                let mut st = roots.to_vec();
                let mut n = 0;
                while let Some(x) = st.pop() {
                    if seen[x as usize] {
                        continue;
                    }
                    seen[x as usize] = true;
                    n += 1;
                    st.extend(g.get(x).args.iter().copied());
                }
                n
            };
            let mut all: Vec<NodeId> = Vec::new();
            let mut kept: Vec<NodeId> = Vec::new();
            let mut dropped = 0;
            for (oi, o) in r.bound.outcomes.iter().enumerate() {
                for (k, (cell, node, _)) in o.outputs.iter().enumerate() {
                    let _ = cell;
                    all.push(*node);
                    let path = crate::trace::iface::show(&r.frame.outs[oi].fields[k].0);
                    if erased.iter().any(|e| path.ends_with(e)) {
                        dropped += 1;
                    } else {
                        kept.push(*node);
                    }
                }
                all.push(o.live);
                all.push(o.ok);
                kept.push(o.live);
                kept.push(o.ok);
            }
            let (a, k) = (reach(&all), reach(&kept));
            eprintln!(
                "[erase] shape {}: {} nodes reachable from all outputs, {} without the {} \
                 erased cells -> {} nodes ({:.1}%) exist only for values the boundary discards",
                si, a, k, dropped, a - k, 100.0 * (a - k) as f64 / a as f64
            );
        }
    }

    /// What specializing the fork WOULD cost, in nodes - the measurement
    /// that refuted it (2026-08-23).
    ///
    /// The runtime loop runs the tail of the frame once per fork
    /// configuration; specialization runs each configuration's nodes
    /// once, sharing everything the configurations agree on. Which is
    /// cheaper is entirely a question of how much of the frame is
    /// downstream of the split, and that is a reachability fact this
    /// answers before anything is built.
    ///
    /// Reported per shape: the specialized arena's live node count with
    /// the splits left standing (what is emitted today, inside a loop)
    /// against the same count with all 2^forks configurations resolved.
    ///
    /// The room (1,0) answer - 3.92x of a maximum 4.00x, 1.9% shared -
    /// was WRONG TWICE, and the wrongness is the reason this prints as
    /// much as it does now. It ran on the only room that existed, which
    /// forks twice and is the weakest point on the curve; and it blended
    /// all 64 button assignments into the arena, so button divergence
    /// dominated and the fork question was never actually asked.
    ///
    /// Holding the buttons fixed and running room (2,0), the ratio is
    /// flat at ~2.3-3.2x while the configuration count goes 4 -> 16 ->
    /// 256, i.e. sharing goes 40.6% -> 82-86% -> 98.7-99.0%.
    /// Specialization gets CHEAPER, relatively, the more forks there
    /// are.
    ///
    /// Node counts alone do not decide it, so this also reports the
    /// runtime comparison. The loop re-executes every node above its
    /// fork level once per configuration, so its work is
    /// `sum_l count_l * 2^l`; the flat form executes each node once, so
    /// its work IS its node count. Those two are the numbers that
    /// matter, and the level histogram says where a shape sits between
    /// them.
    #[test]
    #[ignore]
    fn what_specializing_the_fork_would_cost() {
        use crate::transpile::graph::{Graph, NodeId};
        let refs = match super::room_kernels_in(std::path::Path::new(".")) {
            Ok(r) => r,
            Err(e) => panic!("{:#}", e),
        };
        for (si, r) in refs.iter().enumerate() {
            let g = &r.bound.graph;
            let forks = r.bound.forks;
            let base: Vec<NodeId> = {
                let mut v = Vec::new();
                for o in r.bound.outcomes.iter() {
                    v.extend(o.outputs.iter().map(|(_, n, _)| *n));
                    v.push(o.live);
                    v.push(o.ok);
                }
                v
            };
            let live_count = |arena: &Graph, roots: &[NodeId]| -> usize {
                crate::transpile::bdd::reachable(arena, roots)
                    .iter()
                    .filter(|b| **b)
                    .count()
            };
            // Today: ONE assignment, splits left as nodes. One rather
            // than all 64, because the question is what the FORK
            // configurations share, and holding the buttons fixed is
            // what isolates it - mixing both makes the ratio a blend of
            // two effects.
            let mut loop_arena = Graph::new();
            let mut loop_roots: Vec<NodeId> = Vec::new();
            {
                let map = g.specialize_config_into(0, None, &mut loop_arena);
                loop_roots.extend(base.iter().map(|x| map[*x as usize]));
            }
            // Specialized: ONE assignment x 2^forks configurations.
            //
            // `let ns: u8 = 1 << forks` was the third member of today's
            // shift-overflow family (see `ChoiceSet`): at 8 forks it
            // wraps to 0 and the inner loop does not run, so the test
            // would have reported perfect sharing for the shapes that
            // share least. It only ever ran on room (1,0), where forks
            // is 2, so it was never wrong in practice - which is
            // exactly how the ChoiceSet one survived too.
            //
            // Enumeration is 2^forks, so it is capped and the cap is
            // ANNOUNCED. A silent skip here would read as "measured and
            // fine" for precisely the shapes the question is about.
            const MAX_FORKS: u8 = 14;
            if forks > MAX_FORKS {
                eprintln!(
                    "[fork] shape {}: {} forks - SKIPPED, 2^{} configurations is past \
                     the {} cap. Not measured, not zero.",
                    si, forks, forks, MAX_FORKS
                );
                continue;
            }
            let ns: u64 = 1u64 << forks;
            let mut flat_arena = Graph::new();
            let mut flat_roots: Vec<NodeId> = Vec::new();
            // Configurations that FOLD AWAY. Resolving a split turns
            // `FragOk` into ordinary arithmetic, and arithmetic folds:
            // a configuration whose every outcome is statically dead is
            // one the flat form never emits, while the runtime loop
            // still enters it and discovers `valid == 0`. So this is
            // both a discount on the flat size and a count of trips the
            // loop takes for nothing.
            let mut dead = 0u64;
            let mut live_roots: Vec<NodeId> = Vec::new();
            // How many DISTINCT successors the configurations produce.
            //
            // Sharing the compute is only half a flat kernel: each
            // configuration also has to WRITE its row, and the write
            // does not hash-cons - it is emitted code. But two
            // configurations that land on the same output tuple are the
            // same successor for every lane (the same argument
            // `specialize_into` makes for buttons), so they need ONE
            // write between them. This counts the tuples, which is the
            // number of write sites a flat kernel actually needs, as
            // against `ns` if none of them coincide.
            let mut tuples: std::collections::HashSet<Vec<NodeId>> =
                std::collections::HashSet::new();
            for c in 0..ns {
                let map = g.specialize_config_into(0, Some(c), &mut flat_arena);
                let alive = r.bound.outcomes.iter().any(|o| {
                    !matches!(
                        flat_arena.get(map[o.live as usize]).op,
                        crate::transpile::graph::Op::ConstBool(false)
                    )
                });
                if alive {
                    live_roots.extend(base.iter().map(|x| map[*x as usize]));
                    tuples.insert(base.iter().map(|x| map[*x as usize]).collect());
                } else {
                    dead += 1;
                }
                flat_roots.extend(base.iter().map(|x| map[*x as usize]));
            }
            let (a, b) = (
                live_count(&loop_arena, &loop_roots),
                live_count(&flat_arena, &flat_roots),
            );
            let b_live = live_count(&flat_arena, &live_roots);
            eprintln!(
                "[fork]   shape {}: {} of {} configurations fold away \
                 ({:.1}%); flat nodes {} -> {} once they are dropped",
                si,
                dead,
                ns,
                100.0 * dead as f64 / ns as f64,
                b,
                b_live,
            );
            eprintln!(
                "[fork]   shape {}: {} distinct successors from {} configurations \
                 ({} write sites, not {})",
                si,
                tuples.len(),
                ns,
                tuples.len(),
                ns,
            );

            // WHERE each fork comes from.
            //
            // `Iface::ival` says the only INTERVAL input is the player's
            // `rem.x`/`rem.y`, so a shape forking 14 times is forking on
            // twelve values that became intervals during the frame. That
            // is either six more objects whose motion genuinely inherits
            // the player's uncertainty, or an imprecision worth
            // narrowing - and the difference is the difference between
            // 16,384 real successors and 16,384 spurious ones. Naming
            // the cells under each fork is what tells them apart.
            let mut in_path: std::collections::BTreeMap<u32, String> =
                std::collections::BTreeMap::new();
            for (i, cell) in r.frame.in_cells.iter().enumerate() {
                in_path.insert(*cell, crate::trace::iface::show(&r.frame.iface.slots[i]));
            }
            let ivals: Vec<String> = r
                .frame
                .iface
                .ival
                .iter()
                .enumerate()
                .filter(|(_, b)| **b)
                .map(|(i, _)| crate::trace::iface::show(&r.frame.iface.slots[i]))
                .collect();
            eprintln!("[fork]   shape {}: interval INPUTS are {:?}", si, ivals);
            for d in 0..forks {
                let split = (0..g.len() as NodeId)
                    .find(|id| matches!(g.get(*id).op, crate::transpile::graph::Op::Split(x) if x == d));
                let Some(split) = split else {
                    eprintln!("[fork]   shape {} fork {}: no Split node (folded away)", si, d);
                    continue;
                };
                let operand = g.get(split).args[0];
                let reach = crate::transpile::bdd::reachable(g, &[operand]);
                let mut objs: Vec<String> = Vec::new();
                let mut n_cells = 0usize;
                for id in 0..g.len() as NodeId {
                    if !reach[id as usize] {
                        continue;
                    }
                    if let crate::transpile::graph::Op::Cell(c) = g.get(id).op {
                        if let Some(p) = in_path.get(&c) {
                            n_cells += 1;
                            // "objects[3].spd.y" -> "objects[3]"
                            let head = match p.find(']') {
                                Some(k) => p[..=k].to_string(),
                                None => p.clone(),
                            };
                            objs.push(head);
                        }
                    }
                }
                objs.sort();
                objs.dedup();
                eprintln!(
                    "[fork]   shape {} fork {}: {} input cells under it, from {:?}",
                    si, d, n_cells, objs
                );
            }

            // The other direction, which is the one that names an
            // object: not "what does this fork read" but "whose
            // position does it write". A fork reads most of the state
            // (every object's motion is guarded by the same globals),
            // so the read cone identifies nothing; the WRITE cone does,
            // because `objects[k].rem.x` is written by exactly the
            // `move` call on object k.
            //
            // `bound.outcomes` and `frame.outs` are the same list in the
            // same order - `bind` renumbers cells, not roots - so the
            // node ids come from the first and the paths from the
            // second.
            let cones = g.split_cones();
            for (oi, o) in r.bound.outcomes.iter().enumerate() {
                let paths = &r.frame.outs[oi].fields;
                assert_eq!(
                    paths.len(),
                    o.outputs.len(),
                    "outcome {} has {} paths but {} outputs - the two lists are \
                     supposed to be the same roots in the same order",
                    oi,
                    paths.len(),
                    o.outputs.len()
                );
                let mut by_obj: std::collections::BTreeMap<String, u64> =
                    std::collections::BTreeMap::new();
                for ((path, _, _), (_, node, _)) in paths.iter().zip(o.outputs.iter()) {
                    let p = crate::trace::iface::show(path);
                    let head = match p.find(']') {
                        Some(k) => p[..=k].to_string(),
                        None => p.clone(),
                    };
                    *by_obj.entry(head).or_insert(0) |= cones[*node as usize];
                }
                for (obj, mask) in by_obj {
                    if mask == 0 {
                        continue;
                    }
                    let which: Vec<u8> = (0..forks).filter(|d| mask & (1 << d) != 0).collect();
                    eprintln!(
                        "[fork]   shape {} outcome {}: {} depends on forks {:?}",
                        si, oi, obj, which
                    );
                }
                // The whole outcome, VALUES AND MASKS. `live` and `ok`
                // matter as much as the fields: an outcome whose values
                // ignore every fork can still be gated on fork
                // validity, and an outcome is only cheap to specialize
                // if the mask is cheap too.
                let whole = o
                    .outputs
                    .iter()
                    .map(|(_, n, _)| cones[*n as usize])
                    .chain([cones[o.live as usize], cones[o.ok as usize]])
                    .fold(0u64, |a, b| a | b);
                let which: Vec<u8> = (0..forks).filter(|d| whole & (1 << d) != 0).collect();
                eprintln!(
                    "[fork]   OUTCOME shape {} outcome {}: {} of {} forks {:?} \
                     -> {} configurations",
                    si,
                    oi,
                    which.len(),
                    forks,
                    which,
                    1u64 << which.len()
                );
            }

            // WHERE the loop's nodes sit, which is what decides its
            // runtime: a node whose split cone's highest bit is `d`
            // lives inside loops 0..=d and is therefore executed up to
            // 2^(d+1) times. Same `level` the emitter places by
            // (`transpile::lower`), so this histogram is literally the
            // shape of the emitted loop nest.
            let live_loop = crate::transpile::bdd::reachable(&loop_arena, &loop_roots);
            let scone = loop_arena.split_cones();
            let mut per_level = vec![0usize; forks as usize + 1];
            for id in 0..loop_arena.len() as NodeId {
                if !live_loop[id as usize] {
                    continue;
                }
                let m = scone[id as usize];
                let lvl = if m == 0 {
                    0
                } else {
                    (crate::transpile::graph::ChoiceSet::BITS - m.leading_zeros()) as usize
                };
                per_level[lvl.min(forks as usize)] += 1;
            }
            // The loop's WORK, with no pruning: each level's nodes run
            // once per configuration of the forks below them. This is an
            // upper bound - `if valid == 0 { continue }` skips
            // configurations no lane reaches - so a flat form cheaper
            // than this is not yet proof, but a flat form cheaper than
            // the loop's SIZE times a handful is a strong hint.
            let loop_work: u64 = per_level
                .iter()
                .enumerate()
                .map(|(l, n)| *n as u64 * (1u64 << l))
                .sum();
            eprintln!(
                "[fork]   shape {} levels {:?}; loop work <= {} node-evals, \
                 flat work = {} ({:.2}x cheaper at most)",
                si,
                per_level,
                loop_work,
                b,
                loop_work as f64 / b.max(1) as f64,
            );
            // The two are equal in WORK - the loop evaluates its tail
            // up to 2^forks times, specialization evaluates 2^forks
            // copies once each - so the ratio is pure code size, and
            // anything near 2^forks means the configurations share
            // nothing and the trade is all cost.
            eprintln!(
                "[fork] shape {}: {} forks, traced graph {} nodes; \
                 emitted live nodes {} (loop) -> {} (specialized), {:.2}x \
                 of a maximum {:.2}x, so {:.1}% shared.",
                si,
                forks,
                g.len(),
                a,
                b,
                b as f64 / a.max(1) as f64,
                ns as f64,
                100.0 * (1.0 - b as f64 / (ns as f64 * a.max(1) as f64)),
            );
        }
    }

}
