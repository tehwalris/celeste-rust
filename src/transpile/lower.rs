//! Graph -> emitted kernel body (`plans/multi-output-fusion.md`, P1').
//!
//! The emitter's walk (`transpile::kernel::emit_walk`) evaluates `__frame`
//! against the shape witness and, as it goes, records what each value MEANS
//! as a node in `transpile::graph`. This module turns that graph back into
//! the lines of a class kernel. The walk decides semantics; this decides
//! representation and placement, and nothing else.
//!
//! Three things the old text-streaming emitter conflated are separate here,
//! and that separation is the whole point:
//!
//! * **Representation is DERIVED, not chosen.** Whether a value is a
//!   block-uniform `P8` or a 16-lane `ZN`, an exact number or an interval,
//!   is a fixed point over the graph: uniform until a per-lane input
//!   reaches it, exact until an interval does. `zn_splat`/`zi_of_zn` and
//!   friends are then COERCIONS at use sites, not nodes - which is why the
//!   graph has ~35% fewer nodes than the emitted text had lets.
//!
//! * **Placement is DERIVED too.** A node sits at the shallowest scope its
//!   operands allow. Both scopes it can sit in - the split loop nest and
//!   the free-choice suffix - are the SAME question, "which choices
//!   reach this node", answered once by `Graph::choice_cones`; the old
//!   "prefix/suffix taint" was one hand-maintained special case of it.
//!
//! * **Validity is a VALUE, all the way down.** Every condition under
//!   which the abstract domain gave up is a conjunct of the walk's `ok`
//!   node, and every conjunct is an ordinary node like any other. There
//!   is no `&mut dp` and no `*bd` side channel: the member's deopt mask
//!   is `!(m1 & m2 & ..)` over the per-lane conjuncts and its block-level
//!   bail is `u1 || u2 || ..` over the uniform ones, both plain
//!   expressions over emitted values.
//!
//!   This is what makes specializing the graph possible at all. A node
//!   shared between two variants cannot carry a side effect that belongs
//!   to only one of them, so as long as `zsel_n(.., &mut dp)` existed,
//!   sharing a select across variants would have merged their deopt
//!   masks. The engine primitives lost their deopt parameters for exactly
//!   this reason; the premises they used to enforce internally are now
//!   conjuncts (`zi_flr_ok`, `zi_span_ok`, `Known(c)`, `b > 0`).

use std::collections::{BTreeMap, BTreeSet};

use anyhow::{bail, Result};

use super::graph::{Graph, NodeId, Op};
use super::kernel::{Emit, Line, OutFields};

/// Which of the two value domains a node lives in.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Dom {
    Num,
    Bool,
}

/// A node's runtime representation. Both flags are over-approximations in
/// the safe direction: `lane` false means the value is PROVABLY the same in
/// every lane, `wide` false means it is PROVABLY a single element.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct Repr {
    dom: Dom,
    /// Per-lane (16 rows per slice) rather than block-uniform.
    lane: bool,
    /// An interval (numbers) or a tri-state (booleans) rather than exact.
    wide: bool,
}

impl Repr {
    fn num(lane: bool, wide: bool) -> Self {
        Repr { dom: Dom::Num, lane, wide }
    }
    fn boolean(lane: bool, wide: bool) -> Self {
        Repr { dom: Dom::Bool, lane, wide }
    }
    /// The generated Rust type. A per-lane boolean is always `ZB`, which
    /// carries its own `known` mask, so `wide` does not change the type
    /// there - only what the guards have to say about it.
    pub(crate) fn ty(self) -> &'static str {
        match (self.dom, self.lane, self.wide) {
            (Dom::Num, false, false) => "P8",
            (Dom::Num, false, true) => "(P8, P8)",
            (Dom::Num, true, false) => "ZN",
            (Dom::Num, true, true) => "ZI",
            (Dom::Bool, false, false) => "bool",
            (Dom::Bool, false, true) => "Option<bool>",
            (Dom::Bool, true, _) => "ZB",
        }
    }
    /// Is `self` at least as coarse as `other` - can an `other` value be
    /// coerced up to it without losing information?
    fn admits(self, other: Repr) -> bool {
        self.dom == other.dom && self.lane >= other.lane && self.wide >= other.wide
    }
}

/// Coerce an expression of representation `from` to `to`. Every case here
/// is a widening: a splat repeats a uniform value across the lanes, an
/// interval lift makes a singleton interval. There is no narrowing - a
/// caller that wants one has a derivation bug, and gets an error.
fn coerce(expr: &str, from: Repr, to: Repr) -> Result<String> {
    // Same generated type, nothing to do. This is not the same test as
    // `from == to`: a per-lane boolean carries its own `known` mask, so
    // `ZB` covers both widths and only the guards can tell them apart.
    if from.ty() == to.ty() {
        return Ok(expr.to_string());
    }
    if !to.admits(from) {
        bail!("cannot represent a {} as a {}", from.ty(), to.ty());
    }
    Ok(match (to.dom, from.lane, from.wide, to.lane, to.wide) {
        (Dom::Num, false, false, false, true) => format!("({e}, {e})", e = expr),
        (Dom::Num, false, false, true, false) => format!("zn_splat({})", expr),
        (Dom::Num, false, false, true, true) => format!("zi_splat({e}, {e})", e = expr),
        (Dom::Num, false, true, true, true) => format!("zi_splat({e}.0, {e}.1)", e = expr),
        (Dom::Num, true, false, true, true) => format!("zi_of_zn({})", expr),
        (Dom::Bool, false, false, false, true) => format!("Some({})", expr),
        (Dom::Bool, false, false, true, _) => format!("zb_splat({})", expr),
        // A block-uniform tri-state broadcast into lanes: undecided becomes
        // every lane undecided, which is what the guards then act on. No
        // class kernel needs this today - the walk decides a uniform
        // tri-state before it reaches a per-lane site - but leaving the
        // table with a hole in it means the next member that DOES need it
        // fails at emit time with "no coercion" instead of just working.
        (Dom::Bool, false, true, true, _) => format!(
            "ZB {{ val: if {e} == Some(true) {{ ALL }} else {{ 0 }},              known: if {e}.is_some() {{ ALL }} else {{ 0 }} }}",
            e = expr
        ),
        _ => bail!("no coercion from {} to {}", from.ty(), to.ty()),
    })
}

struct Ctx<'a> {
    g: &'a Graph,
    /// Witness cell kinds, for the leaf representations.
    uni: &'a BTreeMap<u32, &'static str>,
    vary: &'a BTreeMap<u32, &'static str>,
    repr: Vec<Repr>,
    name: Vec<Option<String>>,
    expr: Vec<Option<String>>,
}

impl<'a> Ctx<'a> {
    fn r(&self, id: NodeId) -> Repr {
        self.repr[id as usize]
    }
    fn args(&self, id: NodeId) -> &'a [NodeId] {
        &self.g.get(id).args
    }
    /// Read operand `k` of `id` at representation `want`.
    fn at(&self, id: NodeId, k: usize, want: Repr) -> Result<String> {
        let a = self.args(id)[k];
        let e = self.expr[a as usize]
            .as_deref()
            .ok_or_else(|| anyhow::anyhow!("node {} reads unemitted node {}", id, a))?;
        coerce(e, self.r(a), want).map_err(|err| {
            // WHICH node, and which operand. A bare "cannot represent a
            // ZN as a P8" names neither, and the answer is always a
            // specific site.
            anyhow::anyhow!(
                "{} (operand {} of node {}, {:?}, whose operand is {})",
                err,
                k,
                id,
                self.g.get(id).op,
                crate::trace::emit::show_tree(self.g, a, 3)
            )
        })
    }
    /// Read operand `k` at its own representation.
    fn raw(&self, id: NodeId, k: usize) -> Result<String> {
        let a = self.args(id)[k];
        self.expr[a as usize]
            .as_deref()
            .map(|s| s.to_string())
            .ok_or_else(|| anyhow::anyhow!("node {} reads unemitted node {}", id, a))
    }

    /// The representation of a node, from its op and its operands'. This is
    /// the abstract interpreter's own type rule, written once instead of
    /// once per emit site.
    fn derive(&self, id: NodeId) -> Result<Repr> {
        let op = &self.g.get(id).op;
        let a = self.args(id);
        // The common numeric shape: per-lane if any operand is, interval if
        // any operand is.
        let joined = |dom: Dom| -> Repr {
            Repr {
                dom,
                lane: a.iter().any(|x| self.r(*x).lane),
                wide: a.iter().any(|x| self.r(*x).wide),
            }
        };
        Ok(match op {
            Op::Const(lo, hi) => Repr::num(false, lo != hi),
            Op::ConstBool(_) | Op::Free(_) => Repr::boolean(false, false),
            Op::Cell(c) => {
                if let Some(kind) = self.vary.get(c) {
                    match *kind {
                        "num" => Repr::num(true, false),
                        // A per-lane INTERVAL: `player.rem`, which the
                        // boundary widens. The walk only ever had
                        // block-uniform ones (below), because its
                        // widening is the same for every lane of a
                        // chunk; a traced block's lanes each carry their
                        // own.
                        "ival" => Repr::num(true, true),
                        "bool" => Repr::boolean(true, false),
                        other => bail!("varying cell {} has kind {:?}", c, other),
                    }
                } else if let Some(kind) = self.uni.get(c) {
                    match *kind {
                        "num" => Repr::num(false, false),
                        "ival" => Repr::num(false, true),
                        "bool" => Repr::boolean(false, false),
                        other => bail!("uniform cell {} has kind {:?}", c, other),
                    }
                } else {
                    bail!("cell {} is neither a uniform nor a varying witness input", c)
                }
            }
            // A split narrows an interval; the fragment is still an interval.
            Op::Split(_) => Repr::num(true, true),
            Op::SplitValid(_) => Repr::boolean(true, false),
            Op::SplitOk => Repr::boolean(true, false),
            Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Neg | Op::Abs | Op::Min
            | Op::Max => joined(Dom::Num),
            // flr of an interval is exact BY GUARD: the lane survives only
            // where the floor is unique, and that condition is a conjunct
            // of `ok`. So this is not an unchecked narrowing - it is the
            // conjunct's postcondition, and `Known(Flr(x))` is rendered
            // from the INTERVAL rather than from this result precisely so
            // that the premise does not answer itself.
            Op::Flr => Repr::num(self.r(a[0]).lane, false),
            // sin over an inexact input never reaches here: the walk
            // replaces it with sin's RANGE as a constant.
            Op::Sin => Repr::num(self.r(a[0]).lane, false),
            Op::Mget => Repr::num(joined(Dom::Num).lane, false),
            Op::TileFlagAt => Repr::boolean(
                a.iter().take(4).any(|x| self.r(*x).lane),
                false,
            ),
            Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => joined(Dom::Bool),
            Op::Not => self.r(a[0]),
            // Both are DECIDED whatever their operands are: `zb_and` and
            // `zb_or` resolve the Kleene short-circuit cases, and the
            // `Known(x) AND x` idiom is decided by construction.
            Op::And | Op::Or => Repr::boolean(a.iter().any(|x| self.r(*x).lane), false),
            Op::Sel => {
                let (t, f) = (self.r(a[1]), self.r(a[2]));
                if t.dom != f.dom {
                    bail!(
                        "select arms disagree on domain: {} vs {} in {}",
                        t.ty(),
                        f.ty(),
                        crate::trace::emit::show_tree(self.g, id, 4)
                    );
                }
                Repr {
                    dom: t.dom,
                    lane: self.r(a[0]).lane || t.lane || f.lane,
                    wide: t.wide || f.wide,
                }
            }
            Op::Known => Repr::boolean(self.r(a[0]).lane, false),
        })
    }

    /// A two-operand tri-state boolean, in all three representations.
    /// AND and OR differ in exactly one thing: which value DOMINATES -
    /// decides the result on its own, known or not - so that is the
    /// parameter and the rest is shared.
    fn render_bool2(
        &self,
        id: NodeId,
        out: Repr,
        zop: &str,
        sop: &str,
        dominant: bool,
    ) -> Result<String> {
        let (x, y) = (self.at(id, 0, out)?, self.at(id, 1, out)?);
        Ok(match (out.lane, out.wide) {
            (true, _) => format!("{}({}, {})", zop, x, y),
            // No parentheses: `inline` returns Some only for LEAVES, so
            // every non-leaf node gets its own `let` and both operands
            // are atomic - a bound name, a literal or a coercion call.
            // There is therefore no `a || b && c` to misparse, and the
            // parentheses would only be `unused_parens` warnings in the
            // generated crate, which has to build clean.
            (false, false) => format!("{} {} {}", x, sop, y),
            (false, true) => format!(
                "(if {a} == Some({d}) || {b} == Some({d}) {{ Some({d}) }} \
                 else if {a}.is_some() && {b}.is_some() {{ Some({nd}) }} \
                 else {{ None }})",
                a = x,
                b = y,
                d = dominant,
                nd = !dominant
            ),
        })
    }

    /// The RHS of node `id`'s `let`.
    fn render(&self, id: NodeId) -> Result<String> {
        let op = self.g.get(id).op.clone();
        let out = self.r(id);
        let lane = out.lane;
        let a = self.args(id).to_vec();
        Ok(match op {
            Op::Add | Op::Sub | Op::Min | Op::Max => {
                let (x, y) = (self.at(id, 0, out)?, self.at(id, 1, out)?);
                let (zn, zi, sn, si) = match op {
                    Op::Add => ("zn_add", "zi_add", "+", "si_add"),
                    Op::Sub => ("zn_sub", "zi_sub", "-", "si_sub"),
                    Op::Min => ("zn_min", "zi_min", "min", ""),
                    Op::Max => ("zn_max", "zi_max", "max", ""),
                    _ => unreachable!(),
                };
                match (lane, out.wide) {
                    (true, false) => format!("{}({}, {})", zn, x, y),
                    (true, true) => format!("{}({}, {})", zi, x, y),
                    (false, false) if si.is_empty() => format!("{}.{}({})", x, sn, y),
                    (false, false) => format!("{} {} {}", x, sn, y),
                    (false, true) if si.is_empty() => {
                        format!("({x}.0.{f}({y}.0), {x}.1.{f}({y}.1))", x = x, y = y, f = sn)
                    }
                    (false, true) => format!("{}({}, {})", si, x, y),
                }
            }
            // Multiply and divide have an interval arm only for a POSITIVE
            // scalar (the interval helpers are monotone only there), which
            // is why the walk records `scalar > 0` as an ok conjunct.
            Op::Mul | Op::Div if out.wide => {
                let iv = Repr::num(lane, true);
                let num = Repr::num(lane, false);
                let (x, y) = (self.at(id, 0, iv)?, self.at(id, 1, num)?);
                let (zf, sf) = if matches!(op, Op::Mul) {
                    ("zi_mul_pos", "scale_positive")
                } else {
                    ("zi_div_pos", "div_positive")
                };
                if lane {
                    format!("{}({}, {})", zf, x, y)
                } else {
                    format!(
                        "{{ let r = IV::new({x}.0, {x}.1).{f}({y}); (r.low, r.high) }}",
                        x = x, y = y, f = sf
                    )
                }
            }
            Op::Mul | Op::Div | Op::Rem => {
                if out.wide {
                    bail!("{:?} of intervals has no lowering (only interval * positive num)", op);
                }
                let (x, y) = (self.at(id, 0, out)?, self.at(id, 1, out)?);
                let (zn, sn) = match op {
                    Op::Mul => ("zn_mul", "*"),
                    Op::Div => ("zn_div", "/"),
                    _ => ("zn_rem", "%"),
                };
                if lane {
                    format!("{}({}, {})", zn, x, y)
                } else {
                    format!("{} {} {}", x, sn, y)
                }
            }
            Op::Neg => {
                let x = self.at(id, 0, out)?;
                match (lane, out.wide) {
                    (true, false) => format!("zn_neg({})", x),
                    (true, true) => format!("zi_neg({})", x),
                    (false, false) => format!("-{}", x),
                    (false, true) => format!("(-{x}.1, -{x}.0)", x = x),
                }
            }
            Op::Abs => {
                let x = self.at(id, 0, out)?;
                match (lane, out.wide) {
                    (true, false) => format!("zn_abs({})", x),
                    (true, true) => format!("zi_abs({})", x),
                    (false, false) => format!("{}.abs()", x),
                    (false, true) => bail!("abs of a block-uniform interval has no lowering"),
                }
            }
            Op::Flr => {
                let src = self.r(a[0]);
                match (src.lane, src.wide) {
                    (true, true) => format!("zi_flr({})", self.raw(id, 0)?),
                    (true, false) => format!("zn_flr({})", self.raw(id, 0)?),
                    // The uniform interval's guard is a separate line; here
                    // the low endpoint's floor IS the floor, given it.
                    (false, true) => format!("{}.0.flr()", self.raw(id, 0)?),
                    (false, false) => format!("{}.flr()", self.raw(id, 0)?),
                }
            }
            Op::Sin => {
                let x = self.at(id, 0, out)?;
                if lane {
                    format!("zn_sin({})", x)
                } else {
                    format!("{}.pico8_sin()", x)
                }
            }
            Op::Mget => {
                let n = Repr::num(lane, false);
                let (x, y) = (self.at(id, 0, n)?, self.at(id, 1, n)?);
                if lane {
                    format!("zn_mget(g.cart, {}, {})", x, y)
                } else {
                    format!("P8::from_i16(g.cart.mget({}, {}).expect(\"mget\") as i16)", x, y)
                }
            }
            Op::TileFlagAt => {
                // x/y vary. The BOX is block-uniform in almost every
                // frame - a hitbox is fixed per object type - and the
                // walk lifts it with `sn`. It is not uniform in the frame
                // an object is CREATED: `init_object` gives it a default
                // 8x8 which `type.init` may or may not replace, so the
                // width arrives as a select on a per-lane condition.
                //
                // Decided from the operands rather than assumed, and the
                // uniform form is kept because `solid_map` is keyed by
                // (w, h) and a per-lane box cannot use it.
                let u = Repr::num(false, false);
                let z = Repr::num(true, false);
                let f = self.at(id, 4, u)?;
                let (x, y) = (self.at(id, 0, z)?, self.at(id, 1, z)?);
                let box_lane = self.r(self.args(id)[2]).lane || self.r(self.args(id)[3]).lane;
                let (call, w, h) = if box_lane {
                    ("zn_tile_flag_at_lanes", self.at(id, 2, z)?, self.at(id, 3, z)?)
                } else {
                    ("zn_tile_flag_at", self.at(id, 2, u)?, self.at(id, 3, u)?)
                };
                if lane {
                    format!("{}(g.cache, g.cart, {}, {}, {}, {}, {})", call, x, y, w, h, f)
                } else {
                    format!(
                        "{{ let z = {}(g.cache, g.cart, {}, {}, {}, {}, {}); \
                         z.val & 1 != 0 }}",
                        call, x, y, w, h, f
                    )
                }
            }
            Op::Lt | Op::Le | Op::Gt | Op::Ge => {
                let sub = Repr::num(
                    self.r(a[0]).lane || self.r(a[1]).lane,
                    self.r(a[0]).wide || self.r(a[1]).wide,
                );
                let (x, y) = (self.at(id, 0, sub)?, self.at(id, 1, sub)?);
                let (cmp, zn, sym) = match op {
                    Op::Lt => ("Lt", "zn_lt", "<"),
                    Op::Le => ("Le", "zn_le", "<="),
                    Op::Gt => ("Gt", "zn_gt", ">"),
                    _ => ("Ge", "zn_ge", ">="),
                };
                match (sub.lane, sub.wide) {
                    (true, true) => format!("zi_cmp(Cmp::{}, {}, {})", cmp, x, y),
                    (true, false) => format!("{}({}, {})", zn, x, y),
                    (false, true) => format!("si_cmp(Cmp::{}, {}, {})", cmp, x, y),
                    (false, false) => format!("{} {} {}", x, sym, y),
                }
            }
            Op::Eq => {
                let da = self.r(a[0]).dom;
                let sub = Repr {
                    dom: da,
                    lane: self.r(a[0]).lane || self.r(a[1]).lane,
                    wide: self.r(a[0]).wide || self.r(a[1]).wide,
                };
                if da == Dom::Num && sub.wide {
                    // The walk folds interval equality to `false` at emit
                    // time (two intervals are never EQUAL as abstract
                    // values), so there is nothing to lower here.
                    bail!("equality on intervals has no lowering");
                }
                let (x, y) = (self.at(id, 0, sub)?, self.at(id, 1, sub)?);
                match (da, sub.lane) {
                    (Dom::Num, true) => format!("zn_eq({}, {})", x, y),
                    (Dom::Bool, true) => format!("zb_eq({}, {})", x, y),
                    (_, false) => format!("{} == {}", x, y),
                }
            }
            Op::Not => {
                let x = self.raw(id, 0)?;
                let src = self.r(a[0]);
                match (src.lane, src.wide) {
                    (true, _) => format!("zb_not({})", x),
                    (false, false) => format!("!{}", x),
                    (false, true) => format!("{}.map(|b| !b)", x),
                }
            }
            Op::And => {
                // `Known(x) AND x` - "x is definitely true". Any other AND
                // is a validity conjunct, flattened rather than rendered.
                // Either ORDER: `fold` sorts a commutative op's operands
                // into node-id order, so which side the `Known` lands on
                // is an accident of when each was interned.
                let (k, v) = if matches!(self.g.get(a[1]).op, Op::Known) {
                    (a[1], a[0])
                } else {
                    (a[0], a[1])
                };
                match &self.g.get(k).op {
                    // `Known(x) AND x` - "x is definitely true".
                    Op::Known if self.g.get(k).args[0] == v => {
                        let inner = v;
                        let src = self.r(inner);
                        let x = self.expr[inner as usize].as_deref().unwrap();
                        match (src.lane, src.wide) {
                            (true, _) => {
                                format!("ZB {{ val: {x}.val & {x}.known, known: ALL }}", x = x)
                            }
                            (false, true) => format!("{}.unwrap_or(false)", x),
                            (false, false) => x.to_string(),
                        }
                    }
                    // An ordinary AND. The old front end never produced
                    // one that had to be RENDERED - every `And` it built
                    // was the idiom above or a validity conjunct the
                    // emitter flattened - but a traced graph builds them
                    // freely: a guard is `g AND c`, and `or` is De Morgan
                    // over two of them.
                    _ => self.render_bool2(id, out, "zb_and", "&&", false)?,
                }
            }
            Op::Or => self.render_bool2(id, out, "zb_or", "||", true)?,
            Op::Sel => {
                let arms = Repr {
                    dom: out.dom,
                    lane: out.lane,
                    wide: out.wide,
                };
                let (t, f) = (self.at(id, 1, arms)?, self.at(id, 2, arms)?);
                let c = self.r(a[0]);
                if c.lane {
                    // An undecided lane blends as if false; `Known(cond)`
                    // is a conjunct of `ok`, so such a lane is filtered
                    // out rather than trusted.
                    let cv = self.raw(id, 0)?;
                    let f_ = match (out.dom, out.lane, out.wide) {
                        (Dom::Num, true, true) => "zsel_i",
                        (Dom::Num, true, false) => "zsel_n",
                        (Dom::Bool, true, _) => "zsel_b",
                        _ => bail!("per-lane condition with block-uniform arms"),
                    };
                    format!("{}({}, {}, {})", f_, cv, t, f)
                } else if c.wide {
                    bail!("select on a block-uniform tri-state: the walk decides it first")
                } else {
                    format!("if {} {{ {} }} else {{ {} }}", self.raw(id, 0)?, t, f)
                }
            }
            Op::Known => {
                // "Is this value decided?" of a FLOOR reads the interval it
                // came from, not the (already narrowed) result: `Flr` is
                // exact by derivation precisely BECAUSE this conjunct
                // holds, so asking the result would answer `true` and lose
                // the guard.
                let inner = a[0];
                if let Op::Flr = &self.g.get(inner).op {
                    let src = self.g.get(inner).args[0];
                    let r = self.r(src);
                    if r.wide {
                        let x = self.expr[src as usize].as_deref().unwrap();
                        return Ok(if r.lane {
                            format!("zi_flr_ok({})", x)
                        } else {
                            format!("{x}.0.flr() == {x}.1.flr()", x = x)
                        });
                    }
                }
                let src = self.r(inner);
                let x = self.raw(id, 0)?;
                match (src.lane, src.dom, src.wide) {
                    (true, _, _) => format!("ZB {{ val: {}.known, known: ALL }}", x),
                    (false, Dom::Bool, true) => format!("{}.is_some()", x),
                    (false, Dom::Num, true) => format!("{x}.0 == {x}.1", x = x),
                    (false, _, false) => "true".to_string(),
                }
            }
            Op::SplitOk => format!("zi_span_ok({})", self.raw(id, 0)?),
            Op::Const(..) | Op::ConstBool(_) | Op::Cell(_) | Op::Free(_) => {
                bail!("node {} is an inline leaf and needs no let", id)
            }
            Op::Split(_) | Op::SplitValid(_) => {
                bail!("node {} is part of a split group, emitted as one unit", id)
            }
        })
    }
}

/// The inline reading of a leaf, or None if the node needs a `let`.
fn inline(g: &Graph, id: NodeId, r: Repr) -> Option<String> {
    match &g.get(id).op {
        Op::Const(lo, hi) => Some(if lo == hi {
            format!("P8::from_raw({}i32)", lo)
        } else {
            format!("(P8::from_raw({}i32), P8::from_raw({}i32))", lo, hi)
        }),
        Op::ConstBool(b) => Some(format!("{}", b)),
        Op::Free(b) => Some(format!("kb{}", b)),
        // A varying cell is loaded once into `r_cN` at the top of the
        // frame; a uniform one is a field of the bound `Uni`.
        Op::Cell(c) => Some(if r.lane {
            format!("r_c{}", c)
        } else {
            format!("u.c{}", c)
        }),
        _ => None,
    }
}

/// Split `ok` into the conditions it is the AND of. `Known(x)` is dropped
/// when `x` is itself a conjunct: guarding "x is definitely true" already
/// implies "x is decided", so keeping both would emit the same mask twice.
fn conjuncts(g: &Graph, ok: NodeId) -> Vec<NodeId> {
    fn walk(g: &Graph, n: NodeId, out: &mut Vec<NodeId>) {
        match &g.get(n).op {
            Op::And => {
                for a in &g.get(n).args {
                    walk(g, *a, out);
                }
            }
            // The identity of the chain.
            Op::ConstBool(true) => {}
            _ => out.push(n),
        }
    }
    let mut all = Vec::new();
    walk(g, ok, &mut all);
    let mut seen = BTreeSet::new();
    all.retain(|n| seen.insert(*n));
    let set: BTreeSet<NodeId> = all.iter().copied().collect();
    all.retain(|n| match &g.get(*n).op {
        Op::Known => !set.contains(&g.get(*n).args[0]),
        _ => true,
    });
    all
}

/// Nodes reachable from `roots`, walking operands.
fn reachable(g: &Graph, roots: &[NodeId]) -> Vec<bool> {
    let mut live = vec![false; g.len()];
    let mut stack: Vec<NodeId> = roots.to_vec();
    while let Some(n) = stack.pop() {
        if live[n as usize] {
            continue;
        }
        live[n as usize] = true;
        stack.extend(g.get(n).args.iter().copied());
    }
    live
}

/// What one validity conjunct contributes.
enum Term {
    /// A per-lane mask of the lanes on which the condition HOLDS.
    Lanes(String),
    /// A block-uniform condition, in the form that says the slice BAILS.
    Block(String),
}

/// Is this conjunct vacuously true? The walk asks `Known(x)` uniformly and
/// lets the graph answer; where `x` is exact by derivation the answer is
/// yes and there is nothing to emit.
///
/// The exception is the one that matters: `Flr` over an interval is exact
/// BECAUSE of this very conjunct, so asking the result would let the
/// premise answer itself. `render` reads the interval behind it instead.
fn vacuous(ctx: &Ctx, cond: NodeId) -> bool {
    let Op::Known = &ctx.g.get(cond).op else { return false };
    let inner = ctx.g.get(cond).args[0];
    if let Op::Flr = &ctx.g.get(inner).op {
        if ctx.r(ctx.g.get(inner).args[0]).wide {
            return false;
        }
    }
    let r = ctx.r(inner);
    !r.wide && !r.lane
}

/// How one validity conjunct enters the member's validity. The conjunct is
/// an ordinary emitted node by this point - `render` has already turned
/// `Known(Flr(x))` into `zi_flr_ok(x)` and `SplitOk(x)` into
/// `zi_span_ok(x)` - so all that is left is to read it at the right width.
fn conjunct_term(ctx: &Ctx, cond: NodeId) -> Result<Term> {
    let r = ctx.r(cond);
    if r.dom != Dom::Bool {
        bail!("validity conjunct {} is a {}, not a condition", cond, r.ty());
    }
    let x = ctx.expr[cond as usize]
        .as_deref()
        .ok_or_else(|| anyhow::anyhow!("validity conjunct {} was not emitted", cond))?;
    // A whole-slice impossibility: the kernel has no lowering for this at
    // all, so the block takes the interpreter path unconditionally.
    if let Op::ConstBool(false) = &ctx.g.get(cond).op {
        return Ok(Term::Block("true".to_string()));
    }
    Ok(match (r.lane, r.wide) {
        (true, _) => Term::Lanes(format!("zb_holds({})", x)),
        (false, false) => Term::Block(format!("!{}", x)),
        (false, true) => Term::Block(format!("{} != Some(true)", x)),
    })
}

/// One free-choice assignment's result: what it writes and whether it
/// counts. Assignments that agree on all three collapse to one of these,
/// so `mask` is a REPRESENTATIVE, not an enumeration.
pub(crate) struct Variant {
    pub(crate) mask: u8,
    /// One entry per OUTCOME - per output shape the frame can end in.
    ///
    /// A frame that kills an object ends in a different heap shape than
    /// one that does not, and both are real successors. Lowering them
    /// separately emits the whole frame up to the branch once per
    /// outcome; lowering them together emits it once, because they are
    /// nodes in the same graph and the emitter binds a node once.
    ///
    /// Length 1 is the checked-in kernels, and that path is unchanged.
    pub(crate) per: Vec<VarOut>,
}

/// What one assignment produces FOR ONE OUTCOME.
pub(crate) struct VarOut {
    /// cell -> the expression holding its value under this assignment.
    pub(crate) outputs: BTreeMap<u32, String>,
    pub(crate) ok: String,
    pub(crate) bd: String,
    /// The lanes that REACH this outcome, as a `u16` expression.
    ///
    /// A frame with several output shapes has to say which lanes take
    /// which successor, or the caller cannot route a single row. The
    /// walk path has one outcome and its `live` is `ConstBool(true)`,
    /// which has no conjuncts - so this is the literal `ALL` there, no
    /// line is emitted for it, and the checked-in kernels are unchanged.
    pub(crate) live: String,
}

/// One output shape: the cells it writes, and the two booleans that say
/// which lanes reach it (`live`) and which of those the kernel may keep
/// (`ok`).
pub(crate) struct Outcome {
    pub(crate) of: OutFields,
    pub(crate) ok: NodeId,
    pub(crate) live: NodeId,
}

/// Emit the body of one member: fills `e.body` and `e.variants` from the
/// graph, and rewrites the output fields to name graph nodes.
///
/// The free choices - the six buttons - are ELIMINATED here rather than
/// deferred to the compiler. The graph is rebuilt once per assignment into
/// one shared hash-consed arena, so a value two assignments compute the
/// same way IS one node, and the whole body is emitted once, straight
/// line. What used to be a shared prefix plus a suffix monomorphized 64
/// times is now just... the body.
///
/// Two things fall out that the prefix/suffix split could not express. A
/// node depending on k of the six bits is computed 2^k times rather than
/// 64 (and often fewer, because substituting a constant FOLDS: with
/// `right` held true, `Sel(right, 1, Sel(left, -1, 0))` is 1 whatever
/// `left` is, so those two assignments intern to the same node). And
/// assignments whose entire result agrees - outputs, validity and
/// liveness - collapse to one `Variant`, because they are the same
/// successor state and dedup would have merged their rows anyway.
pub(crate) fn emit_body(e: &mut Emit, outs: &mut [Outcome]) -> Result<()> {
    // --- specialize every free assignment into ONE arena ---
    let mut sp = Graph::new();
    let mut maps: Vec<Vec<NodeId>> =
        (0u8..64).map(|m| e.graph.specialize_into(m, &mut sp)).collect();

    // --- decide the boolean layer, if asked ---
    //
    // HERE, after specializing, and not on the traced graph: the guard
    // algebra only collapses once the buttons are constants, so the same
    // pass applied a step earlier finds a handful of nodes instead of a
    // fifth of them. Measured on a traced frame's outcome 2: 10,510
    // specialized nodes -> 4,714, from 183 substitutions cascading
    // through `fold`. The control - the identical rebuild with the BDD
    // switched off - removes exactly nothing, so none of that is the
    // rebuild.
    if e.decide {
        let mut roots: Vec<NodeId> = Vec::new();
        for m in 0..64usize {
            for o in outs.iter() {
                roots.extend(o.of.fields.iter().map(|f| maps[m][f.node as usize]));
                roots.push(maps[m][o.ok as usize]);
                roots.push(maps[m][o.live as usize]);
            }
        }
        // INTERVAL, then BOOLEAN, then INTERVAL. The two decide disjoint
        // things - `ival` knows `abs` is non-negative and the BDD does
        // not; the BDD knows `x and not x` is false and `ival` does not -
        // and each one's constants are the other's input, so the second
        // interval pass sees comparisons that only collapsed because the
        // BDD folded a select away.
        let (g1, m1, _) =
            crate::transpile::ival::fold(&sp, &roots, e.room.as_ref()).expect("interval fold");
        let r1: Vec<NodeId> = roots.iter().map(|r| m1[*r as usize]).collect();
        let (g2, m2, _) =
            crate::transpile::bdd::simplify_until_stable(&g1, &r1, 1 << 22, 4);
        let r2: Vec<NodeId> = r1.iter().map(|r| m2[*r as usize]).collect();
        let (sp2, m3, _) =
            crate::transpile::ival::fold(&g2, &r2, e.room.as_ref()).expect("interval fold 2");
        let nodemap = |x: NodeId| -> NodeId { m3[m2[m1[x as usize] as usize] as usize] };
        // Only the roots are remapped, because only the roots are read.
        // Anything else would be `UNREACHABLE` and would panic on use,
        // which is the point of that sentinel.
        for m in 0..64usize {
            let mut fresh = vec![crate::transpile::bdd::UNREACHABLE; e.graph.len()];
            for o in outs.iter() {
                for f in o.of.fields.iter() {
                    fresh[f.node as usize] = nodemap(maps[m][f.node as usize]);
                }
                fresh[o.ok as usize] = nodemap(maps[m][o.ok as usize]);
                fresh[o.live as usize] = nodemap(maps[m][o.live as usize]);
            }
            maps[m] = fresh;
        }
        sp = sp2;
    }
    let maps = maps;
    let n = sp.len();

    // Two assignments are the same successor iff they agree on every
    // output, on which lanes are valid, and on which lanes are live. The
    // last two are not optional: `dispatch.rs` ignores the variant mask
    // but accumulates `deopt_rows` from `kout.deopt & kout.valid` and
    // aborts the chunk on `kout.bd`, so two assignments writing the same
    // cells while deopting different lanes are NOT interchangeable.
    //
    // With several outcomes the signature spans ALL of them: two
    // assignments are interchangeable only if they agree about every
    // successor, not just about one.
    let signature = |m: u8, maps: &Vec<Vec<NodeId>>, outs: &[Outcome]| -> Vec<NodeId> {
        let mut sig: Vec<NodeId> = Vec::new();
        for o in outs {
            sig.extend(o.of.fields.iter().map(|f| maps[m as usize][f.node as usize]));
            sig.push(maps[m as usize][o.ok as usize]);
            sig.push(maps[m as usize][o.live as usize]);
        }
        sig
    };
    let mut sigs: BTreeMap<Vec<NodeId>, u8> = BTreeMap::new();
    for m in 0u8..64 {
        sigs.entry(signature(m, &maps, outs)).or_insert(m);
    }
    let reps: Vec<u8> = {
        let mut r: Vec<u8> = sigs.values().copied().collect();
        r.sort_unstable();
        r
    };

    // --- representations, for every node in the specialized arena ---
    let mut ctx = Ctx {
        g: &sp,
        uni: &e.uni,
        vary: &e.vary_in,
        repr: vec![Repr::num(false, false); n],
        name: vec![None; n],
        expr: vec![None; n],
    };
    for id in 0..n as NodeId {
        ctx.repr[id as usize] = ctx.derive(id)?;
    }

    // --- what has to exist ---
    // Per representative: its outputs, and every conjunct of its validity.
    let mut conj_of: BTreeMap<(u8, usize), Vec<NodeId>> = BTreeMap::new();
    let mut live_of: BTreeMap<(u8, usize), Vec<NodeId>> = BTreeMap::new();
    let mut roots: Vec<NodeId> = Vec::new();
    for r in &reps {
        let map = &maps[*r as usize];
        for (oi, o) in outs.iter().enumerate() {
            roots.extend(o.of.fields.iter().map(|f| map[f.node as usize]));
            let cs: Vec<NodeId> = conjuncts(&sp, map[o.ok as usize])
                .into_iter()
                .filter(|c| !vacuous(&ctx, *c))
                .collect();
            roots.extend(cs.iter().copied());
            conj_of.insert((*r, oi), cs);
            let ls: Vec<NodeId> = conjuncts(&sp, map[o.live as usize])
                .into_iter()
                .filter(|c| !vacuous(&ctx, *c))
                .collect();
            roots.extend(ls.iter().copied());
            live_of.insert((*r, oi), ls);
        }
    }
    for id in 0..n as NodeId {
        if matches!(sp.get(id).op, Op::Split(_)) {
            roots.push(id);
        }
    }
    let live = reachable(&sp, &roots);

    // --- names and inline forms ---
    for id in 0..n as NodeId {
        if !live[id as usize] {
            continue;
        }
        let r = ctx.repr[id as usize];
        match inline(ctx.g, id, r) {
            Some(text) => ctx.expr[id as usize] = Some(text),
            None => {
                let name = match &ctx.g.get(id).op {
                    Op::Split(d) => format!("f{}", d),
                    Op::SplitValid(d) => format!("f{}_v", d),
                    _ => format!("n{}", id),
                };
                ctx.expr[id as usize] = Some(name.clone());
                ctx.name[id as usize] = Some(name);
            }
        }
    }

    // --- placement: only the SPLITS scope anything now ---
    let scone = sp.split_cones();
    let level = |id: NodeId| -> usize {
        let m = scone[id as usize];
        if m == 0 { 0 } else { 8 - m.leading_zeros() as usize }
    };

    let depth = e.fork_depth;
    let mut body: Vec<Line> = Vec::new();
    let mut var_ty: std::collections::HashMap<String, &'static str> =
        std::collections::HashMap::new();

    for (id, kind) in &e.vary_in {
        let (ty, load) = match *kind {
            "num" => ("ZN", format!("let r_c{}: ZN = rin.c{};", id, id)),
            "ival" => ("ZI", format!("let r_c{}: ZI = rin.c{};", id, id)),
            "bool" => (
                "ZB",
                format!("let r_c{}: ZB = ZB {{ val: rin.c{}, known: ALL }};", id, id),
            ),
            other => bail!("varying cell {} has kind {:?}", id, other),
        };
        body.push(Line::Raw(load));
        var_ty.insert(format!("r_c{}", id), ty);
    }

    let mut valid_expr = "ALL".to_string();
    for lvl in 0..=depth {
        for id in 0..n as NodeId {
            if !live[id as usize] || level(id) != lvl {
                continue;
            }
            if matches!(ctx.g.get(id).op, Op::Split(_) | Op::SplitValid(_)) {
                continue; // emitted as part of the split group
            }
            emit_node(&ctx, &mut body, &mut var_ty, id)?;
        }
        if lvl == depth {
            break;
        }
        let d = lvl;
        let split = (0..n as NodeId)
            .find(|id| live[*id as usize] && matches!(ctx.g.get(*id).op, Op::Split(x) if x as usize == d))
            .ok_or_else(|| anyhow::anyhow!("split {} has no node", d))?;
        let fname = ctx.name[split as usize].clone().unwrap();
        let src = ctx.expr[ctx.g.get(split).args[0] as usize].clone().unwrap();
        // OPAQUE trip count, for the TRACED path only.
        //
        // `for c0 in 0..2` invites LLVM to unroll, and here the loop
        // body is the rest of the frame - so two forks become four
        // copies of a 10,000-line function. Measured 2026-08-23 on the
        // traced room kernels: 55 s to compile with the bound hidden,
        // over 25 MINUTES with it visible. Same code either way; the
        // loop runs twice regardless.
        //
        // NOT applied to the walk's kernels, and the reason is a rule
        // rather than caution: those are what the production search
        // runs and what every number in BENCHMARK_DATA was measured on.
        // `black_box` blocks optimisation across it, so turning it on
        // there would reprice recorded results to make MY build loop
        // shorter. Their fork also sits near the end of the body, so
        // there is little left to duplicate and they were never slow.
        let bound = if e.opaque_forks {
            "std::hint::black_box(2usize)"
        } else {
            "2usize"
        };
        body.push(Line::Raw(format!("for c{} in 0..{} {{", d, bound)));
        body.push(Line::Raw(format!(
            "let ({f}, {f}_fv): (ZI, u16) = zi_fork_flr({s}, c{d});",
            f = fname, s = src, d = d
        )));
        body.push(Line::Raw(format!(
            "let valid{}: u16 = {} & {}_fv;",
            d, valid_expr, fname
        )));
        body.push(Line::Raw(format!("if valid{} == 0 {{ continue; }}", d)));
        // Fork validity in BOTH forms, but the second one only when
        // something wants it.
        //
        // `zi_fork_flr` returns a raw mask, which is what `valid{d}`
        // wants. Validity is also an ordinary boolean VALUE: a traced
        // frame conjoins it into the state's guard, and a guard is what
        // merges select on, so it has to be a `ZB` wherever the boolean
        // algebra consumes it - emitting only the mask produced
        // `zb_and(n124, f0_fv)`, a u16 where a ZB was wanted.
        //
        // The WALK never consumes one as a value; its fork validity only
        // ever reaches the liveness machinery, which takes the mask. So
        // this is emitted on demand, and the checked-in kernels stay
        // byte-identical rather than gaining a dead binding each.
        let valid_node = (0..n as NodeId).find(
            |id| live[*id as usize] && matches!(ctx.g.get(*id).op, Op::SplitValid(x) if x as usize == d),
        );
        let wanted = valid_node.is_some_and(|v| {
            (0..n as NodeId)
                .any(|id| live[id as usize] && ctx.g.get(id).args.contains(&v))
        });
        if wanted {
            body.push(Line::Raw(format!(
                "let {f}_v: ZB = ZB {{ val: {f}_fv, known: ALL }};",
                f = fname
            )));
            var_ty.insert(format!("{}_v", fname), "ZB");
        }
        var_ty.insert(fname.clone(), "ZI");
        var_ty.insert(format!("valid{}", d), "u16");
        valid_expr = format!("valid{}", d);
    }

    // --- per-representative validity, at the innermost scope ---
    // Every conjunct is an ordinary node emitted above; all that is left
    // is to read each at the right width and AND/OR them together. No
    // per-level accumulation, because validity is a value and nothing
    // reads it until the end.
    let mut variants: Vec<Variant> = Vec::new();
    for r in &reps {
        let mut per: Vec<VarOut> = Vec::new();
        for (oi, o) in outs.iter().enumerate() {
            let (mut lanes, mut blocks) = (Vec::new(), Vec::new());
            for c in &conj_of[&(*r, oi)] {
                match conjunct_term(&ctx, *c)? {
                    Term::Lanes(m) => lanes.push(m),
                    Term::Block(b) => blocks.push(b),
                }
            }
            // One outcome keeps the original names, so a single-outcome
            // body is byte-identical to what this emitted before.
            let sfx = if outs.len() == 1 { String::new() } else { format!("_o{}", oi) };
            let ok = format!("ok_v{}{}", r, sfx);
            let bd = format!("bd_v{}{}", r, sfx);
            body.push(Line::Let {
                name: ok.clone(),
                ty: "u16",
                expr: if lanes.is_empty() {
                    "ALL".into()
                } else {
                    format!("ALL & {}", lanes.join(" & "))
                },
            });
            body.push(Line::Let {
                name: bd.clone(),
                ty: "bool",
                expr: if blocks.is_empty() { "false".into() } else { blocks.join(" || ") },
            });
            var_ty.insert(ok.clone(), "u16");
            var_ty.insert(bd.clone(), "bool");
            // Which lanes REACH this outcome. A block-level conjunct
            // says the whole block does not, which is a `u16` of zero
            // rather than a deopt - a sibling outcome claims those lanes.
            //
            // ONLY WHEN THERE IS MORE THAN ONE. With a single outcome,
            // "which lanes take this successor" is "which lanes exist in
            // this fork configuration", which the walk already binds as
            // `valid{d}` and `render` already emits as `valid` - so
            // emitting it again would be 36 duplicate lines in every
            // checked-in kernel.
            //
            // That equivalence is CHECKED, not assumed: at one outcome
            // every live conjunct has to be a `SplitValid`, which is the
            // only thing `Emit::require_live` ever contributes. A traced
            // single-outcome frame satisfies it the other way, with no
            // conjuncts at all, because the frontier's guards partition
            // the lanes and a lone outcome therefore claims all of them.
            // Anything else would be a real condition about to be
            // dropped, so it stops here instead.
            let live = {
                let (mut lanes, mut blocks) = (Vec::new(), Vec::new());
                for c in &live_of[&(*r, oi)] {
                    if outs.len() == 1 && !matches!(sp.get(*c).op, Op::SplitValid(_)) {
                        bail!(
                            "the only outcome's liveness has a conjunct ({:?}) that is not \
                             fork validity, and a single-outcome kernel has nowhere to put it",
                            sp.get(*c).op
                        );
                    }
                    match conjunct_term(&ctx, *c)? {
                        Term::Lanes(m) => lanes.push(m),
                        Term::Block(b) => blocks.push(b),
                    }
                }
                if outs.len() == 1 || (lanes.is_empty() && blocks.is_empty()) {
                    "ALL".to_string()
                } else {
                    let name = format!("live_v{}{}", r, sfx);
                    let mask = if lanes.is_empty() {
                        "ALL".to_string()
                    } else {
                        format!("ALL & {}", lanes.join(" & "))
                    };
                    let expr = if blocks.is_empty() {
                        mask
                    } else {
                        format!("if {} {{ 0 }} else {{ {} }}", blocks.join(" || "), mask)
                    };
                    body.push(Line::Let { name: name.clone(), ty: "u16", expr });
                    var_ty.insert(name.clone(), "u16");
                    name
                }
            };
            let map = &maps[*r as usize];
            let mut outputs = BTreeMap::new();
            for f in o.of.fields.iter() {
                let want = repr_of_ty(f.ty)?;
                let node = map[f.node as usize];
                let have = ctx.repr[node as usize];
                if !want.admits(have) {
                    bail!(
                        "output cell {} wants a {} but the graph computes a {}",
                        f.cell, want.ty(), have.ty()
                    );
                }
                outputs.insert(
                    f.cell,
                    coerce(ctx.expr[node as usize].as_deref().unwrap(), have, want)?,
                );
            }
            per.push(VarOut { outputs, ok, bd, live });
        }
        variants.push(Variant { mask: *r, per });
    }

    // A cell is per-variant only if the variants DISAGREE about it. That
    // is exact, where the old button-cone test was an over-approximation:
    // a value can depend on a button and still be the same in every
    // assignment, and such a cell belongs in the shared output.
    for (oi, o) in outs.iter_mut().enumerate() {
        for f in o.of.fields.iter_mut() {
            let first = variants[0].per[oi].outputs[&f.cell].clone();
            f.tainted = variants.iter().any(|v| v.per[oi].outputs[&f.cell] != first);
            // A COMPILE-TIME CONSTANT that every variant agrees on holds
            // the same value in every row of this outcome's accumulator,
            // so the column can be written once as `Col::U` rather than
            // pushed per row.
            //
            // Read off the specialized graph rather than by matching the
            // emitted text: a literal is `Op::Const` / `Op::ConstBool`
            // there, and the rendered form varies with the coercion the
            // field's type asked for (`zn_splat(P8::from_raw(..))` and
            // friends).
            f.konst = if f.tainted {
                None
            } else {
                let node = maps[reps[0] as usize][f.node as usize];
                match sp.get(node).op {
                    Op::Const(lo, hi) if lo == hi => {
                        Some(format!("AV::Num(P8::from_raw({}i32))", lo))
                    }
                    Op::Const(lo, hi) => Some(format!(
                        "AV::Ival(P8::from_raw({}i32), P8::from_raw({}i32))",
                        lo, hi
                    )),
                    Op::ConstBool(b) => Some(format!("AV::Bool({})", b)),
                    _ => None,
                }
            };
            f.expr = first;
        }
    }

    // Which representative each of the 64 assignments collapsed onto. The
    // FUSED artifact needs it: two assignments are interchangeable there
    // only if EVERY member says so, and a member's own collapse is
    // coarser than the fused one.
    let mut rep_of = [0u8; 64];
    for m in 0u8..64 {
        rep_of[m as usize] = sigs[&signature(m, &maps, outs)];
    }

    e.body = body;
    e.var_ty = var_ty;
    e.valid_expr = valid_expr;
    e.variants = variants;
    e.rep_of = rep_of;
    Ok(())
}

/// Let-bind one node.
fn emit_node(
    ctx: &Ctx,
    buf: &mut Vec<Line>,
    var_ty: &mut std::collections::HashMap<String, &'static str>,
    id: NodeId,
) -> Result<()> {
    let Some(name) = ctx.name[id as usize].clone() else {
        return Ok(()); // an inline leaf
    };
    let ty = ctx.r(id).ty();
    buf.push(Line::Let { name: name.clone(), ty, expr: ctx.render(id)? });
    var_ty.insert(name, ty);
    Ok(())
}

/// The representation a generated Rust type stands for - the inverse of
/// `Repr::ty`, used to check the boundary's expectation against the graph's
/// derivation.
fn repr_of_ty(ty: &str) -> Result<Repr> {
    Ok(match ty {
        "P8" => Repr::num(false, false),
        "(P8, P8)" => Repr::num(false, true),
        "ZN" => Repr::num(true, false),
        "ZI" => Repr::num(true, true),
        "bool" => Repr::boolean(false, false),
        "Option<bool>" => Repr::boolean(false, true),
        "ZB" => Repr::boolean(true, false),
        other => bail!("no representation for the generated type {:?}", other),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_lane_primitives_agree_with_the_folder_about_and_and_or() {
        // `zb_and` and `zb_or` are what an `And`/`Or` node LOWERS to, and
        // `Graph::eval` is what the same node means to every analysis
        // upstream. If those two disagree, a folded graph and the kernel
        // built from it answer differently, and nothing else in the
        // pipeline would notice - the graph is never executed and the
        // kernel is never folded.
        //
        // This became load-bearing rather than theoretical when `fold`
        // started rewriting `Sel(c, x, true)` into `Or(Not c, x)`: the
        // walk-driven path had never emitted either primitive before, and
        // the checked-in kernels now contain 18 `zb_and` and 5 `zb_or`.
        use celeste_engine::kernel::{zb_and, zb_or, ALL, ZB};
        use std::collections::HashMap;

        let tri = [Some(true), Some(false), None];
        let as_zb = |b: Option<bool>| match b {
            Some(true) => ZB { val: ALL, known: ALL },
            Some(false) => ZB { val: 0, known: ALL },
            None => ZB { val: 0, known: 0 },
        };
        // Lane 0 only; the primitives are bitwise, so one lane decides.
        let from_zb = |z: ZB| -> Option<bool> {
            if z.known & 1 == 0 {
                None
            } else {
                Some(z.val & 1 != 0)
            }
        };

        let mut g = Graph::new();
        let (x, y) = (g.leaf(Op::Cell(0)), g.leaf(Op::Cell(1)));
        // `add`, not `fold`: folding would answer some of these from the
        // constants and the point is to exercise the OP.
        let and = g.add(Op::And, vec![x, y]);
        let or = g.add(Op::Or, vec![x, y]);
        for a in tri {
            for b in tri {
                let cells = HashMap::from([
                    (0u32, super::super::graph::Val::Bool(a)),
                    (1u32, super::super::graph::Val::Bool(b)),
                ]);
                let out = g.eval(&cells).unwrap();
                let want = |n: NodeId| match out[n as usize] {
                    super::super::graph::Val::Bool(v) => v,
                    other => panic!("not a boolean: {:?}", other),
                };
                assert_eq!(
                    from_zb(zb_and(as_zb(a), as_zb(b))),
                    want(and),
                    "zb_and disagrees with the folder at {:?} AND {:?}",
                    a,
                    b
                );
                assert_eq!(
                    from_zb(zb_or(as_zb(a), as_zb(b))),
                    want(or),
                    "zb_or disagrees with the folder at {:?} OR {:?}",
                    a,
                    b
                );
            }
        }
    }

    fn all_reprs() -> Vec<Repr> {
        let mut v = Vec::new();
        for dom in [Dom::Num, Dom::Bool] {
            for lane in [false, true] {
                for wide in [false, true] {
                    v.push(Repr { dom, lane, wide });
                }
            }
        }
        v
    }

    #[test]
    fn the_type_table_and_its_inverse_agree() {
        // `Repr::ty` feeds the generated code; `repr_of_ty` reads the
        // BOUNDARY's expectation back. If they drift, an output cell can
        // be written into a column of the wrong kind - which changes the
        // row key, and a different row key is a different search.
        for r in all_reprs() {
            let back = repr_of_ty(r.ty()).unwrap();
            // ZB is the one lossy entry: a per-lane boolean carries its own
            // `known` mask, so both widths share a type.
            if r.dom == Dom::Bool && r.lane {
                assert_eq!(back.ty(), "ZB");
            } else {
                assert_eq!(back, r, "{} did not round-trip", r.ty());
            }
        }
    }

    #[test]
    fn coercion_only_ever_widens() {
        for from in all_reprs() {
            for to in all_reprs() {
                let out = coerce("x", from, to);
                if from.ty() == to.ty() {
                    assert_eq!(out.unwrap(), "x");
                } else if to.admits(from) {
                    // Every admitted pair must have a lowering - a hole
                    // here would be a silent representation mismatch.
                    assert!(out.is_ok(), "no coercion {} -> {}", from.ty(), to.ty());
                } else {
                    assert!(out.is_err(), "{} -> {} is a NARROWING", from.ty(), to.ty());
                }
            }
        }
    }

    #[test]
    fn validity_flattens_and_drops_the_redundant_knownness() {
        // zguard's condition is `Known(c) AND c`. Guarding "c is definitely
        // true" already implies "c is decided", so the flattened conjunct
        // list must not carry both - emitting the same mask twice is the
        // kind of waste the old hand-placed guards were full of.
        let mut g = Graph::new();
        let c = g.leaf(Op::Cell(1));
        let k = g.add(Op::Known, vec![c]);
        let both = g.add(Op::And, vec![k, c]);
        let all = g.leaf(Op::ConstBool(true));
        let ok = g.add(Op::And, vec![all, both]);
        assert_eq!(conjuncts(&g, ok), vec![c]);

        // But a Known whose value is NOT itself a conjunct survives: it is
        // the only thing standing between an undecided lane and a wrong
        // answer.
        let d = g.leaf(Op::Cell(2));
        let kd = g.add(Op::Known, vec![d]);
        let ok2 = g.add(Op::And, vec![ok, kd]);
        assert_eq!(conjuncts(&g, ok2), vec![c, kd]);
    }

    #[test]
    fn an_impossible_lane_collapses_the_whole_chain() {
        // `require_never` folds `ok` to false. Every other conjunct then
        // disappears, and that is correct rather than lossy: a false `ok`
        // is `*bd = true`, the whole slice goes to the interpreter, and
        // per-lane deopt bits for a slice nobody executes are noise.
        let mut g = Graph::new();
        let c = g.leaf(Op::Cell(1));
        let all = g.leaf(Op::ConstBool(true));
        let ok = g.fold(Op::And, vec![all, c]);
        let never = g.leaf(Op::ConstBool(false));
        let ok = g.fold(Op::And, vec![ok, never]);
        assert_eq!(conjuncts(&g, ok), vec![never]);
    }
}
