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
    /// Machine words - the row-key fold. Not an abstract game value: see
    /// `Op::Word` / `Op::Bits` / `Op::Mix`. `wide` is meaningless here
    /// and is always false.
    Word,
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
    fn word(lane: bool) -> Self {
        Repr { dom: Dom::Word, lane, wide: false }
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
            (Dom::Word, false, _) => "u64",
            (Dom::Word, true, _) => "ZW",
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
        (Dom::Word, false, _, true, _) => format!("zw_splat({})", expr),
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
            Op::Split(_) | Op::Frag(_) => Repr::num(true, true),
            Op::SplitValid(_) | Op::FragOk(_) => Repr::boolean(true, false),
            Op::SplitOk => Repr::boolean(true, false),
            Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Neg | Op::Abs | Op::Min
            | Op::Max => joined(Dom::Num),
            // Always an interval, whatever the bounds are: a `Span` of
            // two exact values is still a set of numbers, and the whole
            // reason it exists is that the two are not the same node.
            // (`fold` turns a span of two LITERALS back into a `Const`,
            // so the degenerate case never reaches here.)
            Op::Span => Repr { dom: Dom::Num, lane: joined(Dom::Num).lane, wide: true },
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
            // The row-key layer. `Bits` is per-lane exactly when the
            // value it reads is: a block-uniform cell contributes the
            // same word to every row, which costs one scalar mix instead
            // of sixteen.
            Op::Word(_) => Repr::word(false),
            Op::Bits => Repr::word(self.r(a[0]).lane),
            Op::Mix(_, _) => Repr::word(a.iter().any(|x| self.r(*x).lane)),
            // The sound full-key layer: `CellMix` reads one value's per-lane
            // contribution (always a per-lane WORD - it is only ever built
            // over the non-const output cells), `AddW` sums them.
            Op::CellMix(_, _) => Repr::word(true),
            Op::AddW => Repr::word(a.iter().any(|x| self.r(*x).lane)),
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
            // Both operands coerced to the OUTPUT (interval) repr, then
            // one bound taken from each. Going through `at` rather than
            // reading the operands raw is what makes an exact operand
            // work: the existing widening coercions (`zi_of_zn`,
            // `zi_splat`, `(e, e)`) already turn a number into the
            // singleton interval, and taking a bound off that is the
            // number back again.
            Op::Span => {
                let (x, y) = (self.at(id, 0, out)?, self.at(id, 1, out)?);
                if lane {
                    format!("ZI {{ lo: {}.lo, hi: {}.hi }}", x, y)
                } else {
                    format!("({}.0, {}.1)", x, y)
                }
            }
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
                    (Dom::Word, true) => bail!("equality on row-key words"),
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
                    (false, Dom::Word, true) => bail!("Known of a row-key word"),
                    (false, _, false) => "true".to_string(),
                }
            }
            Op::SplitOk => format!("zi_span_ok({})", self.raw(id, 0)?),
            // The resolved fork. `zi_fork_flr` is the same primitive
            // the loop nest calls; the only difference is that `c` is a
            // literal here rather than a loop variable, which is what
            // lets the two fragments intern, fold, and share.
            //
            // This arm used to `bail!` on the grounds that fork
            // specialization was refuted. That refutation was measured
            // on the only room that existed - which forks twice - with
            // all 64 button assignments blended into one arena, and it
            // was wrong twice over. See `plans/tracing.md`.
            Op::Frag(c) => format!("zi_fork_flr({}, {}).0", self.raw(id, 0)?, c),
            // A per-lane boolean like any other guard atom - `zi_fork_flr`
            // reports validity as a bare mask, and it is always known.
            Op::FragOk(c) => format!(
                "ZB {{ val: zi_fork_flr({}, {}).1, known: ALL }}",
                self.raw(id, 0)?,
                c
            ),
            // ---- row key ----
            Op::Bits => {
                let src = self.r(a[0]);
                let x = self.raw(id, 0)?;
                match (src.lane, src.dom, src.wide) {
                    (true, Dom::Num, false) => format!("zw_bits_n({})", x),
                    (true, Dom::Num, true) => format!("zw_bits_i({})", x),
                    (true, Dom::Bool, _) => format!("zw_bits_b({})", x),
                    (false, Dom::Num, false) => format!("{}.as_raw_u32() as u64", x),
                    (false, Dom::Bool, false) => format!("{} as u64", x),
                    // A block-uniform INTERVAL output: the fruit rooms'
                    // rung-agnostic kernels compute e.g. the fruit's `y`
                    // from the boundary-widened (uniform) `off`, and the
                    // result feeds the row key. Pack both endpoints the
                    // way `zw_bits_i` does per lane - (lo << 32) | hi is
                    // injective on the u32 raw pair, so the dedup key
                    // distinguishes exactly by interval value.
                    (false, Dom::Num, true) => format!(
                        "({x}.0.as_raw_u32() as u64) << 32 | {x}.1.as_raw_u32() as u64",
                        x = x
                    ),
                    // A block-uniform TRI-STATE bool output. No outcome
                    // has one today; the hole is left open on purpose so
                    // the first one that does fails here with a name
                    // rather than silently hashing something else.
                    (false, Dom::Bool, true) => {
                        bail!("no row-key bits for a block-uniform wide Bool")
                    }
                    (_, Dom::Word, _) => bail!("Bits of a machine word"),
                }
            }
            Op::Mix(c, half) => {
                let w = Repr::word(lane);
                let (h, v) = (self.at(id, 0, w)?, self.at(id, 1, w)?);
                match (lane, half) {
                    (true, 0) => format!("zw_mix1({}, {}, {}u64)", h, v, c),
                    (true, _) => format!("zw_mix2({}, {}, {}u64)", h, v, c),
                    (false, 0) => format!("mix64({} ^ mix64({} ^ {}u64))", h, v, c),
                    (false, _) => format!(
                        "{}.wrapping_add(mix64({}.wrapping_mul(({}u64 << 1) | 1)))",
                        h, v, c
                    ),
                }
            }
            // ---- sound full row key ----
            // One cell's ADDITIVE contribution `cell_mix(cell, value,
            // seed[half])`, vectorized. The value is coerced to its per-lane
            // form first (a uniform per-lane-sum cell is splatted), so the
            // type dispatch is exactly `Op::Bits`'s.
            Op::CellMix(c, half) => {
                let src = self.r(a[0]);
                let seed: u64 = if half == 0 { 0x5bf0_3635 } else { 0x27d4_eb2f };
                let (want, f) = match (src.dom, src.wide) {
                    (Dom::Num, false) => (Repr::num(true, false), "zw_cellmix_n"),
                    (Dom::Num, true) => (Repr::num(true, true), "zw_cellmix_i"),
                    (Dom::Bool, _) => (Repr::boolean(true, false), "zw_cellmix_b"),
                    (Dom::Word, _) => bail!("CellMix of a machine word"),
                };
                let v = self.at(id, 0, want)?;
                format!("{}({}u64, {}, {}u64)", f, c, v, seed)
            }
            Op::AddW => {
                let w = Repr::word(lane);
                let (x, y) = (self.at(id, 0, w)?, self.at(id, 1, w)?);
                if lane {
                    format!("zw_add({}, {})", x, y)
                } else {
                    format!("{}.wrapping_add({})", x, y)
                }
            }
            Op::Word(_) => bail!("node {} is an inline leaf and needs no let", id),
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
        // The fold's seed. Always block-uniform, so it never needs the
        // splat - the first `Mix` against a per-lane cell produces the
        // per-lane accumulator.
        Op::Word(w) => Some(format!("{}u64", w)),
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
/// so a variant is a REPRESENTATIVE, not an enumeration.
pub(crate) struct Variant {
    /// One entry per OUTCOME - per output shape the frame can end in.
    ///
    /// A frame that kills an object ends in a different heap shape than
    /// one that does not, and both are real successors. Lowering them
    /// separately emits the whole frame up to the branch once per
    /// outcome; lowering them together emits it once, because they are
    /// nodes in the same graph and the emitter binds a node once.
    ///
    /// `None` means this variant does not write that outcome at all: a
    /// variant is a (button, fork configuration) pair, and a fork
    /// configuration is only meaningful for the outcomes whose cone
    /// contains those forks. Outcome 13 varying over forks 12 and 13 has
    /// nothing to say about a variant that resolved forks 0 and 1.
    pub(crate) per: Vec<Option<VarOut>>,
}

/// What one assignment produces FOR ONE OUTCOME. It used to also carry
/// the variant's `ok`/`bd`/`live`/row-key variable NAMES for the Rust
/// renderer; those went with it (2026-08-29) - the corresponding `Line`s
/// are still emitted into the body, this just stopped recording their
/// names.
pub(crate) struct VarOut {
    /// cell -> the expression holding its value under this assignment.
    pub(crate) outputs: BTreeMap<u32, String>,
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
/// The FLAT fork path: every fork resolved at COMPILE time, so the
/// emitted body is straight-line and every node is bound once.
///
/// ## Why this exists
///
/// The loop path it replaced (a runtime `for cN in 0..2` nest, deleted
/// with the walk kernels 2026-08-25) placed a node at `1 + its highest
/// fork bit` and wrapped the whole frame - every outcome - in one nest of
/// that depth. The
/// dependency structure is not a chain, though: measured on room (2,0)
/// shape 1 (14 forks, 19 outcomes), each outcome depends on a SMALL
/// subset of the forks, usually one `(x, y)` pair from one `obj.move`,
/// and the pairs belong to mutually exclusive paths. So the nest walks
/// 2^14 configurations and performs 311,296 outcome-pushes to produce
/// **65** distinct bodies.
///
/// Three things collapse it, and all three are needed:
///
/// * **Per outcome.** A fork configuration is only meaningful for the
///   outcomes whose cone contains those forks, so each outcome is
///   enumerated over its OWN forks. Nothing here ever builds the
///   product across outcomes - that product is what made 16,384
///   configurations look distinct.
/// * **Resolved, not looped.** `Frag`/`FragOk` are ordinary unary ops,
///   so they intern and fold, and the configurations share every node
///   they agree on.
/// * **`decide`, afterwards.** Guard algebra only collapses once the
///   choices are constants. Skipping it reports 256 bodies where there
///   are 16, because structural node identity is not semantic identity.
/// Specialize a traced frame's graph into ONE fused, hash-consed arena.
///
/// Every (button, fork configuration) is resolved: `Free` becomes a
/// constant and `Split`/`SplitValid` become `Frag`/`FragOk` (ordinary
/// unary ops), so NO fork node survives and configurations that agree
/// share nodes ("duplicate, then fuse again"). Steps 1-4 of the frame
/// lowering. Returns the fused graph and one body per DISTINCT (outcome,
/// roots): `(outcome, frees, splits, roots)`, where `roots` is that
/// outcome's fields in order, then `ok`, then `live`, as node ids in the
/// fused graph.
///
/// This is the SINGLE source of the specialized compute: the kernel
/// emitter renders Rust from it (`emit_body`) and the AVX-512 backend
/// assembles it (`trace::emit::asm_fused`), so both compute byte-for-byte
/// the same thing - there is no parallel specialization.
pub(crate) fn specialize_frame(
    graph: &Graph,
    outs: &[(Vec<NodeId>, NodeId, NodeId)],
    forks: u8,
    decide: bool,
    room: Option<&crate::transpile::graph::Room>,
) -> (Graph, Vec<(usize, u8, u64, Vec<NodeId>)>) {
    // --- 1. which forks each outcome actually depends on ---
    let cones = graph.split_cones();
    let bits_of = |fields: &[NodeId], ok: NodeId, live: NodeId| -> Vec<u8> {
        let mut m = cones[ok as usize] | cones[live as usize];
        for &f in fields {
            m |= cones[f as usize];
        }
        (0..forks).filter(|d| m & (1u64 << d) != 0).collect()
    };

    // --- 2. specialize, per outcome, over (button, its own forks) ---
    let mut sp = Graph::new();
    // (outcome, button, fork configuration, roots) where roots is the
    // outcome's fields in order, then `ok`, then `live`.
    let mut cands: Vec<(usize, u8, u64, Vec<NodeId>)> = Vec::new();
    for (oi, (fields, ok, live)) in outs.iter().enumerate() {
        let mut want: Vec<NodeId> = fields.clone();
        want.push(*ok);
        want.push(*live);
        // An outcome reaches a fraction of the graph, and mapping the
        // whole arena once per (button, configuration) is most of the
        // work and none of the answer.
        let need = reachable(graph, &want);
        let bits = bits_of(fields, *ok, *live);
        // Buttons first, with the forks left standing. If two
        // assignments agree before the forks are resolved they agree
        // after - resolving is substitution, and substitution preserves
        // equality - so this prunes the product soundly, and cheaply
        // enough to be worth a separate pass.
        let reps: Vec<u8> = {
            let mut probe = Graph::new();
            let mut seen: BTreeMap<Vec<NodeId>, u8> = BTreeMap::new();
            for m in 0u8..64 {
                let map = graph.specialize_subset_into(m, None, Some(&need), &mut probe);
                let sig: Vec<NodeId> = want.iter().map(|r| map[*r as usize]).collect();
                seen.entry(sig).or_insert(m);
            }
            let mut v: Vec<u8> = seen.into_values().collect();
            v.sort_unstable();
            v
        };
        for m in reps {
            for k in 0..(1u64 << bits.len()) {
                let mut sm = 0u64;
                for (i, d) in bits.iter().enumerate() {
                    if k & (1 << i) != 0 {
                        sm |= 1u64 << d;
                    }
                }
                let map = graph.specialize_subset_into(m, Some(sm), Some(&need), &mut sp);
                let roots: Vec<NodeId> = want.iter().map(|r| map[*r as usize]).collect();
                cands.push((oi, m, sm, roots));
            }
        }
    }

    // --- 3. decide the boolean layer, on the RESOLVED graph ---
    if decide {
        let all: Vec<NodeId> = cands.iter().flat_map(|c| c.3.iter().copied()).collect();
        let (g1, m1, _) =
            crate::transpile::ival::fold(&sp, &all, room).expect("interval fold");
        let r1: Vec<NodeId> = all.iter().map(|x| m1[*x as usize]).collect();
        let (g2, m2, _) = crate::transpile::bdd::simplify_until_stable(&g1, &r1, 1 << 22, 4);
        let r2: Vec<NodeId> = r1.iter().map(|x| m2[*x as usize]).collect();
        let (g3, m3, _) =
            crate::transpile::ival::fold(&g2, &r2, room).expect("interval fold 2");
        let mut it = r2.iter().map(|x| m3[*x as usize]);
        for c in cands.iter_mut() {
            for r in c.3.iter_mut() {
                *r = it.next().expect("one decided root per root");
            }
        }
        sp = g3;
    }

    // --- 4. identical roots are the same body ---
    let bodies: Vec<(usize, u8, u64, Vec<NodeId>)> = {
        let mut seen: BTreeSet<(usize, Vec<NodeId>)> = BTreeSet::new();
        cands
            .into_iter()
            .filter(|c| seen.insert((c.0, c.3.clone())))
            .collect()
    };
    (sp, bodies)
}

pub(crate) fn emit_body(e: &mut Emit, outs: &mut [Outcome]) -> Result<()> {
    // Steps 1-4: the fused, fork-free graph and its bodies (shared with the
    // ASM backend, so both emit exactly this compute).
    let outs_spec: Vec<(Vec<NodeId>, NodeId, NodeId)> = outs
        .iter()
        .map(|o| (o.of.fields.iter().map(|f| f.node).collect(), o.ok, o.live))
        .collect();
    let (mut sp, bodies) =
        specialize_frame(&e.graph, &outs_spec, e.fork_depth as u8, e.decide, e.room.as_ref());

    // --- 5. the row key, as graph nodes ---
    let mut hash_of: Vec<Option<(NodeId, NodeId)>> = vec![None; bodies.len()];
    if e.row_key {
        for (oi, o) in outs.iter().enumerate() {
            let mine: Vec<usize> = bodies
                .iter()
                .enumerate()
                .filter(|(_, b)| b.0 == oi)
                .map(|(i, _)| i)
                .collect();
            let Some(&first_body) = mine.first() else {
                continue;
            };
            let agreed = |fi: usize| -> bool {
                let first = bodies[first_body].3[fi];
                !mine.iter().any(|b| bodies[*b].3[fi] != first)
            };
            let mut order: Vec<(u32, usize)> = o
                .of
                .fields
                .iter()
                .enumerate()
                .filter(|(fi, f)| {
                    // A cell the boundary widens to a UNIFORM value (rem,
                    // timers) contributes from the constant KPART, not the
                    // per-lane fold - mirror the boundary and drop it here.
                    f.widen_uniform.is_none()
                        && !(agreed(*fi)
                            && matches!(
                                sp.get(bodies[first_body].3[*fi]).op,
                                Op::Const(..) | Op::ConstBool(_)
                            ))
                })
                .map(|(fi, f)| (f.cell, fi))
                .collect();
            // Agreed (button-independent) cells first, so the additive sum's
            // agreed prefix hash-conses across the outcome's variants; the
            // sum is commutative, so ordering only affects sharing, not the
            // key. The SOUND key is the boundary's SUM of `cell_mix` over the
            // per-lane cells; the uniform-cell prefix and the closing `mix64`
            // are added in `append` (`trace::kernel::render` knows the shape
            // hash and the uniform cells; this layer does not).
            order.sort_by_key(|(cell, fi)| (!agreed(*fi), *cell));
            let zero = sp.leaf(Op::Word(0));
            for bi in mine {
                let (mut h1, mut h2) = (zero, zero);
                for (cell, fi) in &order {
                    let v = bodies[bi].3[*fi];
                    let c1 = sp.add(Op::CellMix(*cell, 0), vec![v]);
                    let c2 = sp.add(Op::CellMix(*cell, 1), vec![v]);
                    h1 = sp.add(Op::AddW, vec![h1, c1]);
                    h2 = sp.add(Op::AddW, vec![h2, c2]);
                }
                hash_of[bi] = Some((h1, h2));
            }
        }
    }

    let n = sp.len();
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
    let mut conj_of: Vec<Vec<NodeId>> = Vec::with_capacity(bodies.len());
    let mut live_of: Vec<Vec<NodeId>> = Vec::with_capacity(bodies.len());
    let mut roots: Vec<NodeId> = Vec::new();
    for (bi, b) in bodies.iter().enumerate() {
        let nf = outs[b.0].of.fields.len();
        roots.extend(b.3[..nf].iter().copied());
        if let Some((h1, h2)) = hash_of[bi] {
            roots.push(h1);
            roots.push(h2);
        }
        let cs: Vec<NodeId> = conjuncts(&sp, b.3[nf])
            .into_iter()
            .filter(|c| !vacuous(&ctx, *c))
            .collect();
        roots.extend(cs.iter().copied());
        conj_of.push(cs);
        let ls: Vec<NodeId> = conjuncts(&sp, b.3[nf + 1])
            .into_iter()
            .filter(|c| !vacuous(&ctx, *c))
            .collect();
        roots.extend(ls.iter().copied());
        live_of.push(ls);
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
                let name = format!("n{}", id);
                ctx.expr[id as usize] = Some(name.clone());
                ctx.name[id as usize] = Some(name);
            }
        }
    }

    // --- the body: input loads, then every live node, in id order ---
    //
    // No placement and no nest. Ids only refer downward, so id order is
    // a topological order, and there is no scope for a node to be in
    // any more.
    let mut body: Vec<Line> = Vec::new();
    for (id, kind) in &e.vary_in {
        let load = match *kind {
            "num" => format!("let r_c{}: ZN = rin.c{};", id, id),
            "ival" => format!("let r_c{}: ZI = rin.c{};", id, id),
            "bool" => format!("let r_c{}: ZB = ZB {{ val: rin.c{}, known: ALL }};", id, id),
            other => bail!("varying cell {} has kind {:?}", id, other),
        };
        body.push(Line::Raw(load));
    }
    for id in 0..n as NodeId {
        if !live[id as usize] {
            continue;
        }
        emit_node(&ctx, &mut body, id)?;
    }

    // --- one variant per body ---
    let mut variants: Vec<Variant> = Vec::new();
    for (bi, b) in bodies.iter().enumerate() {
        let (oi, mask) = (b.0, b.1);
        let sfx = format!("_b{}", bi);
        let (mut lanes, mut blocks) = (Vec::new(), Vec::new());
        for c in &conj_of[bi] {
            match conjunct_term(&ctx, *c)? {
                Term::Lanes(m) => lanes.push(m),
                Term::Block(x) => blocks.push(x),
            }
        }
        body.push(Line::Let {
            name: format!("ok_v{}{}", mask, sfx),
            ty: "u16",
            expr: if lanes.is_empty() {
                "ALL".into()
            } else {
                format!("ALL & {}", lanes.join(" & "))
            },
        });
        body.push(Line::Let {
            name: format!("bd_v{}{}", mask, sfx),
            ty: "bool",
            expr: if blocks.is_empty() { "false".into() } else { blocks.join(" || ") },
        });
        // ALWAYS emitted, even at one outcome. The loop path can fall
        // back on `valid{d}` because its `continue` has already taken
        // the invalid lanes off; here nothing has, so a body's fork
        // validity has to reach `take` through `live` or the rows of a
        // configuration a lane is not in would be written anyway.
        {
            let (mut lanes, mut blocks) = (Vec::new(), Vec::new());
            for c in &live_of[bi] {
                match conjunct_term(&ctx, *c)? {
                    Term::Lanes(m) => lanes.push(m),
                    Term::Block(x) => blocks.push(x),
                }
            }
            // Trivially-ALL `live` emits no line, matching the old
            // renderer's elision.
            if !(lanes.is_empty() && blocks.is_empty()) {
                let m = if lanes.is_empty() {
                    "ALL".to_string()
                } else {
                    format!("ALL & {}", lanes.join(" & "))
                };
                let expr = if blocks.is_empty() {
                    m
                } else {
                    format!("if {} {{ 0 }} else {{ {} }}", blocks.join(" || "), m)
                };
                body.push(Line::Let { name: format!("live_v{}{}", mask, sfx), ty: "u16", expr });
            }
        }
        let mut outputs: BTreeMap<u32, String> = BTreeMap::new();
        for (fi, f) in outs[oi].of.fields.iter().enumerate() {
            let node = b.3[fi];
            let have = ctx.repr[node as usize];
            let want = repr_of_ty(f.ty)?;
            outputs.insert(
                f.cell,
                coerce(ctx.expr[node as usize].as_deref().unwrap(), have, want)?,
            );
        }
        let mut per: Vec<Option<VarOut>> = (0..outs.len()).map(|_| None).collect();
        per[oi] = Some(VarOut { outputs });
        variants.push(Variant { per });
    }

    // --- which cells actually vary, per outcome ---
    let first_body: Vec<Option<usize>> =
        (0..outs.len()).map(|oi| bodies.iter().position(|b| b.0 == oi)).collect();
    for (oi, o) in outs.iter_mut().enumerate() {
        let Some(fb) = first_body[oi] else {
            continue;
        };
        for (fi, f) in o.of.fields.iter_mut().enumerate() {
            let first = variants[fb].per[oi]
                .as_ref()
                .expect("the first body of an outcome writes it")
                .outputs[&f.cell]
                .clone();
            f.tainted = variants
                .iter()
                .filter_map(|v| v.per[oi].as_ref())
                .any(|p| p.outputs[&f.cell] != first);
            f.konst = if f.tainted {
                None
            } else {
                match sp.get(bodies[fb].3[fi]).op {
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
            f.konst_av = if f.tainted {
                None
            } else {
                use celeste_engine::runtime2::AV;
                use celeste_core::pico8_num::Pico8Num;
                match sp.get(bodies[fb].3[fi]).op {
                    Op::Const(lo, hi) if lo == hi => Some(AV::Num(Pico8Num::from_raw(lo))),
                    Op::Const(lo, hi) => {
                        Some(AV::Ival(Pico8Num::from_raw(lo), Pico8Num::from_raw(hi)))
                    }
                    Op::ConstBool(b) => Some(AV::Bool(b)),
                    _ => None,
                }
            };
            f.expr = first;
        }
    }

    e.body = body;
    e.variants = variants;
    Ok(())
}

/// Let-bind one node.
fn emit_node(ctx: &Ctx, buf: &mut Vec<Line>, id: NodeId) -> Result<()> {
    let Some(name) = ctx.name[id as usize].clone() else {
        return Ok(()); // an inline leaf
    };
    let ty = ctx.r(id).ty();
    buf.push(Line::Let { name, ty, expr: ctx.render(id)? });
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
