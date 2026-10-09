//! DIAGNOSTIC (not for merge): one-fork duplicate bodies as SMT queries.
//!
//! `CELESTE_SMT_DUMP=DIR` (+ `CELESTE_SMT_TSV=body-stats.tsv`): per kernel in
//! the TSV, one SMT-LIB2 file encoding the kernel's EXACT lane computation
//! (`transpile::asm::codegen::lower_node`: numbers as one i32, intervals as
//! two i32 planes, booleans as (val, known); wrapping arithmetic; callouts
//! (Div, Rem, Sin, Mget, TileFlag) uninterpreted) and per body `j` that took
//! lanes the query
//!   take_j & AND_{i in refs(j)} !(take_i & every compared root equal)
//! with `take = (live.val | !live.known) & !(error.val | !error.known)`, i.e.
//! "a lane of j that the `DupRef` mask would NOT drop". UNSAT = j never emits
//! a row of its own on ANY input (inputs are unconstrained beyond their
//! representation; the region and pin premises are inside `error`).

use std::collections::HashMap;
use std::fmt::Write as _;

use crate::transpile::asm::CellRepr;
use crate::transpile::graph::{Graph, NodeId, Op};

#[derive(Clone, Copy, PartialEq)]
enum Dom {
    Num,
    Bool,
    Ival,
}

/// A numeric operand: its SMT term and, when codegen sees a constant
/// (`NumVal::ConstI32`), the constant.
#[derive(Clone)]
struct N {
    t: String,
    k: Option<i32>,
}

#[derive(Clone)]
enum V {
    Num(N),
    Ival(N, N),
    Bool(String, String),
}

fn lit(x: i32) -> String {
    format!("#x{:08x}", x as u32)
}

fn num(t: String) -> N {
    N { t, k: None }
}

pub(crate) struct Enc<'a> {
    g: &'a Graph,
    reprs: &'a HashMap<u32, CellRepr>,
    vals: HashMap<NodeId, V>,
    pub decls: String,
    pub defs: String,
    /// Range assertions, after the declarations.
    pub bounds: String,
    pub cells: Vec<(u32, CellRepr)>,
    pub ufs: std::collections::BTreeSet<String>,
    ival_ordered: bool,
    /// Input cells known to lie in a range (raw, inclusive; both ends of an interval).
    pub ranges: HashMap<u32, (i32, i32)>,
    /// EMPIRICAL (not a proof): the frame's observed (min, max, bool flags) per input cell.
    pub seen: HashMap<u32, (i32, i32, u8, u32)>,
}

impl<'a> Enc<'a> {
    pub(crate) fn new(g: &'a Graph, reprs: &'a HashMap<u32, CellRepr>, ival_ordered: bool) -> Self {
        Enc { g, reprs, vals: HashMap::new(), decls: String::new(), defs: String::new(), bounds: String::new(), cells: Vec::new(), ufs: Default::default(), ival_ordered, ranges: HashMap::new(), seen: HashMap::new() }
    }

    fn dom(&self, id: NodeId) -> Dom {
        match self.vals[&id] {
            V::Num(_) => Dom::Num,
            V::Bool(..) => Dom::Bool,
            V::Ival(..) => Dom::Ival,
        }
    }
    fn n(&self, id: NodeId) -> N {
        match &self.vals[&id] {
            V::Num(n) => n.clone(),
            _ => panic!("smt: node {id} is not a number"),
        }
    }
    fn iv(&self, id: NodeId) -> (N, N) {
        match &self.vals[&id] {
            V::Ival(a, b) => (a.clone(), b.clone()),
            V::Num(n) => (n.clone(), n.clone()),
            _ => panic!("smt: node {id} is not an interval"),
        }
    }
    fn b(&self, id: NodeId) -> (String, String) {
        match &self.vals[&id] {
            V::Bool(v, k) => (v.clone(), k.clone()),
            _ => panic!("smt: node {id} is not a boolean"),
        }
    }
    fn uf(&mut self, name: &str, n_args: usize, ret: &str) {
        if self.ufs.insert(name.to_string()) {
            let args = vec!["(_ BitVec 32)"; n_args].join(" ");
            let _ = writeln!(self.decls, "(declare-fun {name} ({args}) {ret})");
        }
    }
    /// A named definition of a term (sharing in the file).
    fn def(&mut self, name: String, sort: &str, t: String) -> String {
        let _ = writeln!(self.defs, "(define-fun {name} () {sort} {t})");
        name
    }

    /// Encode every node in the cone of `roots` (memoized across calls).
    pub(crate) fn cone(&mut self, roots: &[NodeId]) {
        let mut stack: Vec<(NodeId, bool)> = roots.iter().map(|&r| (r, false)).collect();
        while let Some((id, expanded)) = stack.pop() {
            if self.vals.contains_key(&id) {
                continue;
            }
            if expanded {
                self.node(id);
            } else {
                stack.push((id, true));
                for &a in &self.g.get(id).args {
                    if !self.vals.contains_key(&a) {
                        stack.push((a, false));
                    }
                }
            }
        }
    }

    fn node(&mut self, id: NodeId) {
        const BV: &str = "(_ BitVec 32)";
        const FLR: i32 = 0xffff_0000u32 as i32;
        const STEP: i32 = 1 << 16;
        let node = self.g.get(id).clone();
        let a = node.args.clone();
        let nm = |s: &str| format!("{s}{id}");
        let v: V = match &node.op {
            Op::Const(lo, hi) => {
                if lo == hi {
                    V::Num(N { t: lit(*lo), k: Some(*lo) })
                } else {
                    V::Ival(N { t: lit(*lo), k: Some(*lo) }, N { t: lit(*hi), k: Some(*hi) })
                }
            }
            Op::Cell(c) => {
                let r = self.reprs.get(c).copied().unwrap_or(CellRepr::Num);
                self.cells.push((*c, r));
                if let Some(&(lo, hi, fl, fr)) = self.seen.get(c) {
                    let terms: Vec<String> = match r {
                        CellRepr::Num => vec![format!("c{c}")],
                        CellRepr::Ival => vec![format!("c{c}l"), format!("c{c}h")],
                        _ => Vec::new(),
                    };
                    if lo <= hi {
                        for t in terms {
                            let _ = writeln!(self.bounds, "(assert (and (bvsle {} {t}) (bvsle {t} {})))", lit(lo), lit(hi));
                            // Only whole numbers seen: whole numbers.
                            if fr == 0 {
                                let _ = writeln!(self.bounds, "(assert (= ((_ extract 15 0) {t}) #x0000))");
                            }
                        }
                    }
                    let k = if r == CellRepr::UBool { format!("c{c}k") } else { "true".into() };
                    if matches!(r, CellRepr::Bool | CellRepr::UBool) && fl != 0 {
                        let mut alts = Vec::new();
                        if fl & 1 != 0 { alts.push(format!("(and {k} c{c}v)")); }
                        if fl & 2 != 0 { alts.push(format!("(and {k} (not c{c}v))")); }
                        if fl & 4 != 0 { alts.push(format!("(not {k})")); }
                        let _ = writeln!(self.bounds, "(assert (or {}))", alts.join(" "));
                    }
                }
                if let Some(&(lo, hi)) = self.ranges.get(c) {
                    let terms: Vec<String> = match r {
                        CellRepr::Num => vec![format!("c{c}")],
                        CellRepr::Ival => vec![format!("c{c}l"), format!("c{c}h")],
                        _ => panic!("smt: a range on boolean cell {c}"),
                    };
                    for t in terms {
                        let _ = writeln!(self.bounds, "(assert (and (bvsle {} {t}) (bvsle {t} {})))", lit(lo), lit(hi));
                    }
                }
                match r {
                    CellRepr::Num => {
                        let _ = writeln!(self.decls, "(declare-const c{c} {BV})");
                        V::Num(num(format!("c{c}")))
                    }
                    CellRepr::Bool => {
                        let _ = writeln!(self.decls, "(declare-const c{c}v Bool)");
                        V::Bool(format!("c{c}v"), "true".into())
                    }
                    CellRepr::UBool => {
                        let _ = writeln!(self.decls, "(declare-const c{c}v Bool)\n(declare-const c{c}k Bool)");
                        V::Bool(format!("c{c}v"), format!("c{c}k"))
                    }
                    CellRepr::Ival => {
                        let _ = writeln!(self.decls, "(declare-const c{c}l {BV})\n(declare-const c{c}h {BV})");
                        if self.ival_ordered {
                            let _ = writeln!(self.decls, "(assert (bvsle c{c}l c{c}h))");
                        }
                        V::Ival(num(format!("c{c}l")), num(format!("c{c}h")))
                    }
                }
            }
            op @ (Op::Add | Op::Sub | Op::Min | Op::Max) => {
                let f = |op: &Op, x: &str, y: &str| match op {
                    Op::Add => format!("(bvadd {x} {y})"),
                    Op::Sub => format!("(bvsub {x} {y})"),
                    Op::Min => format!("(ite (bvslt {x} {y}) {x} {y})"),
                    _ => format!("(ite (bvsgt {x} {y}) {x} {y})"),
                };
                if self.dom(a[0]) == Dom::Ival || self.dom(a[1]) == Dom::Ival {
                    let ((al, ah), (bl, bh)) = (self.iv(a[0]), self.iv(a[1]));
                    let (lo, hi) = if matches!(op, Op::Sub) {
                        (f(op, &al.t, &bh.t), f(op, &ah.t, &bl.t))
                    } else {
                        (f(op, &al.t, &bl.t), f(op, &ah.t, &bh.t))
                    };
                    V::Ival(num(self.def(nm("l"), BV, lo)), num(self.def(nm("h"), BV, hi)))
                } else {
                    let (x, y) = (self.n(a[0]), self.n(a[1]));
                    V::Num(num(self.def(nm("n"), BV, f(op, &x.t, &y.t))))
                }
            }
            Op::Mul => {
                let mul = |x: &str, y: &str| {
                    format!("((_ extract 47 16) (bvmul ((_ sign_extend 32) {x}) ((_ sign_extend 32) {y})))")
                };
                if self.dom(a[0]) == Dom::Ival || self.dom(a[1]) == Dom::Ival {
                    let (ivn, scn) = if self.dom(a[0]) == Dom::Ival { (a[0], a[1]) } else { (a[1], a[0]) };
                    let ((l, h), s) = (self.iv(ivn), self.n(scn));
                    V::Ival(num(self.def(nm("l"), BV, mul(&l.t, &s.t))), num(self.def(nm("h"), BV, mul(&h.t, &s.t))))
                } else {
                    let (x, y) = (self.n(a[0]), self.n(a[1]));
                    // Exact when one factor is a constant; else uninterpreted
                    // (sound: a function of its operands).
                    let t = if x.k.is_some() || y.k.is_some() {
                        mul(&x.t, &y.t)
                    } else {
                        self.uf("mul", 2, BV);
                        format!("(mul {} {})", x.t, y.t)
                    };
                    V::Num(num(self.def(nm("n"), BV, t)))
                }
            }
            Op::Div if self.dom(a[0]) == Dom::Ival => {
                self.uf("div", 2, BV);
                let ((l, h), s) = (self.iv(a[0]), self.n(a[1]));
                V::Ival(
                    num(self.def(nm("l"), BV, format!("(div {} {})", l.t, s.t))),
                    num(self.def(nm("h"), BV, format!("(div {} {})", h.t, s.t))),
                )
            }
            Op::Div | Op::Rem | Op::Sin | Op::Mget => {
                let (f, k) = match node.op {
                    Op::Div => ("div", 2),
                    Op::Rem => ("rem", 2),
                    Op::Sin => ("sin", 1),
                    _ => ("mget", 2),
                };
                self.uf(f, k, BV);
                let args: Vec<String> = (0..k).map(|i| self.n(a[i]).t).collect();
                V::Num(num(self.def(nm("n"), BV, format!("({f} {})", args.join(" ")))))
            }
            Op::Neg => {
                if self.dom(a[0]) == Dom::Ival {
                    let (l, h) = self.iv(a[0]);
                    V::Ival(num(self.def(nm("l"), BV, format!("(bvneg {})", h.t))), num(self.def(nm("h"), BV, format!("(bvneg {})", l.t))))
                } else {
                    let x = self.n(a[0]);
                    V::Num(num(self.def(nm("n"), BV, format!("(bvneg {})", x.t))))
                }
            }
            Op::Abs => {
                let abs = |x: &str| format!("(ite (bvslt {x} #x00000000) (bvneg {x}) {x})");
                if self.dom(a[0]) == Dom::Ival {
                    let (l, h) = self.iv(a[0]);
                    let (al, ah) = (self.def(nm("al"), BV, abs(&l.t)), self.def(nm("ah"), BV, abs(&h.t)));
                    let m = format!("(ite (bvsgt {al} {ah}) {al} {ah})");
                    let pos = format!("(bvsge {} #x00000000)", l.t);
                    let neg = format!("(bvsle {} #x00000000)", h.t);
                    let lo = format!("(ite {pos} {} (ite {neg} {ah} #x00000000))", l.t);
                    let hi = format!("(ite {pos} {} (ite {neg} {al} {m}))", h.t);
                    V::Ival(num(self.def(nm("l"), BV, lo)), num(self.def(nm("h"), BV, hi)))
                } else {
                    let x = self.n(a[0]);
                    V::Num(num(self.def(nm("n"), BV, abs(&x.t))))
                }
            }
            Op::Flr => {
                let (l, _) = self.iv(a[0]);
                V::Num(num(self.def(nm("n"), BV, format!("(bvand {} {})", l.t, lit(FLR)))))
            }
            Op::ConstBool(b) => V::Bool(b.to_string(), "true".into()),
            Op::UnknownBool(_) => V::Bool("false".into(), "false".into()),
            op @ (Op::Lt | Op::Le | Op::Gt | Op::Ge) => {
                if self.dom(a[0]) == Dom::Ival || self.dom(a[1]) == Dom::Ival {
                    let ((al, ah), (bl, bh)) = (self.iv(a[0]), self.iv(a[1]));
                    let (t, f) = match op {
                        Op::Lt => (format!("(bvslt {} {})", ah.t, bl.t), format!("(bvsge {} {})", al.t, bh.t)),
                        Op::Le => (format!("(bvsle {} {})", ah.t, bl.t), format!("(bvsgt {} {})", al.t, bh.t)),
                        Op::Gt => (format!("(bvsgt {} {})", al.t, bh.t), format!("(bvsle {} {})", ah.t, bl.t)),
                        _ => (format!("(bvsge {} {})", al.t, bh.t), format!("(bvslt {} {})", ah.t, bl.t)),
                    };
                    let tv = self.def(nm("v"), "Bool", t);
                    V::Bool(tv.clone(), self.def(nm("k"), "Bool", format!("(or {tv} {f})")))
                } else {
                    let (x, y) = (self.n(a[0]), self.n(a[1]));
                    let f = match op {
                        Op::Lt => "bvslt",
                        Op::Le => "bvsle",
                        Op::Gt => "bvsgt",
                        _ => "bvsge",
                    };
                    V::Bool(self.def(nm("v"), "Bool", format!("({f} {} {})", x.t, y.t)), "true".into())
                }
            }
            Op::Eq => {
                if self.dom(a[0]) == Dom::Ival || self.dom(a[1]) == Dom::Ival {
                    let ((al, ah), (bl, bh)) = (self.iv(a[0]), self.iv(a[1]));
                    let both = format!("(and (= {} {}) (= {} {}))", al.t, ah.t, bl.t, bh.t);
                    let val = self.def(nm("v"), "Bool", format!("(and {both} (= {} {}))", al.t, bl.t));
                    let disjoint = format!("(or (bvsgt {} {}) (bvsgt {} {}))", al.t, bh.t, bl.t, ah.t);
                    V::Bool(val, self.def(nm("k"), "Bool", format!("(or {both} {disjoint})")))
                } else if self.dom(a[0]) == Dom::Bool {
                    let ((pv, pk), (qv, qk)) = (self.b(a[0]), self.b(a[1]));
                    V::Bool(self.def(nm("v"), "Bool", format!("(= {pv} {qv})")), self.def(nm("k"), "Bool", format!("(and {pk} {qk})")))
                } else {
                    let (x, y) = (self.n(a[0]), self.n(a[1]));
                    V::Bool(self.def(nm("v"), "Bool", format!("(= {} {})", x.t, y.t)), "true".into())
                }
            }
            Op::Not => {
                let (v, k) = self.b(a[0]);
                V::Bool(self.def(nm("v"), "Bool", format!("(not {v})")), k)
            }
            op @ (Op::And | Op::Or) => {
                let ((pv, pk), (qv, qk)) = (self.b(a[0]), self.b(a[1]));
                if matches!(op, Op::And) {
                    V::Bool(
                        self.def(nm("v"), "Bool", format!("(and {pv} {qv})")),
                        self.def(nm("k"), "Bool", format!("(or (and {pk} {qk}) (and (not {pv}) {pk}) (and (not {qv}) {qk}))")),
                    )
                } else {
                    V::Bool(
                        self.def(nm("v"), "Bool", format!("(or {pv} {qv})")),
                        self.def(nm("k"), "Bool", format!("(or (and {pk} {qk}) (and {pv} {pk}) (and {qv} {qk}))")),
                    )
                }
            }
            Op::Known => {
                let inner = a[0];
                let inode = self.g.get(inner);
                if inode.op == Op::Flr && self.dom(inode.args[0]) == Dom::Ival {
                    let (l, h) = self.iv(inode.args[0]);
                    V::Bool(self.def(nm("v"), "Bool", format!("(= (bvand {} {f}) (bvand {} {f}))", l.t, h.t, f = lit(FLR))), "true".into())
                } else {
                    match self.dom(inner) {
                        Dom::Bool => (self.b(inner).1, "true".to_string()),
                        Dom::Ival => {
                            let (l, h) = self.iv(inner);
                            (self.def(nm("v"), "Bool", format!("(= {} {})", l.t, h.t)), "true".to_string())
                        }
                        Dom::Num => ("true".to_string(), "true".to_string()),
                    }
                    .into_bool()
                }
            }
            Op::Lo | Op::Hi => {
                if self.dom(a[0]) == Dom::Ival {
                    let (l, h) = self.iv(a[0]);
                    V::Num(if node.op == Op::Lo { l } else { h })
                } else {
                    V::Num(self.n(a[0]))
                }
            }
            Op::Sel => {
                let (cv, _) = self.b(a[0]);
                let (d1, d2) = (self.dom(a[1]), self.dom(a[2]));
                if d1 == Dom::Bool || d2 == Dom::Bool {
                    let ((tv, tk), (fv, fk)) = (self.b(a[1]), self.b(a[2]));
                    V::Bool(self.def(nm("v"), "Bool", format!("(ite {cv} {tv} {fv})")), self.def(nm("k"), "Bool", format!("(ite {cv} {tk} {fk})")))
                } else if d1 == Dom::Ival || d2 == Dom::Ival {
                    let ((tl, th), (fl, fh)) = (self.iv(a[1]), self.iv(a[2]));
                    V::Ival(
                        num(self.def(nm("l"), BV, format!("(ite {cv} {} {})", tl.t, fl.t))),
                        num(self.def(nm("h"), BV, format!("(ite {cv} {} {})", th.t, fh.t))),
                    )
                } else {
                    let (t, f) = (self.n(a[1]), self.n(a[2]));
                    V::Num(num(self.def(nm("n"), BV, format!("(ite {cv} {} {})", t.t, f.t))))
                }
            }
            Op::Span => {
                let ((l, _), (_, h)) = (self.iv(a[0]), self.iv(a[1]));
                V::Ival(l, h)
            }
            Op::Frag(c) | Op::FragOk(c) | Op::IntFrag(c) => {
                let c = *c as i32;
                let (l, h) = self.iv(a[0]);
                let fl = format!("(bvand {} {})", l.t, lit(FLR));
                let base = if c == 0 { fl } else { format!("(bvadd {fl} {})", lit(STEP.wrapping_mul(c))) };
                let base = self.def(nm("base"), BV, base);
                let top = format!("(bvadd {base} {})", lit(STEP - 1));
                let hi = format!("(ite (bvslt {} {top}) {} {top})", h.t, h.t);
                let lo = if c == 0 { l.t.clone() } else { format!("(ite (bvsgt {} {base}) {} {base})", l.t, l.t) };
                match node.op {
                    Op::Frag(_) => V::Ival(num(self.def(nm("l"), BV, lo)), num(self.def(nm("h"), BV, hi))),
                    Op::IntFrag(_) => V::Num(num(self.def(nm("n"), BV, lo))),
                    _ => {
                        let valid = if c == 0 { "true".to_string() } else { format!("(bvsle {base} (bvand {} {}))", h.t, lit(FLR)) };
                        V::Bool(self.def(nm("v"), "Bool", valid), "true".into())
                    }
                }
            }
            Op::SplitOk(ways) => {
                let (l, h) = self.iv(a[0]);
                let t = format!(
                    "(bvsle (bvand {} {f}) (bvadd (bvand {} {f}) {}))",
                    h.t,
                    l.t,
                    lit(STEP.wrapping_mul(*ways as i32 - 1)),
                    f = lit(FLR)
                );
                V::Bool(self.def(nm("v"), "Bool", t), "true".into())
            }
            Op::NoWrap => {
                let x = a[0];
                let xn = self.g.get(x).clone();
                let ok = match (&xn.op, self.dom(x)) {
                    (op @ (Op::Add | Op::Sub), Dom::Ival) => {
                        let ((pl, ph), (ql, qh), (rl, rh)) = (self.iv(xn.args[0]), self.iv(xn.args[1]), self.iv(x));
                        let neg = |t: String| format!("(bvslt {t} #x00000000)");
                        let (ol, oh) = if matches!(op, Op::Sub) {
                            (
                                neg(format!("(bvand (bvxor {} {}) (bvxor {} {}))", pl.t, qh.t, pl.t, rl.t)),
                                neg(format!("(bvand (bvxor {} {}) (bvxor {} {}))", ph.t, ql.t, ph.t, rh.t)),
                            )
                        } else {
                            (
                                neg(format!("(bvand (bvxor {} {}) (bvxor {} {}))", pl.t, rl.t, ql.t, rl.t)),
                                neg(format!("(bvand (bvxor {} {}) (bvxor {} {}))", ph.t, rh.t, qh.t, rh.t)),
                            )
                        };
                        format!("(not (or {ol} {oh}))")
                    }
                    (Op::Neg, Dom::Ival) => {
                        let (pl, _) = self.iv(xn.args[0]);
                        format!("(not (= {} {}))", pl.t, lit(i32::MIN))
                    }
                    _ => "true".into(),
                };
                V::Bool(self.def(nm("v"), "Bool", ok), "true".into())
            }
            Op::TileFlagAt => {
                let (x, y, w, h, f) = (self.n(a[0]), self.n(a[1]), self.n(a[2]), self.n(a[3]), self.n(a[4]));
                let flag = f.k.expect("tile_flag flag must be a constant");
                let t = match (w.k, h.k) {
                    (Some(w), Some(h)) => {
                        let name = format!("tf_{}_{}_{}", w as u32, h as u32, flag as u32);
                        self.uf(&name, 2, "Bool");
                        format!("({name} {} {})", x.t, y.t)
                    }
                    _ => {
                        let name = format!("tfl_{}", flag as u32);
                        self.uf(&name, 4, "Bool");
                        format!("({name} {} {} {} {})", x.t, y.t, w.t, h.t)
                    }
                };
                V::Bool(self.def(nm("v"), "Bool", t), "true".into())
            }
            other => panic!("smt: node {id}: op {other:?} not encoded (the asm slice rejects it too)"),
        };
        self.vals.insert(id, v);
    }

    /// `may(b)`: the kernel's `read_zb_may`.
    pub(crate) fn may(&self, id: NodeId) -> String {
        let (v, k) = self.b(id);
        format!("(or {v} (not {k}))")
    }

    /// Equality of two roots as the `DupRef` mask compares their slots.
    pub(crate) fn eq(&self, x: NodeId, y: NodeId) -> String {
        if x == y {
            return "true".into();
        }
        match (&self.vals[&x], &self.vals[&y]) {
            (V::Num(p), V::Num(q)) => format!("(= {} {})", p.t, q.t),
            (V::Ival(pl, ph), V::Ival(ql, qh)) => format!("(and (= {} {}) (= {} {}))", pl.t, ql.t, ph.t, qh.t),
            (V::Bool(pv, pk), V::Bool(qv, qk)) => format!("(and (= {pv} {qv}) (= {pk} {qk}))"),
            // Kinds differ: `dup_refs` drops such a pair entirely.
            _ => "false".into(),
        }
    }
}

trait IntoBool {
    fn into_bool(self) -> V;
}
impl IntoBool for (String, String) {
    fn into_bool(self) -> V {
        V::Bool(self.0, self.1)
    }
}
