//! THE DOUBLES' KERNELS, operation by operation, against the reference
//! engine's own arithmetic (`LuaF64`, `LuaF64Interval`: what `refdomain`
//! computes per lane), bit for bit: `-0` against `0`, ties, subnormals,
//! overflow, the forks' cut points. Sixteen lanes a call (the body's two
//! halves), every lane compared.

use std::collections::HashMap;

use celeste_core::arith::{Arithmetic, Float};
use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_core::lua_f64::{LuaF64, LuaF64Interval};

use crate::transpile::asm::{check_mxcsr, compile_and_load_reprs, AsmCtx, AsmOptions, CellRepr, CollisionEnv, Compiled, Loaded, RootKind};
use crate::transpile::graph::{CellKind, Graph, NodeId, Op};

/// A lane's input cell: a number, an interval, a boolean.
#[derive(Clone, Copy, Debug)]
enum In {
    N(f64),
    I(f64, f64),
}

/// A root's value on a lane.
#[derive(Clone, Copy, PartialEq, Debug)]
enum Out {
    /// A number's bits.
    N(u64),
    I(u64, u64),
    /// `(val, known)`.
    B(bool, bool),
}

fn n(v: f64) -> LuaF64 {
    LuaF64::new(v).expect("finite")
}

fn iv(i: In) -> LuaF64Interval {
    match i {
        In::N(v) => LuaF64Interval::point(n(v)),
        In::I(lo, hi) => LuaF64Interval::new(n(lo), n(hi)),
    }
}

fn lit(g: &mut Graph<Float>, v: f64) -> NodeId {
    let b = Float::lit(n(v));
    g.leaf(Op::Const(b, b))
}

/// A float kernel of `roots`: cells `0..` (`ival[i]`: cell `i` holds an
/// interval).
fn build(g: &Graph<Float>, roots: &[NodeId], ival: &[bool], tag: &str) -> (Compiled, Loaded) {
    let reprs: HashMap<u32, CellRepr> = ival.iter().enumerate().filter(|(_, w)| **w).map(|(i, _)| (i as u32, CellRepr::Ival)).collect();
    let (c, l) = compile_and_load_reprs(g, roots, tag, &reprs, AsmOptions::default()).expect("compile");
    assert!(c.halves, "a float kernel runs its body on two halves");
    (c, l)
}

/// Run sixteen lanes (`rows[j][i]`: lane `j`'s cell `i`); per lane, per root.
fn run(c: &Compiled, l: &Loaded, rows: &[Vec<In>], env: Option<&CollisionEnv>) -> Vec<Vec<Out>> {
    assert!(rows.len() <= 16);
    check_mxcsr().expect("the floating-point environment");
    let (ib, ob) = (c.input_bytes as usize, c.out_bytes as usize);
    let mut inbuf = vec![0u8; 2 * ib];
    let mut out = vec![0u8; 2 * ob];
    for j in 0..16 {
        // Unused lanes repeat lane 0.
        let row = &rows[if j < rows.len() { j } else { 0 }];
        let (half, k) = (j / 8, j % 8);
        for (ci, cell) in c.input_cells.iter().enumerate() {
            let off = half * ib + c.input_offsets[ci] as usize + 8 * k;
            let put = |buf: &mut Vec<u8>, at: usize, v: f64| buf[at..at + 8].copy_from_slice(&v.to_bits().to_le_bytes());
            match (c.input_reprs[ci], row[*cell as usize]) {
                (CellRepr::Num, In::N(v)) => put(&mut inbuf, off, v),
                (CellRepr::Ival, In::I(lo, hi)) => {
                    put(&mut inbuf, off, lo);
                    put(&mut inbuf, off + 64, hi);
                }
                (CellRepr::Ival, In::N(v)) => {
                    put(&mut inbuf, off, v);
                    put(&mut inbuf, off + 64, v);
                }
                (r, v) => panic!("cell {cell}: {v:?} into a {r:?} input"),
            }
        }
    }
    let env_ptr = env.map_or(std::ptr::null(), |e| e as *const CollisionEnv as *const std::os::raw::c_void);
    let ctx = AsmCtx::new(env_ptr);
    // SAFETY: the buffers have the layout `c` states (two halves).
    unsafe { (l.func)(inbuf.as_ptr(), out.as_mut_ptr(), &ctx as *const AsmCtx as *const std::os::raw::c_void) };
    (0..rows.len())
        .map(|j| {
            let (half, k) = (j / 8, j % 8);
            (0..c.n_roots)
                .map(|r| {
                    let base = half * ob + c.root_offsets[r] as usize;
                    let q = |at: usize| u64::from_le_bytes(out[at..at + 8].try_into().expect("8 bytes"));
                    match c.root_kinds[r] {
                        RootKind::Num => Out::N(q(base + 8 * k)),
                        RootKind::Ival => Out::I(q(base + 8 * k), q(base + 64 + 8 * k)),
                        RootKind::Bool => Out::B(out[base] >> k & 1 == 1, out[base + 2] >> k & 1 == 1),
                    }
                })
                .collect()
        })
        .collect()
}

/// The doubles every operation is tried on: zeros of both signs,
/// subnormals, ties at the cut points, integers, huge values.
fn specials() -> Vec<f64> {
    let mut v = vec![
        0.0,
        -0.0,
        f64::from_bits(1),
        -f64::from_bits(1),
        f64::MIN_POSITIVE,
        -f64::MIN_POSITIVE,
        f64::MIN_POSITIVE / 2.0,
        -f64::MIN_POSITIVE / 3.0,
        0.5,
        -0.5,
        0.5f64.next_down(),
        (-0.5f64).next_down(),
        1.5,
        -1.5,
        1.0,
        -1.0,
        2.0,
        0.3,
        -0.7,
        2.5,
        -3.25,
        7.0,
        1e300,
        -1e300,
        f64::MAX,
        -f64::MAX,
        123.456,
        -0.21,
        0.05,
    ];
    let mut s = 0x2545_f491_4f6c_dd1du64;
    for _ in 0..40 {
        s ^= s << 13;
        s ^= s >> 7;
        s ^= s << 17;
        v.push((s % 1_000_000) as f64 / 997.0 - 500.0);
    }
    v
}

/// Intervals from the specials: points, `[-0, 0]`, ordered pairs.
fn intervals() -> Vec<(f64, f64)> {
    let sp = specials();
    let mut out = vec![(-0.0, 0.0), (0.0, 0.0), (-0.0, -0.0), (-0.5, 0.5f64.next_down()), (0.5f64.next_down(), 0.5), (-1.0, -0.0), (-0.0, 3.5)];
    for (i, a) in sp.iter().enumerate() {
        let b = sp[(i * 7 + 3) % sp.len()];
        let (lo, hi) = if a <= &b { (*a, b) } else { (b, *a) };
        out.push((lo, hi));
        out.push((*a, *a));
    }
    out
}

fn bits(v: LuaF64) -> u64 {
    v.to_bits()
}

/// Every lane of `rows` through `roots`, against `want` (per lane, per
/// root; `None`: not compared, a lane the reference refuses).
fn check(g: &Graph<Float>, roots: &[NodeId], ival: &[bool], rows: &[Vec<In>], want: impl Fn(&[In]) -> Vec<Option<Out>>, what: &str) {
    let (c, l) = build(g, roots, ival, what);
    let mut compared = 0;
    for chunk in rows.chunks(16) {
        let got = run(&c, &l, chunk, None);
        for (row, g) in chunk.iter().zip(&got) {
            for (r, (w, g)) in want(row).iter().zip(g).enumerate() {
                if let Some(w) = w {
                    assert_eq!(g, w, "{what}: root {r} at {row:?}");
                    compared += 1;
                }
            }
        }
    }
    assert!(compared > 0, "{what}: nothing compared");
}

/// A two-operand operation over numbers or intervals, and its own error.
fn binary(op: Op<Float>, wide: bool, refer: impl Fn(LuaF64Interval, LuaF64Interval) -> Option<LuaF64Interval>, what: &str) {
    let mut g = Graph::<Float>::default();
    let (x, y) = (g.leaf(Op::Cell(0)), g.leaf(Op::Cell(1)));
    if wide {
        g.set_cell_kind(0, CellKind::Ival);
        g.set_cell_kind(1, CellKind::Ival);
    }
    let r = g.add(op, vec![x, y]);
    let ok = g.add(Op::NoWrap, vec![r]);
    let rows: Vec<Vec<In>> = if wide {
        let iv = intervals();
        let mut v = Vec::new();
        for (i, a) in iv.iter().enumerate() {
            for b in iv.iter().skip(i % 5).step_by(9) {
                v.push(vec![In::I(a.0, a.1), In::I(b.0, b.1)]);
            }
        }
        v
    } else {
        let sp = specials();
        sp.iter().flat_map(|a| sp.iter().map(move |b| vec![In::N(*a), In::N(*b)])).collect()
    };
    check(
        &g,
        &[r, ok],
        &[wide, wide],
        &rows,
        |row| {
            let w = refer(iv(row[0]), iv(row[1]));
            let value = w.map(|w| if wide { Out::I(bits(w.lo), bits(w.hi)) } else { Out::N(bits(w.as_point().expect("a point"))) });
            vec![value, Some(Out::B(w.is_some(), true))]
        },
        what,
    );
}

#[test]
fn float_arithmetic_is_the_references_bit_for_bit() {
    binary(Op::Add, false, |a, b| a.checked_add(b), "fadd");
    binary(Op::Sub, false, |a, b| a.checked_sub(b), "fsub");
    binary(Op::Mul, false, |a, b| a.checked_mul(b), "fmul");
    binary(Op::Div, false, |a, b| a.checked_div(b), "fdiv");
    binary(Op::Add, true, |a, b| a.checked_add(b), "fiadd");
    binary(Op::Sub, true, |a, b| a.checked_sub(b), "fisub");
    binary(Op::Mul, true, |a, b| a.checked_mul(b), "fimul");
    binary(Op::Div, true, |a, b| a.checked_div(b), "fidiv");
    // `%`: points only (the reference refuses an interval).
    binary(Op::Rem, false, |a, b| a.as_point().expect("p").checked_rem(b.as_point().expect("p")).map(LuaF64Interval::point), "frem");
}

/// `min`/`max`: LuaJIT's, the second argument on a tie (`min(0, -0)` is
/// `-0`, `min(-0, 0)` is `0`), end by end on intervals.
#[test]
fn float_min_max_take_the_second_argument_on_a_tie() {
    for (op, name) in [(Op::Min, "fmin"), (Op::Max, "fmax")] {
        let pick = |a: LuaF64, b: LuaF64| if matches!(op, Op::Min) { a.lua_min(b) } else { a.lua_max(b) };
        for wide in [false, true] {
            let mut g = Graph::<Float>::default();
            let (x, y) = (g.leaf(Op::Cell(0)), g.leaf(Op::Cell(1)));
            if wide {
                g.set_cell_kind(0, CellKind::Ival);
                g.set_cell_kind(1, CellKind::Ival);
            }
            let r = g.add(op.clone(), vec![x, y]);
            let rows: Vec<Vec<In>> = if wide {
                let iv = intervals();
                iv.iter().enumerate().flat_map(|(i, a)| iv.iter().skip(i % 3).step_by(7).map(move |b| vec![In::I(a.0, a.1), In::I(b.0, b.1)])).collect()
            } else {
                let sp = specials();
                sp.iter().flat_map(|a| sp.iter().map(move |b| vec![In::N(*a), In::N(*b)])).collect()
            };
            check(
                &g,
                &[r],
                &[wide, wide],
                &rows,
                |row| {
                    let (a, b) = (iv(row[0]), iv(row[1]));
                    let (lo, hi) = (pick(a.lo, b.lo), pick(a.hi, b.hi));
                    vec![Some(if wide { Out::I(bits(lo), bits(hi)) } else { Out::N(bits(lo)) })]
                },
                name,
            );
        }
        // The tie itself, both orders.
        let mut g = Graph::<Float>::default();
        let (x, y) = (g.leaf(Op::Cell(0)), g.leaf(Op::Cell(1)));
        let r = g.add(op.clone(), vec![x, y]);
        let (c, l) = build(&g, &[r], &[false, false], name);
        let got = run(&c, &l, &[vec![In::N(0.0), In::N(-0.0)], vec![In::N(-0.0), In::N(0.0)]], None);
        assert_eq!(got[0][0], Out::N((-0.0f64).to_bits()), "{name}(0, -0) is the second");
        assert_eq!(got[1][0], Out::N(0.0f64.to_bits()), "{name}(-0, 0) is the second");
    }
}

/// Negation, `abs`, `flr` (the low end's, on a lane spanning one floor),
/// the tools' `sin` (a call-out to glibc's), number by number.
#[test]
fn float_unary_operations_are_the_references() {
    let sp = specials();
    for wide in [false, true] {
        let mut g = Graph::<Float>::default();
        let x = g.leaf(Op::Cell(0));
        if wide {
            g.set_cell_kind(0, CellKind::Ival);
        }
        let (neg, abs, flr) = (g.add(Op::Neg, vec![x]), g.add(Op::Abs, vec![x]), g.add(Op::Flr, vec![x]));
        let rows: Vec<Vec<In>> = if wide { intervals().into_iter().map(|(a, b)| vec![In::I(a, b)]).collect() } else { sp.iter().map(|v| vec![In::N(*v)]).collect() };
        check(
            &g,
            &[neg, abs, flr],
            &[wide],
            &rows,
            |row| {
                let a = iv(row[0]);
                let one = |i: LuaF64Interval| if wide { Out::I(bits(i.lo), bits(i.hi)) } else { Out::N(bits(i.lo)) };
                let one_floor = a.lo.flr().value() == a.hi.flr().value();
                vec![Some(one(a.neg())), Some(one(a.abs())), one_floor.then(|| Out::N(bits(a.lo.flr())))]
            },
            if wide { "fiunary" } else { "funary" },
        );
    }
    // `sin` of numbers in turns (the cart's), extremes kept finite.
    let mut g = Graph::<Float>::default();
    let x = g.leaf(Op::Cell(0));
    let s = g.add(Op::Sin, vec![x]);
    let rows: Vec<Vec<In>> = sp.iter().filter(|v| v.abs() < 1e15).map(|v| vec![In::N(*v)]).collect();
    check(&g, &[s], &[false], &rows, |row| vec![Some(Out::N(bits(iv(row[0]).lo.lua_sin().expect("finite"))))], "fsin");
}

/// Comparisons: on numbers the cart's (`-0 == 0`), on intervals tri-state
/// as the reference decides them (separated or two points); `Same` is bit
/// identity.
#[test]
fn float_comparisons_decide_as_the_reference() {
    let iv_rows: Vec<Vec<In>> = {
        let iv = intervals();
        iv.iter().enumerate().flat_map(|(i, a)| iv.iter().skip(i % 4).step_by(6).map(move |b| vec![In::I(a.0, a.1), In::I(b.0, b.1)])).collect()
    };
    let num_rows: Vec<Vec<In>> = {
        let sp = specials();
        sp.iter().flat_map(|a| sp.iter().map(move |b| vec![In::N(*a), In::N(*b)])).collect()
    };
    for wide in [false, true] {
        let mut g = Graph::<Float>::default();
        let (x, y) = (g.leaf(Op::Cell(0)), g.leaf(Op::Cell(1)));
        if wide {
            g.set_cell_kind(0, CellKind::Ival);
            g.set_cell_kind(1, CellKind::Ival);
        }
        let ops = [Op::Lt, Op::Le, Op::Gt, Op::Ge, Op::Eq];
        let roots: Vec<NodeId> = ops.iter().map(|o| g.add(o.clone(), vec![x, y])).collect();
        let mut all = roots.clone();
        if !wide {
            all.push(g.add(Op::Same, vec![x, y]));
        }
        check(
            &g,
            &all,
            &[wide, wide],
            if wide { &iv_rows } else { &num_rows },
            |row| {
                let (a, b) = (iv(row[0]), iv(row[1]));
                let (al, ah, bl, bh) = (a.lo.value(), a.hi.value(), b.lo.value(), b.hi.value());
                // `refdomain::compare`: definite true, definite false, else unknown.
                let tri = |t: bool, f: bool| Out::B(t, t || f);
                let mut v = vec![
                    Some(tri(ah < bl, bh <= al)),
                    Some(tri(ah <= bl, bh < al)),
                    Some(tri(bh < al, ah <= bl)),
                    Some(tri(bh <= al, ah < bl)),
                    Some(match (a.as_point(), b.as_point()) {
                        (Some(p), Some(q)) => Out::B(p.value() == q.value(), true),
                        _ => Out::B(false, ah < bl || bh < al),
                    }),
                ];
                if !wide {
                    v.push(Some(Out::B(a.lo.to_bits() == b.lo.to_bits(), true)));
                }
                v
            },
            if wide { "ficmp" } else { "fcmp" },
        );
    }
}

/// The forks' fragments: `floor_fragment` and `round_fragment` per
/// configuration, their validity and coverage, around zeros and the cut
/// below 1 (`0.5 - 2^-54 + 0.5` rounds to 1).
#[test]
fn float_fork_fragments_are_the_references() {
    let rows: Vec<Vec<In>> = intervals()
        .into_iter()
        .filter(|(lo, hi)| hi - lo < 4.0 && lo.abs() < 1e6)
        .chain([(-0.5, 0.5f64.next_down()), (0.5f64.next_down(), 1.5), (-0.0, 0.7), (-1.5, -0.5), ((-0.5f64).next_down(), -0.5), (2.5, 3.5f64.next_down())])
        .map(|(a, b)| vec![In::I(a, b)])
        .collect();
    let ways = 3u8;
    let mut g = Graph::<Float>::default();
    let x = g.leaf(Op::Cell(0));
    g.set_cell_kind(0, CellKind::Ival);
    let mut roots = Vec::new();
    for c in 0..ways {
        roots.push(g.add(Op::Frag(c), vec![x]));
        roots.push(g.add(Op::FragOk(c), vec![x]));
        roots.push(g.add(Op::IntFrag(c), vec![x]));
        roots.push(g.add(Op::RFrag(c), vec![x]));
        roots.push(g.add(Op::RFragOk(c), vec![x]));
    }
    roots.push(g.add(Op::SplitOk(ways), vec![x]));
    roots.push(g.add(Op::RSplitOk(ways), vec![x]));
    let flr = g.add(Op::Flr, vec![x]);
    roots.push(g.add(Op::Known, vec![flr]));
    check(
        &g,
        &roots,
        &[true],
        &rows,
        |row| {
            let a = iv(row[0]);
            let floors = a.hi.flr().value() - a.lo.flr().value();
            let rounds = a.rounds().expect("rounds").expect("few");
            let mut v = Vec::new();
            for c in 0..ways {
                let fv = floors >= c as f64;
                let f = a.floor_fragment(c as u32);
                v.push(fv.then(|| Out::I(bits(f.lo), bits(f.hi))));
                v.push(Some(Out::B(fv, true)));
                v.push(fv.then(|| Out::N(bits(f.lo))));
                let rv = (c as u32) < rounds;
                v.push(rv.then(|| {
                    let r = a.round_fragment(c as u32).expect("a fragment");
                    Out::I(bits(r.lo), bits(r.hi))
                }));
                v.push(Some(Out::B(rv, true)));
            }
            v.push(Some(Out::B(floors <= ways as f64 - 1.0, true)));
            v.push(Some(Out::B(rounds <= ways as u32, true)));
            v.push(Some(Out::B(a.lo.flr().value() == a.hi.flr().value(), true)));
            v
        },
        "ffork",
    );
}

/// Literal operands (a broadcast constant), and a sum the cart rounds:
/// `0.1 + 0.2` is not `0.3`, and no fused multiply-add joins `x * y + z`.
#[test]
fn float_literals_and_rounding_are_the_carts() {
    let mut g = Graph::<Float>::default();
    let (x, y) = (g.leaf(Op::Cell(0)), g.leaf(Op::Cell(1)));
    let tenth = lit(&mut g, 0.1);
    let s = g.add(Op::Add, vec![x, tenth]);
    let p = g.add(Op::Mul, vec![x, y]);
    let fma = g.add(Op::Add, vec![p, tenth]);
    let rows: Vec<Vec<In>> = specials().iter().filter(|v| v.abs() < 1e100).map(|v| vec![In::N(*v), In::N(1.0 / 3.0)]).collect();
    check(
        &g,
        &[s, fma],
        &[false, false],
        &rows,
        |row| {
            let (a, b) = (iv(row[0]).lo, iv(row[1]).lo);
            let t = n(0.1);
            let prod = a.checked_mul(b).expect("finite");
            vec![Some(Out::N(bits(a.checked_add(t).expect("finite")))), Some(Out::N(bits(prod.checked_add(t).expect("finite"))))]
        },
        "flit",
    );
    let (c, _) = build(&g, &[fma], &[false, false], "ffma");
    assert!(c.halves);
}

/// The collision look-ups on whole-number coordinates: the 16.16 call-outs'
/// answers.
#[test]
fn float_collision_lookups_are_the_16_16_ones() {
    let cart = CartData::load("cart").expect("cart");
    let cache = CollisionCache::new(&cart, 1, 0).expect("cache");
    let env = CollisionEnv { cart: &cart, cache: &cache };
    let mut g = Graph::<Float>::default();
    let (x, y) = (g.leaf(Op::Cell(0)), g.leaf(Op::Cell(1)));
    let m = g.add(Op::Mget, vec![x, y]);
    let (w, h, f) = (lit(&mut g, 8.0), lit(&mut g, 6.0), lit(&mut g, 0.0));
    let t = g.add(Op::TileFlagAt, vec![x, y, w, h, f]);
    let (c, l) = build(&g, &[m, t], &[false, false], "fcoll");
    let rows: Vec<Vec<In>> = (0..16).map(|i| vec![In::N((i * 7 % 128) as f64), In::N((i * 5 % 16) as f64 + 16.0)]).collect();
    let got = run(&c, &l, &rows, Some(&env));
    for (row, got) in rows.iter().zip(&got) {
        let (In::N(xv), In::N(yv)) = (row[0], row[1]) else { unreachable!() };
        let (xi, yi) = (xv as i16, yv as i16);
        let tile = cart.mget_whole(xi, yi) as f64;
        let hit = cache.flag_at(&cart, xi, yi, 8, 6, 0).expect("flag_at");
        assert_eq!(got[0], Out::N(tile.to_bits()), "mget at {row:?}");
        assert_eq!(got[1], Out::B(hit, true), "tile_flag_at at {row:?}");
    }
}

/// A kernel text holding a fused multiply-add or an MXCSR write is refused
/// before it is assembled.
#[test]
fn a_fused_multiply_add_is_refused() {
    for text in ["    vfmadd231pd %zmm1, %zmm2, %zmm3\n", "    vfnmsub213sd %xmm1, %xmm2, %xmm3\n", "    ldmxcsr (%rsp)\n"] {
        let asm = format!(".text\n.globl k\nk:\n{text}    ret\n");
        let e = crate::transpile::asm::assemble(&asm, "refuse").expect_err("refused");
        assert!(format!("{e}").contains("forbidden"), "{e}");
    }
}
