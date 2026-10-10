//! Correctness gate for the asm backend: emitted code vs the real
//! `celeste_engine::kernel` primitives, bit-exact on random inputs.

use std::collections::HashMap;

const ONE_FIXED2: i32 = 0x0002_0000; // +2.0 in 16.16

use celeste_engine::kernel::{
    zb_and, zb_eq, zb_not, zb_or, zi_abs, zi_add, zi_add_wraps, zi_cmp, zi_eq, zi_flr, zi_fork_flr, zi_max, zi_min,
    zi_neg, zi_neg_wraps, zi_span_ok, zi_sub, zi_sub_wraps, zn_abs, zn_add, zn_eq, zn_flr, zn_ge, zn_gt, zn_le, zn_lt, zn_max,
    zn_min, zn_mul, zn_neg, zn_sub, zsel_b, zsel_i, zsel_n,
    zn_div, zn_mget, zn_rem, zn_sin, zn_tile_flag_at, Cmp, ZB, ZI,
    ZN, ALL,
};

use crate::pico8_num::Pico8Num as P8;
use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use crate::transpile::asm::compile_and_load;
use crate::transpile::graph::{Graph, NodeId, Op};

/// A tiny deterministic PRNG so the test needs no `rand` dependency.
struct Lcg(u64);
impl Lcg {
    fn next_u64(&mut self) -> u64 {
        // splitmix64
        self.0 = self.0.wrapping_add(0x9e37_79b9_7f4a_7c15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^ (z >> 31)
    }
    fn i32(&mut self) -> i32 {
        self.next_u64() as i32
    }
}

/// One random 16-lane i32 column per cell.
fn random_columns(cells: &[u32], rng: &mut Lcg) -> HashMap<u32, [i32; 16]> {
    cells
        .iter()
        .map(|c| (*c, std::array::from_fn::<i32, 16, _>(|_| rng.i32())))
        .collect()
}

/// Pack the columns into the input buffer in `input_cells` order.
fn pack_inputs(input_cells: &[u32], cols: &HashMap<u32, [i32; 16]>) -> Vec<u8> {
    let mut buf = vec![0u8; input_cells.len() * 64];
    for (i, c) in input_cells.iter().enumerate() {
        let col = &cols[c];
        for (l, v) in col.iter().enumerate() {
            buf[i * 64 + l * 4..i * 64 + l * 4 + 4].copy_from_slice(&v.to_le_bytes());
        }
    }
    buf
}

/// A node's abstract value in the reference evaluator.
#[derive(Clone, Copy)]
enum V {
    N(ZN),
    B(ZB),
    I(ZI),
}

/// Evaluate every node with the REAL `celeste_engine::kernel` primitives.
fn eval_nodes(
    g: &Graph,
    cols: &HashMap<u32, [i32; 16]>,
    room: Option<(&CartData, &CollisionCache)>,
) -> Vec<V> {
    let zn_of = |col: &[i32; 16]| ZN::from_array(std::array::from_fn(|i| P8::from_raw(col[i])));
    let mut vals: Vec<V> = Vec::with_capacity(g.len());
    for id in 0..g.len() as NodeId {
        let node = g.get(id);
        let n = |k: usize| -> ZN {
            match vals[node.args[k] as usize] {
                V::N(z) => z,
                _ => panic!("node {id}: expected numeric operand"),
            }
        };
        let b = |k: usize| -> ZB {
            match vals[node.args[k] as usize] {
                V::B(z) => z,
                _ => panic!("node {id}: expected boolean operand"),
            }
        };
        let iv = |k: usize| -> ZI {
            match vals[node.args[k] as usize] {
                V::I(z) => z,
                V::N(z) => ZI { lo: z, hi: z },
                _ => panic!("node {id}: expected interval operand"),
            }
        };
        let dom_bool = |k: usize| matches!(vals[node.args[k] as usize], V::B(_));
        let is_i = |k: usize| matches!(vals[node.args[k] as usize], V::I(_));
        let wide = |k: usize| is_i(k);
        let v = match &node.op {
            Op::Const(lo, hi) => {
                if lo == hi {
                    V::N(ZN::from_array([P8::from_raw(*lo); 16]))
                } else {
                    V::I(ZI { lo: ZN::from_array([P8::from_raw(*lo); 16]), hi: ZN::from_array([P8::from_raw(*hi); 16]) })
                }
            }
            Op::ConstBool(x) => V::B(ZB { val: if *x { ALL } else { 0 }, known: ALL }),
            Op::Cell(c) => V::N(zn_of(&cols[c])),
            Op::Add if wide(0) || wide(1) => V::I(zi_add(iv(0), iv(1))),
            Op::Sub if wide(0) || wide(1) => V::I(zi_sub(iv(0), iv(1))),
            Op::Min if wide(0) || wide(1) => V::I(zi_min(iv(0), iv(1))),
            Op::Max if wide(0) || wide(1) => V::I(zi_max(iv(0), iv(1))),
            Op::Add => V::N(zn_add(n(0), n(1))),
            Op::Sub => V::N(zn_sub(n(0), n(1))),
            Op::Min => V::N(zn_min(n(0), n(1))),
            Op::Max => V::N(zn_max(n(0), n(1))),
            Op::Mul => V::N(zn_mul(n(0), n(1))),
            Op::Div => V::N(zn_div(n(0), n(1))),
            Op::Rem => V::N(zn_rem(n(0), n(1))),
            Op::Sin => V::N(zn_sin(n(0))),
            Op::Neg if wide(0) => V::I(zi_neg(iv(0))),
            Op::Abs if wide(0) => V::I(zi_abs(iv(0))),
            Op::Neg => V::N(zn_neg(n(0))),
            Op::Abs => V::N(zn_abs(n(0))),
            Op::Flr if wide(0) => V::N(zi_flr(iv(0))),
            Op::Flr => V::N(zn_flr(n(0))),
            Op::Lt if wide(0) || wide(1) => V::B(zi_cmp(Cmp::Lt, iv(0), iv(1))),
            Op::Le if wide(0) || wide(1) => V::B(zi_cmp(Cmp::Le, iv(0), iv(1))),
            Op::Gt if wide(0) || wide(1) => V::B(zi_cmp(Cmp::Gt, iv(0), iv(1))),
            Op::Ge if wide(0) || wide(1) => V::B(zi_cmp(Cmp::Ge, iv(0), iv(1))),
            Op::Lt => V::B(zn_lt(n(0), n(1))),
            Op::Le => V::B(zn_le(n(0), n(1))),
            Op::Gt => V::B(zn_gt(n(0), n(1))),
            Op::Ge => V::B(zn_ge(n(0), n(1))),
            Op::Eq if !dom_bool(0) && (wide(0) || wide(1)) => V::B(zi_eq(iv(0), iv(1))),
            Op::Eq => {
                if dom_bool(0) {
                    V::B(zb_eq(b(0), b(1)))
                } else {
                    V::B(zn_eq(n(0), n(1)))
                }
            }
            Op::Not => V::B(zb_not(b(0))),
            Op::And => V::B(zb_and(b(0), b(1))),
            Op::Or => V::B(zb_or(b(0), b(1))),
            Op::Known => {
                // Known(Flr(interval)) reads the interval (zi_flr_ok).
                let inner = node.args[0] as usize;
                let flr_over_ival = matches!(g.get(inner as u32).op, Op::Flr)
                    && matches!(vals[g.get(inner as u32).args[0] as usize], V::I(_));
                if flr_over_ival {
                    let src = g.get(inner as u32).args[0] as usize;
                    if let V::I(z) = vals[src] {
                        let fl = zn_flr(z.lo);
                        let fh = zn_flr(z.hi);
                        let val = zn_eq(fl, fh).val;
                        V::B(ZB { val, known: ALL })
                    } else {
                        unreachable!()
                    }
                } else {
                    let (val, _) = match vals[inner] {
                        V::B(z) => (z.known, ()),
                        V::N(_) => (ALL, ()),
                        V::I(z) => (zn_eq(z.lo, z.hi).val, ()),
                    };
                    V::B(ZB { val, known: ALL })
                }
            }
            Op::Lo => V::N(iv(0).lo),
            Op::Hi => V::N(iv(0).hi),
            Op::Sel => {
                let c = b(0);
                match vals[node.args[1] as usize] {
                    V::B(_) => V::B(zsel_b(c, b(1), b(2))),
                    V::I(_) => V::I(zsel_i(c, iv(1), iv(2))),
                    _ => V::N(zsel_n(c, n(1), n(2))),
                }
            }
            Op::Span => V::I(ZI { lo: iv(0).lo, hi: iv(1).hi }),
            Op::Frag(c) => V::I(zi_fork_flr(iv(0), *c as usize).0),
            Op::IntFrag(c) => V::N(zi_fork_flr(iv(0), *c as usize).0.lo),
            Op::FragOk(c) => {
                let (_, ok) = zi_fork_flr(iv(0), *c as usize);
                V::B(ZB { val: ok, known: ALL })
            }
            Op::SplitOk(ways) => V::B(zi_span_ok(iv(0), *ways)),
            // Read off the operation's operands, as the codegen does.
            Op::NoWrap => {
                let x = g.get(node.args[0]);
                let ivx = |k: usize| match vals[x.args[k] as usize] {
                    V::I(z) => z,
                    V::N(z) => ZI { lo: z, hi: z },
                    _ => panic!("node {id}: NoWrap of a boolean"),
                };
                let wraps = match (&x.op, vals[node.args[0] as usize]) {
                    (Op::Add, V::I(_)) => zi_add_wraps(ivx(0), ivx(1)),
                    (Op::Sub, V::I(_)) => zi_sub_wraps(ivx(0), ivx(1)),
                    (Op::Neg, V::I(_)) => zi_neg_wraps(ivx(0)),
                    _ => 0,
                };
                V::B(ZB { val: !wraps, known: ALL })
            }
            Op::Mget => {
                let (cart, _) = room.expect("Mget needs a room");
                V::N(zn_mget(cart, n(0), n(1)))
            }
            Op::TileFlagAt => {
                let (cart, cache) = room.expect("TileFlagAt needs a room");
                let w = n(2).lane(0);
                let h = n(3).lane(0);
                let flag = n(4).lane(0);
                V::B(zn_tile_flag_at(cache, cart, n(0), n(1), w, h, flag))
            }
            // The operand unchanged, as the codegen.
            Op::Restrict(..) => vals[node.args[0] as usize],
            other => panic!("oracle: unsupported op {other:?}"),
        };
        vals.push(v);
    }
    vals
}

// ---- typed-root coverage (value + bool + interval layers) ----

use crate::transpile::asm::RootKind;

/// Read the raw output buffer, `out_bytes` long (`Compiled::out_bytes`).
fn run_asm_raw(
    loaded: &crate::transpile::asm::Loaded,
    input: &[u8],
    out_bytes: u32,
    ctx: *const std::os::raw::c_void,
) -> Vec<u8> {
    let mut out = vec![0u8; out_bytes as usize];
    unsafe { (loaded.func)(input.as_ptr(), out.as_mut_ptr(), ctx) };
    out
}

fn zn_bytes(z: ZN) -> [u8; 64] {
    let a = z.to_array();
    let mut o = [0u8; 64];
    for i in 0..16 {
        o[i * 4..i * 4 + 4].copy_from_slice(&(a[i].as_raw_u32()).to_le_bytes());
    }
    o
}

/// Assemble `g`'s roots, run the kernel on 8 random input batches (`cols`)
/// and assert every root bit-exact against the reference evaluator (a bool's
/// `val` on its known lanes only, where it is defined).
fn check(
    g: &Graph,
    roots: &[NodeId],
    tag: &str,
    rng: &mut Lcg,
    cols: fn(&[u32], &mut Lcg) -> HashMap<u32, [i32; 16]>,
    ctx: *const std::os::raw::c_void,
    room: Option<(&CartData, &CollisionCache)>,
) {
    let (compiled, loaded) = compile_and_load(g, roots, tag).expect("compile+load");
    for _ in 0..8 {
        let cols = cols(&compiled.input_cells, rng);
        let input = pack_inputs(&compiled.input_cells, &cols);
        let out = run_asm_raw(&loaded, &input, compiled.out_bytes, ctx);
        let vals = eval_nodes(g, &cols, room);
        for (ri, r) in roots.iter().enumerate() {
            let base = compiled.root_offsets[ri] as usize;
            match (compiled.root_kinds[ri], vals[*r as usize]) {
                (RootKind::Num, V::N(z)) => {
                    assert_eq!(&out[base..base + 64], &zn_bytes(z), "{tag}: num root {ri}");
                }
                (RootKind::Ival, V::I(z)) => {
                    assert_eq!(&out[base..base + 64], &zn_bytes(z.lo), "{tag}: ival lo {ri}");
                    assert_eq!(&out[base + 64..base + 128], &zn_bytes(z.hi), "{tag}: ival hi {ri}");
                }
                (RootKind::Bool, V::B(z)) => {
                    let val = u16::from_le_bytes([out[base], out[base + 1]]);
                    let known = u16::from_le_bytes([out[base + 2], out[base + 3]]);
                    assert_eq!(known, z.known, "{tag}: bool known root {ri}");
                    assert_eq!(val & known, z.val & known, "{tag}: bool val root {ri}");
                }
                (k, _) => panic!("{tag}: root {ri} kind {k:?} vs oracle type mismatch"),
            }
        }
    }
}

/// A graph exercising comparisons, the tri-state bool algebra, Known,
/// selects (num and bool arms), and ConstBool - roots of Num and Bool type.
fn build_value_graph(cells: &[u32], _rng: &mut Lcg) -> (Graph, Vec<NodeId>) {
    let mut g = Graph::new();
    let cs: Vec<NodeId> = cells.iter().map(|c| g.leaf(Op::Cell(*c))).collect();
    let zero = g.leaf(Op::Const(0, 0));
    let mut roots = Vec::new();
    let mut prev_bool = g.leaf(Op::ConstBool(true));
    for w in cs.windows(2) {
        let (x, y) = (w[0], w[1]);
        let lt = g.add(Op::Lt, vec![x, y]);
        let ge = g.add(Op::Ge, vec![x, zero]);
        let eq = g.add(Op::Eq, vec![x, y]);
        let a = g.add(Op::And, vec![lt, ge]);
        let o = g.add(Op::Or, vec![a, prev_bool]);
        let n = g.add(Op::Not, vec![eq]);
        let both = g.add(Op::And, vec![o, n]);
        let seln = g.add(Op::Sel, vec![both, x, y]);
        let beq = g.add(Op::Eq, vec![both, prev_bool]);
        let selb = g.add(Op::Sel, vec![lt, both, beq]);
        let kb = g.add(Op::Known, vec![selb]);
        roots.push(seln);
        roots.push(both);
        roots.push(beq);
        roots.push(kb);
        prev_bool = selb;
    }
    roots.push(prev_bool);
    (g, roots)
}

/// Small bounded columns (raw ~ +-8.0 fixed) so interval add/sub cannot
/// overflow (the `zi_*` primitives panic on overflow).
fn random_small_columns(cells: &[u32], rng: &mut Lcg) -> HashMap<u32, [i32; 16]> {
    cells
        .iter()
        .map(|c| (*c, std::array::from_fn::<i32, 16, _>(|_| (rng.i32() % 0x8_0000) - 0x4_0000)))
        .collect()
}

/// Build intervals (via Span and a wide Const) and exercise the interval
/// ops: zi_add/sub/min/max/neg/abs, zi_flr + Known(Flr), zi_cmp (all four
/// orders), Frag/FragOk/SplitOk, IntFrag, and Sel over intervals.
fn build_interval_graph(cells: &[u32]) -> (Graph, Vec<NodeId>) {
    let mut g = Graph::new();
    let two = g.leaf(Op::Const(ONE_FIXED2, ONE_FIXED2)); // +2.0
    let band = g.leaf(Op::Const(-0x8000, 0x8000)); // a widened +-0.5 literal
    let mut roots = Vec::new();
    for c in cells.iter() {
        let base = g.leaf(Op::Cell(*c));
        let hi = g.add(Op::Add, vec![base, two]);
        let ivl = g.add(Op::Span, vec![base, hi]); // [base, base+2]
        let shifted = g.add(Op::Add, vec![ivl, band]); // interval + interval
        let neg = g.add(Op::Neg, vec![shifted]);
        // their no-wrap premises, which hold on these bounded inputs
        let nw_add = g.add(Op::NoWrap, vec![shifted]);
        let nw_neg = g.add(Op::NoWrap, vec![neg]);
        let ab = g.add(Op::Abs, vec![neg]);
        let mn = g.add(Op::Min, vec![ab, ivl]);
        let mx = g.add(Op::Max, vec![mn, shifted]);
        let diff = g.add(Op::Sub, vec![mx, ivl]);
        let nw_sub = g.add(Op::NoWrap, vec![diff]);
        // floor + its premise
        let fl = g.add(Op::Flr, vec![mx]);
        let flok = g.add(Op::Known, vec![fl]);
        // interval comparisons
        let lt = g.add(Op::Lt, vec![ivl, shifted]);
        let ge = g.add(Op::Ge, vec![mx, ivl]);
        // interval equality: interval vs interval, interval vs number
        let eqi = g.add(Op::Eq, vec![ivl, shifted]);
        let eqn = g.add(Op::Eq, vec![mx, base]);
        // fork, at arity 2 and 3
        let spanok = g.add(Op::SplitOk(2), vec![mx]);
        let spanok3 = g.add(Op::SplitOk(3), vec![mx]);
        let frag0 = g.add(Op::Frag(0), vec![mx]);
        let ok0 = g.add(Op::FragOk(0), vec![mx]);
        let frag1 = g.add(Op::Frag(1), vec![mx]);
        let ok1 = g.add(Op::FragOk(1), vec![mx]);
        let frag2 = g.add(Op::Frag(2), vec![mx]);
        let ok2 = g.add(Op::FragOk(2), vec![mx]);
        // the exact-number fragments of an interval of whole numbers
        let ifrag0 = g.add(Op::IntFrag(0), vec![mx]);
        let ifrag1 = g.add(Op::IntFrag(1), vec![mx]);
        // select an interval on a decided bool
        let seli = g.add(Op::Sel, vec![spanok, frag0, frag1]);
        // a spread of typed roots
        roots.push(mx);
        roots.push(fl);
        roots.push(flok);
        roots.push(lt);
        roots.push(ge);
        roots.push(eqi);
        roots.push(eqn);
        roots.push(spanok);
        roots.push(spanok3);
        roots.push(ok0);
        roots.push(ok1);
        roots.push(frag2);
        roots.push(ok2);
        roots.push(ifrag0);
        roots.push(ifrag1);
        roots.push(seli);
        roots.push(diff);
        roots.push(nw_add);
        roots.push(nw_sub);
        roots.push(nw_neg);
    }
    (g, roots)
}

#[test]
fn asm_interval_layer_matches_primitives() {
    let mut rng = Lcg(0x1234_9999);
    for k in [2usize, 4, 7] {
        let cells: Vec<u32> = (0..k as u32).map(|i| i * 3 + 2).collect();
        let (g, roots) = build_interval_graph(&cells);
        check(&g, &roots, &format!("iv{k}"), &mut rng, random_small_columns, std::ptr::null(), None);
    }
}

/// Div / Rem / Sin, emitted as call-outs through `AsmCtx` (no cart needed;
/// `env` is null). Divisor is a nonzero constant so `zn_div/zn_rem` are
/// defined on every lane.
#[test]
fn asm_callout_div_rem_sin_matches_primitives() {
    let mut rng = Lcg(0xca11_0075);
    let ctx = crate::transpile::asm::AsmCtx::new(std::ptr::null());
    let mut g = Graph::new();
    let three = g.leaf(Op::Const(0x0003_0000, 0x0003_0000));
    let cells: Vec<u32> = (0..6u32).map(|i| i + 1).collect();
    let cs: Vec<NodeId> = cells.iter().map(|c| g.leaf(Op::Cell(*c))).collect();
    let mut roots = Vec::new();
    for &c in cs.iter() {
        let d = g.add(Op::Div, vec![c, three]);
        let r = g.add(Op::Rem, vec![c, three]);
        let sn = g.add(Op::Sin, vec![c]);
        // A call-out result feeding arithmetic.
        let d2 = g.add(Op::Add, vec![d, r]);
        roots.push(d);
        roots.push(r);
        roots.push(sn);
        roots.push(d2);
    }
    check(&g, &roots, "callout", &mut rng, random_small_columns, &ctx as *const _ as *const std::os::raw::c_void, None);
}

/// mget + tile_flag_at, emitted as call-outs into the real collision cache
/// (a cart-backed `AsmCtx`). Coordinates are floored and clamped into range
/// so `zn_mget` / `zn_tile_flag_at` are defined on every lane.
#[test]
fn asm_callout_collision_matches_primitives() {
    let cart = CartData::load("cart").expect("cart");
    let cache = CollisionCache::new(&cart, 1, 0).expect("cache");
    let env = crate::transpile::asm::CollisionEnv { cart: &cart, cache: &cache };
    let ctx = crate::transpile::asm::AsmCtx::new(&env as *const _ as *const std::os::raw::c_void);

    let mut g = Graph::new();
    let clamp = |g: &mut Graph, v: NodeId, lo: i32, hi: i32| -> NodeId {
        let fl = g.add(Op::Flr, vec![v]);
        let lo = g.leaf(Op::Const(lo << 16, lo << 16));
        let hi = g.leaf(Op::Const(hi << 16, hi << 16));
        let mx = g.add(Op::Max, vec![fl, lo]);
        g.add(Op::Min, vec![mx, hi])
    };
    let w8 = g.leaf(Op::Const(8 << 16, 8 << 16));
    let flag0 = g.leaf(Op::Const(0, 0));
    let cells: Vec<u32> = (0..8u32).map(|i| i + 1).collect();
    let cs: Vec<NodeId> = cells.iter().map(|c| g.leaf(Op::Cell(*c))).collect();
    let mut roots = Vec::new();
    for w in cs.windows(2) {
        let xt = clamp(&mut g, w[0], 0, 127);
        let yt = clamp(&mut g, w[1], 0, 63);
        let mg = g.add(Op::Mget, vec![xt, yt]);
        let xp = clamp(&mut g, w[0], 0, 118);
        let yp = clamp(&mut g, w[1], 0, 118);
        let tf = g.add(Op::TileFlagAt, vec![xp, yp, w8, w8, flag0]);
        roots.push(mg);
        roots.push(tf);
        // a call result feeding arithmetic
        roots.push(g.add(Op::Add, vec![mg, w8]));
    }

    let mut rng = Lcg(0xc0111_5107);
    check(&g, &roots, "collision", &mut rng, random_columns, &ctx as *const _ as *const std::os::raw::c_void, Some((&cart, &cache)));
}

/// Ice (flag 4): the lane primitive's answer through the precomputed
/// player-hitbox ice map is the tile scan's at every position of an ice
/// room ((3,1)), and true somewhere.
#[test]
fn tile_flag_ice_map_matches_the_scan() {
    let cart = CartData::load("cart").expect("cart");
    let cache = CollisionCache::new(&cart, 3, 1).expect("cache");
    let p = |v: i16| crate::pico8_num::Pico8Num::from_i16(v);
    let scan = |x: i16, y: i16, w: i16, h: i16| -> bool {
        (y.max(0) / 8..=((y + h - 1) / 8).min(15)).any(|ty| {
            (x.max(0) / 8..=((x + w - 1) / 8).min(15)).any(|tx| {
                let t = cart.mget(p(3 * 16 + tx), p(16 + ty)).expect("mget");
                cart.fget(p(t as i16), p(4)).expect("fget")
            })
        })
    };
    let mut hits = 0;
    for y in -8..136i16 {
        for xs in (-8..136i16).collect::<Vec<_>>().chunks(16) {
            let xv: Vec<_> = (0..16).map(|i| p(*xs.get(i).unwrap_or(&xs[0]) + 1)).collect();
            let x = ZN::from_array(xv.try_into().unwrap());
            let yz = ZN::from_array([p(y + 3); 16]);
            let zb = zn_tile_flag_at(&cache, &cart, x, yz, p(6), p(5), p(4));
            for (i, &xi) in xs.iter().enumerate() {
                let want = scan(xi + 1, y + 3, 6, 5);
                assert_eq!(zb.val >> i & 1 == 1, want, "ice at ({}, {})", xi + 1, y + 3);
                hits += want as u32;
            }
        }
    }
    assert!(hits > 0, "room (3,1) has ice");
}

#[test]
fn asm_value_and_bool_layer_matches_primitives() {
    let mut rng = Lcg(0x51ce_d00d);
    for k in [3usize, 5, 9, 16] {
        let cells: Vec<u32> = (0..k as u32).map(|i| i * 2 + 3).collect();
        let (g, roots) = build_value_graph(&cells, &mut rng);
        check(&g, &roots, &format!("val{k}"), &mut rng, random_columns, std::ptr::null(), None);
    }
}

/// A bool INPUT cell (`CellRepr::Bool`): the input buffer carries a 16-bit
/// `val` mask (known implicitly all-ones), the codegen expands it to a
/// per-lane vector mask, and it flows through `Not`/`Sel` to Num and Bool
/// roots. Bit-exact against the `ZB`/`ZN` primitives.
#[test]
fn asm_bool_input_matches_primitives() {
    use crate::transpile::asm::{compile_and_load_reprs, CellRepr, RootKind};
    let mut g = Graph::new();
    let cnum = g.leaf(Op::Cell(0)); // num
    let cbool = g.leaf(Op::Cell(1)); // bool
    let cnum2 = g.leaf(Op::Cell(2)); // num
    let notb = g.add(Op::Not, vec![cbool]);
    let sel = g.add(Op::Sel, vec![cbool, cnum, cnum2]); // Num, gated by the bool input
    let seln = g.add(Op::Sel, vec![notb, cnum2, cnum]); // Num, gated by !bool
    let roots = vec![sel, seln, notb, cbool];
    let mut reprs = HashMap::new();
    reprs.insert(1u32, CellRepr::Bool);
    let (compiled, loaded) =
        compile_and_load_reprs(&g, &roots, "boolin", &reprs).expect("compile+load");
    assert_eq!(
        compiled.root_kinds,
        vec![RootKind::Num, RootKind::Num, RootKind::Bool, RootKind::Bool]
    );
    assert_eq!(compiled.input_cells, vec![0, 1, 2]);

    let mut rng = Lcg(0x600d_cafe);
    for trial in 0..8 {
        let mask = (rng.next_u64() & 0xFFFF) as u16;
        let num0: [i32; 16] = std::array::from_fn(|_| rng.i32());
        let num2: [i32; 16] = std::array::from_fn(|_| rng.i32());
        // cell0 @0 (num ZN); cell1 @64 (u16 mask in the first 2 bytes);
        // cell2 @128 (num ZN).
        let mut input = vec![0u8; 3 * 64];
        for (l, v) in num0.iter().enumerate() {
            input[l * 4..l * 4 + 4].copy_from_slice(&v.to_le_bytes());
        }
        input[64..66].copy_from_slice(&mask.to_le_bytes());
        for (l, v) in num2.iter().enumerate() {
            input[128 + l * 4..128 + l * 4 + 4].copy_from_slice(&v.to_le_bytes());
        }
        let out = run_asm_raw(&loaded, &input, compiled.out_bytes,std::ptr::null());

        let zb = ZB { val: mask, known: ALL };
        let zn0 = ZN::from_array(std::array::from_fn(|i| P8::from_raw(num0[i])));
        let zn2 = ZN::from_array(std::array::from_fn(|i| P8::from_raw(num2[i])));
        let e_sel = zsel_n(zb, zn0, zn2);
        let e_seln = zsel_n(zb_not(zb), zn2, zn0);
        let e_not = zb_not(zb);

        let o = |ri: usize| compiled.root_offsets[ri] as usize;
        assert_eq!(&out[o(0)..o(0) + 64], &zn_bytes(e_sel), "trial {trial}: sel");
        assert_eq!(&out[o(1)..o(1) + 64], &zn_bytes(e_seln), "trial {trial}: seln");
        assert_eq!(
            u16::from_le_bytes([out[o(2)], out[o(2) + 1]]),
            e_not.val,
            "trial {trial}: not val"
        );
        assert_eq!(
            u16::from_le_bytes([out[o(2) + 2], out[o(2) + 3]]),
            e_not.known,
            "trial {trial}: not known"
        );
        assert_eq!(
            u16::from_le_bytes([out[o(3)], out[o(3) + 1]]),
            mask,
            "trial {trial}: passthrough val"
        );
        assert_eq!(
            u16::from_le_bytes([out[o(3) + 2], out[o(3) + 3]]),
            ALL,
            "trial {trial}: passthrough known"
        );
    }
}

/// A maybe-unknown bool INPUT cell (`CellRepr::UBool`, a near level's floor
/// `collideable`): `val` at +0 and `known` at +2 of its slot, both loaded, so
/// `Known` reads the lanes' own known mask and the cell passes through to a
/// bool root with it.
#[test]
fn asm_ubool_input_carries_its_known_mask() {
    use crate::transpile::asm::{compile_and_load_reprs, CellRepr, RootKind};
    let mut g = Graph::new();
    let cb = g.leaf(Op::Cell(0));
    let known = g.add(Op::Known, vec![cb]);
    let roots = vec![known, cb];
    let mut reprs = HashMap::new();
    reprs.insert(0u32, CellRepr::UBool);
    let (compiled, loaded) = compile_and_load_reprs(&g, &roots, "uboolin", &reprs).expect("compile+load");
    assert_eq!(compiled.root_kinds, vec![RootKind::Bool, RootKind::Bool]);
    let mut rng = Lcg(0x0b00_1ea4);
    for trial in 0..8 {
        let (val, kn) = ((rng.next_u64() & 0xFFFF) as u16, (rng.next_u64() & 0xFFFF) as u16);
        let mut input = vec![0u8; 64];
        input[0..2].copy_from_slice(&val.to_le_bytes());
        input[2..4].copy_from_slice(&kn.to_le_bytes());
        let out = run_asm_raw(&loaded, &input, compiled.out_bytes, std::ptr::null());
        let o = |ri: usize| compiled.root_offsets[ri] as usize;
        let word = |at: usize| u16::from_le_bytes([out[at], out[at + 1]]);
        assert_eq!((word(o(0)), word(o(0) + 2)), (kn, ALL), "trial {trial}: Known");
        assert_eq!((word(o(1)) & kn, word(o(1) + 2)), (val & kn, kn), "trial {trial}: passthrough");
    }
}

/// An interval (`ZI`) INPUT cell (`CellRepr::Ival`): two `ZN` planes (lo at
/// +0, hi at +64 of a 128-byte slot) flowing through `Add`/`Flr` to Ival
/// and Num roots, packed via `input_offsets` and compared bit-exact against
/// `zi_add`/`zi_flr`.
#[test]
fn asm_ival_input_matches_primitives() {
    use crate::transpile::asm::{compile_and_load_reprs, CellRepr, RootKind};
    let mut g = Graph::new();
    let civ = g.leaf(Op::Cell(0)); // ival
    let cnum = g.leaf(Op::Cell(1)); // num
    let sum = g.add(Op::Add, vec![civ, cnum]); // Ival
    let fl = g.add(Op::Flr, vec![sum]); // Num
    let roots = vec![sum, fl, civ];
    let mut reprs = HashMap::new();
    reprs.insert(0u32, CellRepr::Ival);
    let (compiled, loaded) =
        compile_and_load_reprs(&g, &roots, "ivalin", &reprs).expect("compile+load");
    assert_eq!(
        compiled.root_kinds,
        vec![RootKind::Ival, RootKind::Num, RootKind::Ival]
    );
    // cell0 ival (128 bytes) then cell1 num (64 bytes).
    assert_eq!(compiled.input_offsets, vec![0, 128]);
    assert_eq!(compiled.input_bytes, 192);

    let mut rng = Lcg(0xda7a_1a11);
    for trial in 0..8 {
        // Bounded so zi_add cannot overflow (the kernel panics on overflow).
        let bound = |r: &mut Lcg| ((r.next_u64() % 0x10_0000) as i32) - 0x8_0000;
        let lo: [i32; 16] = std::array::from_fn(|_| bound(&mut rng));
        let hi: [i32; 16] = std::array::from_fn(|i| lo[i] + (rng.next_u64() % 0x2_0000) as i32);
        let num: [i32; 16] = std::array::from_fn(|_| bound(&mut rng));

        let mut input = vec![0u8; compiled.input_bytes as usize];
        for (l, v) in lo.iter().enumerate() {
            input[l * 4..l * 4 + 4].copy_from_slice(&v.to_le_bytes());
        }
        for (l, v) in hi.iter().enumerate() {
            input[64 + l * 4..64 + l * 4 + 4].copy_from_slice(&v.to_le_bytes());
        }
        for (l, v) in num.iter().enumerate() {
            input[128 + l * 4..128 + l * 4 + 4].copy_from_slice(&v.to_le_bytes());
        }
        let out = run_asm_raw(&loaded, &input, compiled.out_bytes,std::ptr::null());

        let ziv = ZI {
            lo: ZN::from_array(std::array::from_fn(|i| P8::from_raw(lo[i]))),
            hi: ZN::from_array(std::array::from_fn(|i| P8::from_raw(hi[i]))),
        };
        let znum = ZN::from_array(std::array::from_fn(|i| P8::from_raw(num[i])));
        let e_sum = zi_add(ziv, ZI { lo: znum, hi: znum });
        let e_fl = zi_flr(e_sum);

        let o = |ri: usize| compiled.root_offsets[ri] as usize;
        assert_eq!(&out[o(0)..o(0) + 64], &zn_bytes(e_sum.lo), "trial {trial}: sum lo");
        assert_eq!(&out[o(0) + 64..o(0) + 128], &zn_bytes(e_sum.hi), "trial {trial}: sum hi");
        assert_eq!(&out[o(1)..o(1) + 64], &zn_bytes(e_fl), "trial {trial}: flr");
        assert_eq!(&out[o(2)..o(2) + 64], &zn_bytes(ziv.lo), "trial {trial}: passthrough lo");
        assert_eq!(&out[o(2) + 64..o(2) + 128], &zn_bytes(ziv.hi), "trial {trial}: passthrough hi");
    }
}

/// Interval `+`, `-` and negation whose endpoints OVERFLOW the 16.16 range
/// are an ERROR in that lane, never a value: the kernel's `NoWrap` premise
/// is false exactly where the primitives' wrap guards (`zi_add_wraps`,
/// `zi_sub_wraps`, `zi_neg_wraps`) fire, so a body reading such an
/// operation declines there (`trace::error`). Every other lane is bit-exact.
/// A silently wrapped endpoint inverts the interval.
#[test]
fn asm_interval_overflow_declines() {
    use crate::transpile::asm::{compile_and_load_reprs, CellRepr};
    let mut g = Graph::new();
    let civ = g.leaf(Op::Cell(0)); // ival
    let cnum = g.leaf(Op::Cell(1)); // num
    let add = g.add(Op::Add, vec![civ, cnum]);
    let sub = g.add(Op::Sub, vec![civ, cnum]);
    let neg = g.add(Op::Neg, vec![civ]);
    let ok: Vec<NodeId> = [add, sub, neg].iter().map(|&x| g.add(Op::NoWrap, vec![x])).collect();
    let roots = vec![add, sub, neg, ok[0], ok[1], ok[2]];
    let mut reprs = HashMap::new();
    reprs.insert(0u32, CellRepr::Ival);
    let (compiled, loaded) = compile_and_load_reprs(&g, &roots, "ivalovf", &reprs).expect("compile+load");
    let one = 0x1_0000;
    // Per lane (lo, hi, num): the whole range plus/minus one, a top end that
    // overflows on +1, a bottom end on -1, MIN negated, and lanes right up to
    // the ends without overflowing.
    let lanes: [(i32, i32, i32); 16] = [
        (i32::MIN, i32::MAX, one),
        (i32::MIN, i32::MAX, -one),
        (i32::MAX - 5, i32::MAX, one),
        (i32::MIN, i32::MIN + 5, one),
        (i32::MIN, 0, 0),
        (-one, one, one),
        (0, 3 * one, -one),
        (i32::MIN + one, i32::MAX - one, one),
        (i32::MIN + one, i32::MAX - one, -one),
        (5, 9, 2),
        (i32::MAX, i32::MAX, 1),
        (i32::MIN, i32::MIN, -1),
        (-7 * one, -6 * one, 4 * one),
        (100, 200, 300),
        (i32::MIN + 1, i32::MIN + 1, 0),
        (0, 0, 0),
    ];
    let mut input = vec![0u8; compiled.input_bytes as usize];
    for (l, &(lo, hi, num)) in lanes.iter().enumerate() {
        input[l * 4..l * 4 + 4].copy_from_slice(&lo.to_le_bytes());
        input[64 + l * 4..64 + l * 4 + 4].copy_from_slice(&hi.to_le_bytes());
        input[128 + l * 4..128 + l * 4 + 4].copy_from_slice(&num.to_le_bytes());
    }
    let out = run_asm_raw(&loaded, &input, compiled.out_bytes, std::ptr::null());
    let ziv = ZI {
        lo: ZN::from_array(std::array::from_fn(|i| P8::from_raw(lanes[i].0))),
        hi: ZN::from_array(std::array::from_fn(|i| P8::from_raw(lanes[i].1))),
    };
    let znum = ZN::from_array(std::array::from_fn(|i| P8::from_raw(lanes[i].2)));
    let zb = ZI { lo: znum, hi: znum };
    let wraps = [zi_add_wraps(ziv, zb), zi_sub_wraps(ziv, zb), zi_neg_wraps(ziv)];
    // What the test is about: exactly these lanes overflow.
    assert_eq!(wraps[0], 0x0c07, "add: the whole range +- 1, a top end past MAX, MAX + eps, MIN - eps");
    assert_eq!(wraps[1], 0x000b, "sub: the whole range -+ 1, a bottom end past MIN");
    assert_eq!(wraps[2], 0x081b, "neg: every lane with an endpoint at MIN");
    let word = |o: usize| u16::from_le_bytes([out[o], out[o + 1]]);
    let plane = |ri: usize, off: usize| -> [i32; 16] {
        let o = compiled.root_offsets[ri] as usize + off;
        std::array::from_fn(|l| i32::from_le_bytes(out[o + l * 4..o + l * 4 + 4].try_into().unwrap()))
    };
    for (k, name) in ["add", "sub", "neg"].into_iter().enumerate() {
        let o = compiled.root_offsets[3 + k] as usize;
        assert_eq!(word(o + 2), 0xffff, "{name}: the premise is decided on every lane");
        assert_eq!(word(o), !wraps[k], "{name}: NoWrap false exactly where the primitives' guard fires");
        // The other lanes are bit-exact with the primitives (run on those lanes
        // alone: the primitives panic on the rest).
        let (lo, hi) = (plane(k, 0), plane(k, 64));
        for l in (0..16).filter(|l| wraps[k] >> l & 1 == 0) {
            let pick = |z: ZN| ZN::from_array([z.to_array()[l]; 16]);
            let (a, b) = (ZI { lo: pick(ziv.lo), hi: pick(ziv.hi) }, ZI { lo: pick(znum), hi: pick(znum) });
            let e = match k {
                0 => zi_add(a, b),
                1 => zi_sub(a, b),
                _ => zi_neg(a),
            };
            let want = (e.lo.to_array()[0].as_raw_u32() as i32, e.hi.to_array()[0].as_raw_u32() as i32);
            assert_eq!((lo[l], hi[l]), want, "{name} lane {l} {:?}: assembled vs primitive", lanes[l]);
        }
    }
}

/// The tri-state layer with DECIDED planes: a maybe-unknown input, a bool
/// input, a comparison (known everywhere), the two literals and an undecided
/// constant, combined pairwise by `And`/`Or`/`Eq`/`Not` and as `Sel`
/// conditions and arms. The codegen folds the decided planes by bit identities
/// (`codegen::Lower::m_and` ...); every root must still equal the `zb_*`
/// primitives on EVERY bit of both planes, not only on the known lanes.
#[test]
fn asm_bool_layer_with_decided_planes_is_bit_exact() {
    use crate::transpile::asm::{compile_and_load_reprs, CellRepr};
    let mut g = Graph::new();
    let u = g.leaf(Op::Cell(0));
    let b = g.leaf(Op::Cell(1));
    let (x, y) = (g.leaf(Op::Cell(2)), g.leaf(Op::Cell(3)));
    let c = g.add(Op::Lt, vec![x, y]);
    let t = g.leaf(Op::ConstBool(true));
    let f = g.leaf(Op::ConstBool(false));
    let k = g.leaf(Op::UnknownBool(0));
    let leaves = [u, b, c, t, f, k];
    let mut roots = Vec::new();
    for &p in &leaves {
        roots.push(g.add(Op::Not, vec![p]));
        roots.push(g.add(Op::Sel, vec![p, x, y]));
        for &q in &leaves {
            for op in [Op::And, Op::Or, Op::Eq] {
                roots.push(g.add(op, vec![p, q]));
            }
            for &r in &leaves {
                roots.push(g.add(Op::Sel, vec![p, q, r]));
            }
        }
    }
    let reprs = HashMap::from([(0u32, CellRepr::UBool), (1u32, CellRepr::Bool)]);
    let (compiled, loaded) = compile_and_load_reprs(&g, &roots, "decided", &reprs).expect("compile+load");
    let mut rng = Lcg(0xdec1_dedd);
    for trial in 0..16 {
        let (uv, uk, bv) = ((rng.next_u64() & 0xFFFF) as u16, (rng.next_u64() & 0xFFFF) as u16, (rng.next_u64() & 0xFFFF) as u16);
        let (xs, ys): ([i32; 16], [i32; 16]) = (std::array::from_fn(|_| rng.i32() % 4), std::array::from_fn(|_| rng.i32() % 4));
        let mut input = vec![0u8; compiled.input_bytes as usize];
        for (ci, cell) in compiled.input_cells.iter().enumerate() {
            let at = compiled.input_offsets[ci] as usize;
            match cell {
                0 => {
                    input[at..at + 2].copy_from_slice(&uv.to_le_bytes());
                    input[at + 2..at + 4].copy_from_slice(&uk.to_le_bytes());
                }
                1 => input[at..at + 2].copy_from_slice(&bv.to_le_bytes()),
                _ => {
                    let col = if *cell == 2 { &xs } else { &ys };
                    for (l, v) in col.iter().enumerate() {
                        input[at + l * 4..at + l * 4 + 4].copy_from_slice(&v.to_le_bytes());
                    }
                }
            }
        }
        let out = run_asm_raw(&loaded, &input, compiled.out_bytes, std::ptr::null());
        let zn = |a: &[i32; 16]| ZN::from_array(std::array::from_fn(|i| P8::from_raw(a[i])));
        let (zx, zy) = (zn(&xs), zn(&ys));
        // The oracle, per node: each leaf's planes as the codegen holds them.
        let mut vals: HashMap<NodeId, ZB> = HashMap::new();
        vals.insert(u, ZB { val: uv, known: uk });
        vals.insert(b, ZB { val: bv, known: ALL });
        vals.insert(c, zn_lt(zx, zy));
        vals.insert(t, ZB { val: ALL, known: ALL });
        vals.insert(f, ZB { val: 0, known: ALL });
        vals.insert(k, ZB { val: 0, known: 0 });
        for (ri, &r) in roots.iter().enumerate() {
            let node = g.get(r);
            let a = |i: usize| vals[&node.args[i]];
            let o = compiled.root_offsets[ri] as usize;
            let word = |at: usize| u16::from_le_bytes([out[at], out[at + 1]]);
            if node.op == Op::Sel && !matches!(g.get(node.args[1]).op, Op::Cell(2) | Op::Cell(3)) {
                let want = zsel_b(a(0), a(1), a(2));
                assert_eq!((word(o), word(o + 2)), (want.val, want.known), "trial {trial}: root {ri} {:?}", node);
                continue;
            }
            if node.op == Op::Sel {
                assert_eq!(&out[o..o + 64], &zn_bytes(zsel_n(a(0), zx, zy)), "trial {trial}: root {ri} numeric select");
                continue;
            }
            let want = match node.op {
                Op::Not => zb_not(a(0)),
                Op::And => zb_and(a(0), a(1)),
                Op::Or => zb_or(a(0), a(1)),
                _ => zb_eq(a(0), a(1)),
            };
            assert_eq!((word(o), word(o + 2)), (want.val, want.known), "trial {trial}: root {ri} {:?}", node);
        }
    }
}

/// `/ 2^s` and `% 2^m` by a literal are INLINE shifts and masks
/// (`codegen::Lower::div_pow2`, `pow2_modulus_mask`), not call-outs, and are
/// bit-exact against `Pico8Num`'s `/` (truncating, saturating) and `%`
/// (`rem_euclid`) over the whole i32 range: the edges (MIN, MAX, 0, +-1,
/// every +-2^k and its neighbours) and random raws, negatives included; for
/// the interval `/`, both endpoints.
#[test]
fn inline_div_rem_by_a_power_of_two_is_pico8s() {
    let mut g = Graph::new();
    let x = g.leaf(Op::Cell(0));
    let y = g.leaf(Op::Cell(1));
    let iv = g.add(Op::Span, vec![x, y]);
    let mut roots = Vec::new();
    let mut want: Vec<(u8, i32)> = Vec::new(); // (0 div, 1 rem, 2 interval div), raw divisor
    for s in 0..=14u32 {
        let d = 1i32 << (16 + s);
        let c = g.leaf(Op::Const(d, d));
        roots.push(g.add(Op::Div, vec![x, c]));
        want.push((0, d));
        roots.push(g.add(Op::Div, vec![iv, c]));
        want.push((2, d));
    }
    for m in 0..=30u32 {
        let d = 1i32 << m;
        let c = g.leaf(Op::Const(d, d));
        roots.push(g.add(Op::Rem, vec![x, c]));
        want.push((1, d));
    }
    let (compiled, loaded) = compile_and_load(&g, &roots, "pow2").expect("compile+load");
    // (`compile_and_load` drops the text once loaded.)
    let text = crate::transpile::asm::compile(&g, &roots, "pow2", &HashMap::new()).expect("compile").asm;
    assert!(!text.contains("call"), "a power-of-two divisor is inline");
    let mut edges: Vec<i32> = vec![i32::MIN, i32::MIN + 1, i32::MAX, i32::MAX - 1, 0, 1, -1];
    for k in 0..31 {
        for d in [-1i32, 0, 1] {
            edges.push((1i32 << k).wrapping_add(d));
            edges.push((-(1i64 << k) as i32).wrapping_add(d));
        }
    }
    let mut rng = Lcg(0x0d1f_2e3d);
    let mut batches: Vec<[i32; 16]> = edges.chunks(16).map(|c| std::array::from_fn(|i| c[i % c.len()])).collect();
    batches.extend((0..64).map(|_| std::array::from_fn(|_| rng.i32())));
    for xs in &batches {
        // The interval's high end: the low end plus a random non-negative span.
        let ys: [i32; 16] = std::array::from_fn(|i| xs[i].saturating_add((rng.i32() & 0x7fff_ffff) >> (rng.i32() & 31)));
        let cols = HashMap::from([(0u32, *xs), (1u32, ys)]);
        let input = pack_inputs(&compiled.input_cells, &cols);
        let out = run_asm_raw(&loaded, &input, compiled.out_bytes, std::ptr::null());
        for (ri, &(kind, d)) in want.iter().enumerate() {
            let o = compiled.root_offsets[ri] as usize;
            let lane = |half: usize, l: usize| i32::from_le_bytes(out[o + half * 64 + l * 4..o + half * 64 + l * 4 + 4].try_into().unwrap());
            for l in 0..16 {
                let (p, q) = (P8::from_raw(xs[l]), P8::from_raw(d));
                match kind {
                    0 => assert_eq!(lane(0, l), (p / q).as_raw_u32() as i32, "{:#x} / {:#x}", xs[l], d),
                    1 => assert_eq!(lane(0, l), (p % q).as_raw_u32() as i32, "{:#x} % {:#x}", xs[l], d),
                    _ => {
                        assert_eq!(lane(0, l), (p / q).as_raw_u32() as i32, "[{:#x}, ..] / {:#x}", xs[l], d);
                        assert_eq!(lane(1, l), (P8::from_raw(ys[l]) / q).as_raw_u32() as i32, "[.., {:#x}] / {:#x}", ys[l], d);
                    }
                }
            }
        }
    }
}

/// Only the registers live across a call-out are saved and restored: a
/// value computed before a call and read after it survives it, the kernel
/// saves fewer than all 32, and `ZERO` (read by a `Neg` after the call) is
/// zero again.
#[test]
fn a_call_out_preserves_what_is_live_across_it() {
    let mut g = Graph::new();
    let x = g.leaf(Op::Cell(0));
    let y = g.leaf(Op::Cell(1));
    let three = g.leaf(Op::Const(3 << 16, 3 << 16));
    let before = g.add(Op::Add, vec![x, y]); // live across the call
    let q = g.add(Op::Div, vec![before, three]); // a call-out, after `before`
    let n = g.add(Op::Neg, vec![q]); // reads ZERO after the call
    let sum = g.add(Op::Add, vec![before, n]);
    let roots = vec![sum, before];
    let (compiled, loaded) = compile_and_load(&g, &roots, "livecall").expect("compile+load");
    let text = crate::transpile::asm::compile(&g, &roots, "livecall", &HashMap::new()).expect("compile").asm;
    // Stores to the stack: the call's two arguments and the saves.
    let stores = text.lines().filter(|l| l.contains("vmovdqu64 %zmm") && l.contains("(%rsp)")).count();
    assert!(text.contains("call") && (3..32).contains(&stores), "{stores} stack stores around the call");
    let ctx = crate::transpile::asm::AsmCtx::new(std::ptr::null());
    let mut rng = Lcg(0x11fe_ca11);
    for _ in 0..16 {
        let cols = random_small_columns(&compiled.input_cells, &mut rng);
        let input = pack_inputs(&compiled.input_cells, &cols);
        let out = run_asm_raw(&loaded, &input, compiled.out_bytes, &ctx as *const _ as *const std::os::raw::c_void);
        for l in 0..16 {
            let (px, py) = (P8::from_raw(cols[&0][l]), P8::from_raw(cols[&1][l]));
            let want = [(px + py) + -((px + py) / P8::from_raw(3 << 16)), px + py];
            for (ri, w) in want.iter().enumerate() {
                let o = compiled.root_offsets[ri] as usize + l * 4;
                assert_eq!(i32::from_le_bytes(out[o..o + 4].try_into().unwrap()), w.as_raw_u32() as i32, "root {ri} lane {l}");
            }
        }
    }
}

/// Values live across a RUN of call-outs and read only after it stay in the
/// save area between the calls (`codegen::call_saves`): the kernel restores
/// fewer registers than it would around each call alone, and every value
/// survives. Eight call-outs on independent operands, four values held
/// across all of them.
#[test]
fn values_held_across_a_run_of_calls_survive_it() {
    let mut g = Graph::new();
    let cs: Vec<NodeId> = (0..8u32).map(|c| g.leaf(Op::Cell(c))).collect();
    let three = g.leaf(Op::Const(3 << 16, 3 << 16));
    let held: Vec<NodeId> = (0..4).map(|i| g.add(Op::Add, vec![cs[i], cs[i + 4]])).collect();
    // Each call reads one held value, so all four are defined first.
    let qs: Vec<NodeId> = (0..8).map(|i| {
        let arg = g.add(Op::Sub, vec![held[i % 4], cs[i]]);
        g.add(Op::Div, vec![arg, three])
    }).collect();
    let mut roots = held.clone();
    roots.extend(qs.iter().copied());
    let text = crate::transpile::asm::compile(&g, &roots, "callrun", &HashMap::new()).expect("compile").asm;
    let calls = text.matches("call *%rax").count();
    assert_eq!(calls, 8, "eight call-outs");
    let (compiled, loaded) = compile_and_load(&g, &roots, "callrun").expect("compile+load");
    let ctx = crate::transpile::asm::AsmCtx::new(std::ptr::null());
    let mut rng = Lcg(0x5a7e_0a11);
    for _ in 0..16 {
        let cols = random_small_columns(&compiled.input_cells, &mut rng);
        let input = pack_inputs(&compiled.input_cells, &cols);
        let out = run_asm_raw(&loaded, &input, compiled.out_bytes, &ctx as *const _ as *const std::os::raw::c_void);
        let vals = eval_nodes(&g, &cols, None);
        for (ri, r) in roots.iter().enumerate() {
            let V::N(z) = vals[*r as usize] else { panic!("numeric roots") };
            let o = compiled.root_offsets[ri] as usize;
            assert_eq!(&out[o..o + 64], &zn_bytes(z), "root {ri}");
        }
    }
}
