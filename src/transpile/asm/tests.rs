//! Correctness gate for the asm backend: emitted code vs the real
//! `celeste_engine::kernel` primitives, bit-exact on random inputs.

use std::collections::HashMap;

const ONE_FIXED2: i32 = 0x0002_0000; // +2.0 in 16.16

use celeste_engine::kernel::{
    zb_and, zb_eq, zb_not, zb_or, zi_abs, zi_add, zi_cmp, zi_flr, zi_fork_flr, zi_max, zi_min,
    zi_neg, zi_span_ok, zi_sub, zn_abs, zn_add, zn_eq, zn_flr, zn_ge, zn_gt, zn_le, zn_lt, zn_max,
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
    /// A row-key word, per lane.
    W([u64; 16]),
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
                        V::W(_) => panic!("Known of a word"),
                    };
                    V::B(ZB { val, known: ALL })
                }
            }
            Op::Sel => {
                let c = b(0);
                match vals[node.args[1] as usize] {
                    V::B(_) => V::B(zsel_b(c, b(1), b(2))),
                    V::I(_) => V::I(zsel_i(c, iv(1), iv(2))),
                    _ => V::N(zsel_n(c, n(1), n(2))),
                }
            }
            Op::Word(w) => V::W([*w; 16]),
            Op::AddW => {
                let (x, y) = match (vals[node.args[0] as usize], vals[node.args[1] as usize]) {
                    (V::W(x), V::W(y)) => (x, y),
                    _ => panic!("node {id}: AddW of non-words"),
                };
                V::W(std::array::from_fn(|l| x[l].wrapping_add(y[l])))
            }
            Op::CellMix(c, half) => {
                // The oracle is the boundary's own scalar `cell_mix` on the
                // lane's `AV`.
                use celeste_engine::runtime2::{cell_mix, AV, KEY_SEED1, KEY_SEED2};
                let seed = if *half == 0 { KEY_SEED1 } else { KEY_SEED2 };
                let av_of = |l: usize| -> AV {
                    match vals[node.args[0] as usize] {
                        V::N(z) => AV::Num(z.to_array()[l]),
                        V::I(z) => AV::Ival(z.lo.to_array()[l], z.hi.to_array()[l]),
                        V::B(z) => {
                            if z.known & (1 << l) != 0 {
                                AV::Bool(z.val & (1 << l) != 0)
                            } else {
                                AV::UBool
                            }
                        }
                        V::W(_) => panic!("node {id}: CellMix of a word"),
                    }
                };
                V::W(std::array::from_fn(|l| cell_mix(*c as u64, av_of(l), seed)))
            }
            Op::Span => V::I(ZI { lo: iv(0).lo, hi: iv(1).hi }),
            Op::Frag(c) => V::I(zi_fork_flr(iv(0), *c as usize).0),
            Op::FragOk(c) => {
                let (_, ok) = zi_fork_flr(iv(0), *c as usize);
                V::B(ZB { val: ok, known: ALL })
            }
            Op::SplitOk => V::B(zi_span_ok(iv(0))),
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
            other => panic!("oracle: unsupported op {other:?}"),
        };
        vals.push(v);
    }
    vals
}

// ---- typed-root coverage (value + bool + interval layers) ----

use crate::transpile::asm::RootKind;

/// Read raw output bytes for `n_roots` 128-byte slots.
fn run_asm_raw(
    loaded: &crate::transpile::asm::Loaded,
    input: &[u8],
    n_roots: usize,
    ctx: *const std::os::raw::c_void,
) -> Vec<u8> {
    let mut out = vec![0u8; n_roots * 128];
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

/// Assert the emitted kernel's typed roots match the reference evaluator.
fn check_typed(g: &Graph, roots: &[NodeId], tag: &str, rng: &mut Lcg) {
    let (compiled, loaded) = compile_and_load(g, roots, tag).expect("compile+load");
    for _ in 0..8 {
        let cols = random_columns(&compiled.input_cells, rng);
        let input = pack_inputs(&compiled.input_cells, &cols);
        let out = run_asm_raw(&loaded, &input, compiled.n_roots, std::ptr::null());
        let vals = eval_nodes(g, &cols, None);
        for (ri, r) in roots.iter().enumerate() {
            let base = ri * 128;
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
                    assert_eq!(val, z.val, "{tag}: bool val root {ri}");
                    assert_eq!(known, z.known, "{tag}: bool known root {ri}");
                }
                (RootKind::Word, V::W(z)) => {
                    let got: [u64; 16] = std::array::from_fn(|l| {
                        let o = base + (l / 8) * 64 + (l % 8) * 8;
                        u64::from_le_bytes(out[o..o + 8].try_into().unwrap())
                    });
                    assert_eq!(got, z, "{tag}: word root {ri}");
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
/// overflow (the kernel `zi_*` ops panic on overflow, a contract path we do
/// not replicate).
fn random_small_columns(cells: &[u32], rng: &mut Lcg) -> HashMap<u32, [i32; 16]> {
    cells
        .iter()
        .map(|c| (*c, std::array::from_fn::<i32, 16, _>(|_| (rng.i32() % 0x8_0000) - 0x4_0000)))
        .collect()
}

/// Like `check_typed` but with bounded inputs, for the interval ops.
fn check_typed_small(g: &Graph, roots: &[NodeId], tag: &str, rng: &mut Lcg) {
    let (compiled, loaded) = compile_and_load(g, roots, tag).expect("compile+load");
    for _ in 0..8 {
        let cols = random_small_columns(&compiled.input_cells, rng);
        let input = pack_inputs(&compiled.input_cells, &cols);
        let out = run_asm_raw(&loaded, &input, compiled.n_roots, std::ptr::null());
        let vals = eval_nodes(g, &cols, None);
        for (ri, r) in roots.iter().enumerate() {
            let base = ri * 128;
            match (compiled.root_kinds[ri], vals[*r as usize]) {
                (RootKind::Num, V::N(z)) => {
                    assert_eq!(&out[base..base + 64], &zn_bytes(z), "{tag}: num root {ri}");
                }
                (RootKind::Ival, V::I(z)) => {
                    assert_eq!(&out[base..base + 64], &zn_bytes(z.lo), "{tag}: ival lo {ri}");
                    assert_eq!(&out[base + 64..base + 128], &zn_bytes(z.hi), "{tag}: ival hi {ri}");
                }
                (RootKind::Bool, V::B(z)) => {
                    // Only the KNOWN lanes' val bits are defined; compare val
                    // masked by known, and known exactly.
                    let val = u16::from_le_bytes([out[base], out[base + 1]]);
                    let known = u16::from_le_bytes([out[base + 2], out[base + 3]]);
                    assert_eq!(known, z.known, "{tag}: bool known root {ri}");
                    assert_eq!(val & known, z.val & known, "{tag}: bool val root {ri}");
                }
                (RootKind::Word, V::W(z)) => {
                    let got: [u64; 16] = std::array::from_fn(|l| {
                        let o = base + (l / 8) * 64 + (l % 8) * 8;
                        u64::from_le_bytes(out[o..o + 8].try_into().unwrap())
                    });
                    assert_eq!(got, z, "{tag}: word root {ri}");
                }
                (k, _) => panic!("{tag}: root {ri} kind {k:?} vs oracle type mismatch"),
            }
        }
    }
}

/// Build intervals (via Span and a wide Const) and exercise the interval
/// ops: zi_add/sub/min/max/neg/abs, zi_flr + Known(Flr), zi_cmp (all four
/// orders), Frag/FragOk/SplitOk, Sel over intervals, and Bits-of-interval
/// feeding the hash.
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
        let ab = g.add(Op::Abs, vec![neg]);
        let mn = g.add(Op::Min, vec![ab, ivl]);
        let mx = g.add(Op::Max, vec![mn, shifted]);
        // floor + its premise
        let fl = g.add(Op::Flr, vec![mx]);
        let flok = g.add(Op::Known, vec![fl]);
        // interval comparisons
        let lt = g.add(Op::Lt, vec![ivl, shifted]);
        let ge = g.add(Op::Ge, vec![mx, ivl]);
        // fork
        let spanok = g.add(Op::SplitOk, vec![mx]);
        let frag0 = g.add(Op::Frag(0), vec![mx]);
        let ok0 = g.add(Op::FragOk(0), vec![mx]);
        let frag1 = g.add(Op::Frag(1), vec![mx]);
        let ok1 = g.add(Op::FragOk(1), vec![mx]);
        // select an interval on a decided bool
        let seli = g.add(Op::Sel, vec![spanok, frag0, frag1]);
        // a spread of typed roots
        roots.push(mx);
        roots.push(fl);
        roots.push(flok);
        roots.push(lt);
        roots.push(ge);
        roots.push(spanok);
        roots.push(ok0);
        roots.push(ok1);
        roots.push(seli);
    }
    (g, roots)
}

#[test]
fn asm_interval_layer_matches_primitives() {
    let mut rng = Lcg(0x1234_9999);
    for k in [2usize, 4, 7] {
        let cells: Vec<u32> = (0..k as u32).map(|i| i * 3 + 2).collect();
        let (g, roots) = build_interval_graph(&cells);
        check_typed_small(&g, &roots, &format!("iv{k}"), &mut rng);
    }
}

/// Compare typed roots with a real `AsmCtx` (for the call-out ops). `env`
/// may be null for div/rem/sin (they do not touch it).
fn check_typed_ctx(
    g: &Graph,
    roots: &[NodeId],
    tag: &str,
    rng: &mut Lcg,
    ctx: &crate::transpile::asm::AsmCtx,
) {
    let (compiled, loaded) = compile_and_load(g, roots, tag).expect("compile+load");
    let ctxp = ctx as *const _ as *const std::os::raw::c_void;
    for _ in 0..8 {
        let cols = random_small_columns(&compiled.input_cells, rng);
        let input = pack_inputs(&compiled.input_cells, &cols);
        let out = run_asm_raw(&loaded, &input, compiled.n_roots, ctxp);
        let vals = eval_nodes(g, &cols, None);
        for (ri, r) in roots.iter().enumerate() {
            let base = ri * 128;
            match (compiled.root_kinds[ri], vals[*r as usize]) {
                (RootKind::Num, V::N(z)) => {
                    assert_eq!(&out[base..base + 64], &zn_bytes(z), "{tag}: num root {ri}");
                }
                (RootKind::Bool, V::B(z)) => {
                    let val = u16::from_le_bytes([out[base], out[base + 1]]);
                    let known = u16::from_le_bytes([out[base + 2], out[base + 3]]);
                    assert_eq!(known, z.known, "{tag}: bool known {ri}");
                    assert_eq!(val & known, z.val & known, "{tag}: bool val {ri}");
                }
                (RootKind::Word, V::W(z)) => {
                    let got: [u64; 16] = std::array::from_fn(|l| {
                        let o = base + (l / 8) * 64 + (l % 8) * 8;
                        u64::from_le_bytes(out[o..o + 8].try_into().unwrap())
                    });
                    assert_eq!(got, z, "{tag}: word root {ri}");
                }
                (k, _) => panic!("{tag}: root {ri} kind {k:?} mismatch"),
            }
        }
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
    check_typed_ctx(&g, &roots, "callout", &mut rng, &ctx);
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

    let (compiled, loaded) = compile_and_load(&g, &roots, "collision").expect("compile+load");
    let ctxp = &ctx as *const _ as *const std::os::raw::c_void;
    let mut rng = Lcg(0xc0111_5107);
    for _ in 0..8 {
        let cols = random_columns(&compiled.input_cells, &mut rng);
        let input = pack_inputs(&compiled.input_cells, &cols);
        let out = run_asm_raw(&loaded, &input, compiled.n_roots, ctxp);
        let vals = eval_nodes(&g, &cols, Some((&cart, &cache)));
        for (ri, r) in roots.iter().enumerate() {
            let base = ri * 128;
            match (compiled.root_kinds[ri], vals[*r as usize]) {
                (RootKind::Num, V::N(z)) => {
                    assert_eq!(&out[base..base + 64], &zn_bytes(z), "collision num root {ri}");
                }
                (RootKind::Bool, V::B(z)) => {
                    let val = u16::from_le_bytes([out[base], out[base + 1]]);
                    let known = u16::from_le_bytes([out[base + 2], out[base + 3]]);
                    assert_eq!(known, z.known, "collision bool known {ri}");
                    assert_eq!(val & known, z.val & known, "collision bool val {ri}");
                }
                (RootKind::Word, V::W(z)) => {
                    let got: [u64; 16] = std::array::from_fn(|l| {
                        let o = base + (l / 8) * 64 + (l % 8) * 8;
                        u64::from_le_bytes(out[o..o + 8].try_into().unwrap())
                    });
                    assert_eq!(got, z, "collision word root {ri}");
                }
                (k, _) => panic!("collision root {ri} kind {k:?} mismatch"),
            }
        }
    }
}

#[test]
fn asm_value_and_bool_layer_matches_primitives() {
    let mut rng = Lcg(0x51ce_d00d);
    for k in [3usize, 5, 9, 16] {
        let cells: Vec<u32> = (0..k as u32).map(|i| i * 2 + 3).collect();
        let (g, roots) = build_value_graph(&cells, &mut rng);
        check_typed(&g, &roots, &format!("val{k}"), &mut rng);
    }
}

/// A bool INPUT cell (`CellRepr::Bool`): the input buffer carries a 16-bit
/// `val` mask (known implicitly all-ones), the codegen expands it to a
/// per-lane vector mask, and it flows through `Not`/`Sel` to Num and Bool
/// roots. This is the input marshalling the ASM kernel dispatch needs -
/// room (1,0) kernels take `has_dashed`/`will_restart`/... as bool inputs.
/// Compared bit-exact against the real `ZB`/`ZN` primitives.
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
        let out = run_asm_raw(&loaded, &input, compiled.n_roots, std::ptr::null());

        let zb = ZB { val: mask, known: ALL };
        let zn0 = ZN::from_array(std::array::from_fn(|i| P8::from_raw(num0[i])));
        let zn2 = ZN::from_array(std::array::from_fn(|i| P8::from_raw(num2[i])));
        let e_sel = zsel_n(zb, zn0, zn2);
        let e_seln = zsel_n(zb_not(zb), zn2, zn0);
        let e_not = zb_not(zb);

        assert_eq!(&out[0..64], &zn_bytes(e_sel), "trial {trial}: sel");
        assert_eq!(&out[128..192], &zn_bytes(e_seln), "trial {trial}: seln");
        assert_eq!(
            u16::from_le_bytes([out[256], out[257]]),
            e_not.val,
            "trial {trial}: not val"
        );
        assert_eq!(
            u16::from_le_bytes([out[258], out[259]]),
            e_not.known,
            "trial {trial}: not known"
        );
        assert_eq!(
            u16::from_le_bytes([out[384], out[385]]),
            mask,
            "trial {trial}: passthrough val"
        );
        assert_eq!(
            u16::from_le_bytes([out[386], out[387]]),
            ALL,
            "trial {trial}: passthrough known"
        );
    }
}

/// An interval (`ZI`) INPUT cell (`CellRepr::Ival`): two `ZN` planes (lo at
/// +0, hi at +64 of a 128-byte slot), the codegen loads both, and the
/// interval flows through `Add`/`Flr` to Ival and Num roots. Real room
/// kernels take `player.rem` as an ival input. Packed via `input_offsets`
/// (repr-aware layout) and compared bit-exact against `zi_add`/`zi_flr`.
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
        let out = run_asm_raw(&loaded, &input, compiled.n_roots, std::ptr::null());

        let ziv = ZI {
            lo: ZN::from_array(std::array::from_fn(|i| P8::from_raw(lo[i]))),
            hi: ZN::from_array(std::array::from_fn(|i| P8::from_raw(hi[i]))),
        };
        let znum = ZN::from_array(std::array::from_fn(|i| P8::from_raw(num[i])));
        let e_sum = zi_add(ziv, ZI { lo: znum, hi: znum });
        let e_fl = zi_flr(e_sum);

        assert_eq!(&out[0..64], &zn_bytes(e_sum.lo), "trial {trial}: sum lo");
        assert_eq!(&out[64..128], &zn_bytes(e_sum.hi), "trial {trial}: sum hi");
        assert_eq!(&out[128..192], &zn_bytes(e_fl), "trial {trial}: flr");
        assert_eq!(&out[256..320], &zn_bytes(ziv.lo), "trial {trial}: passthrough lo");
        assert_eq!(&out[320..384], &zn_bytes(ziv.hi), "trial {trial}: passthrough hi");
    }
}

// ---------------------------------------------------------------------------
// Benchmark: asserts BOTH the asm backend and a self-contained rustc build
// of the identical graph match the primitives, then prints compile-time and
// runtime numbers. #[ignore] because it shells out to rustc (~0.3 s) and is
// only meaningful under an optimized profile.
// ---------------------------------------------------------------------------

/// The row key in the graph: `CellMix` over a number, an interval and a
/// tri-state bool, summed with `AddW`, against the boundary's scalar
/// `runtime2::cell_mix` per lane. This is what makes `compiled::asm_kernel`'s
/// keys the boundary's keys.
#[test]
fn asm_cell_mix_matches_the_boundary_cell_mix() {
    let mut rng = Lcg(0x5eed_5eed);
    let mut g = Graph::new();
    let two = g.leaf(Op::Const(ONE_FIXED2, ONE_FIXED2));
    let cells: Vec<u32> = (0..4u32).map(|i| i * 5 + 3).collect();
    let cs: Vec<NodeId> = cells.iter().map(|c| g.leaf(Op::Cell(*c))).collect();
    let mut roots = Vec::new();
    for half in 0..2u8 {
        let mut h = g.leaf(Op::Word(0));
        for (i, &c) in cs.iter().enumerate() {
            // a number
            let m = g.add(Op::CellMix(cells[i], half), vec![c]);
            h = g.add(Op::AddW, vec![h, m]);
            // an interval [c, c + 2]
            let hi = g.add(Op::Add, vec![c, two]);
            let iv = g.add(Op::Span, vec![c, hi]);
            let m = g.add(Op::CellMix(cells[i] + 100, half), vec![iv]);
            h = g.add(Op::AddW, vec![h, m]);
            // a tri-state bool: an interval compare (unknown where they overlap)
            let other = cs[(i + 1) % cs.len()];
            let hi2 = g.add(Op::Add, vec![other, two]);
            let iv2 = g.add(Op::Span, vec![other, hi2]);
            let lt = g.add(Op::Lt, vec![iv, iv2]);
            let m = g.add(Op::CellMix(cells[i] + 200, half), vec![lt]);
            h = g.add(Op::AddW, vec![h, m]);
            // a decided bool
            let eq = g.add(Op::Eq, vec![c, other]);
            let m = g.add(Op::CellMix(cells[i] + 300, half), vec![eq]);
            h = g.add(Op::AddW, vec![h, m]);
        }
        roots.push(h);
    }
    check_typed_small(&g, &roots, "cellmix", &mut rng);
}
