//! Correctness gate for the asm backend: emitted code vs the real
//! `celeste_engine::kernel` primitives, bit-exact on random inputs. The
//! benchmark at the end (ignored) also compiles a self-contained rustc
//! equivalent of the identical graph and compares compile-time and runtime.

use std::collections::HashMap;

const ONE_FIXED2: i32 = 0x0002_0000; // +2.0 in 16.16
use std::fmt::Write as _;

use celeste_engine::kernel::{
    zb_and, zb_eq, zb_not, zb_or, zi_abs, zi_add, zi_cmp, zi_flr, zi_fork_flr, zi_max, zi_min,
    zi_neg, zi_span_ok, zi_sub, zn_abs, zn_add, zn_eq, zn_flr, zn_ge, zn_gt, zn_le, zn_lt, zn_max,
    zn_min, zn_mul, zn_neg, zn_sub, zsel_b, zsel_i, zsel_n, zw_bits_b, zw_bits_i, zw_bits_n,
    zn_div, zn_mget, zn_rem, zn_sin, zn_tile_flag_at, zw_mix1, zw_mix2, zw_splat, Cmp, ZB, ZI,
    ZN, ZW, ALL,
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
    W(ZW),
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
        let w = |k: usize| -> ZW {
            match vals[node.args[k] as usize] {
                V::W(z) => z,
                _ => panic!("node {id}: expected word operand"),
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
                        _ => panic!("Known of unsupported domain"),
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
            Op::Bits if is_i(0) => V::W(zw_bits_i(iv(0))),
            Op::Bits if dom_bool(0) => V::W(zw_bits_b(b(0))),
            Op::Bits => V::W(zw_bits_n(n(0))),
            Op::Word(x) => V::W(zw_splat(*x)),
            Op::Mix(c, 0) => V::W(zw_mix1(w(0), w(1), *c as u64)),
            Op::Mix(c, _) => V::W(zw_mix2(w(0), w(1), *c as u64)),
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

/// Word-root oracle for the hashing tests.
fn oracle(g: &Graph, roots: &[NodeId], cols: &HashMap<u32, [i32; 16]>) -> Vec<[u64; 16]> {
    let vals = eval_nodes(g, cols, None);
    roots
        .iter()
        .map(|r| match vals[*r as usize] {
            V::W(z) => z.to_array(),
            _ => panic!("root is not a word"),
        })
        .collect()
}

/// Run the loaded asm kernel and read back the roots as `[u64; 16]` each.
fn run_asm(loaded: &crate::transpile::asm::Loaded, input: &[u8], n_roots: usize) -> Vec<[u64; 16]> {
    let mut out = vec![0u8; n_roots * 128];
    unsafe {
        (loaded.func)(input.as_ptr(), out.as_mut_ptr(), std::ptr::null());
    }
    (0..n_roots)
        .map(|r| {
            std::array::from_fn(|l| {
                let off = r * 128 + l * 8;
                u64::from_le_bytes(out[off..off + 8].try_into().unwrap())
            })
        })
        .collect()
}

/// The row-key fold over `cells`, exactly as `lower.rs` builds it: h1 via
/// `Mix(_,0)`, h2 via `Mix(_,1)`.
fn build_fold(cells: &[u32]) -> (Graph, Vec<NodeId>) {
    let mut g = Graph::new();
    let mut h1 = g.leaf(Op::Word(0x9e37_79b9_7f4a_7c15));
    let mut h2 = g.leaf(Op::Word(0xa076_1d64_78bd_642f));
    for &c in cells {
        let cell = g.leaf(Op::Cell(c));
        let bits = g.add(Op::Bits, vec![cell]);
        h1 = g.add(Op::Mix(c, 0), vec![h1, bits]);
        h2 = g.add(Op::Mix(c, 1), vec![h2, bits]);
    }
    (g, vec![h1, h2])
}

/// The same fold with arithmetic before each `Bits`, to cover
/// `Add/Sub/Min/Max/Neg/Abs/Flr` on the value path.
fn build_fold_arith(cells: &[u32], rng: &mut Lcg) -> (Graph, Vec<NodeId>) {
    let mut g = Graph::new();
    let mut h1 = g.leaf(Op::Word(0x1234_5678_9abc_def0));
    let mut h2 = g.leaf(Op::Word(0x0fed_cba9_8765_4321));
    for &c in cells {
        let cell = g.leaf(Op::Cell(c));
        let kv = (rng.i32() >> 8) << 8;
        let k = g.leaf(Op::Const(kv, kv));
        let v = match rng.next_u64() % 8 {
            0 => g.add(Op::Add, vec![cell, k]),
            1 => g.add(Op::Sub, vec![cell, k]),
            2 => g.add(Op::Min, vec![cell, k]),
            3 => g.add(Op::Max, vec![cell, k]),
            4 => g.add(Op::Neg, vec![cell]),
            5 => g.add(Op::Abs, vec![cell]),
            6 => g.add(Op::Mul, vec![cell, k]),
            _ => g.add(Op::Flr, vec![cell]),
        };
        let v = if rng.next_u64() & 1 == 0 { g.add(Op::Flr, vec![v]) } else { v };
        let bits = g.add(Op::Bits, vec![v]);
        h1 = g.add(Op::Mix(c, 0), vec![h1, bits]);
        h2 = g.add(Op::Mix(c, 1), vec![h2, bits]);
    }
    (g, vec![h1, h2])
}

/// A WIDE key graph, like a real kernel: every cell's `Bits` is computed
/// once and shared by every row, so all `n_cells` bit-pairs plus every
/// row's running accumulator are live at once. This is where register
/// pressure actually comes from (the sequential fold in `build_fold` keeps
/// only ~7 values live and never spills).
fn build_wide(n_cells: u32, n_rows: u32) -> (Graph, Vec<NodeId>) {
    let mut g = Graph::new();
    let bits: Vec<NodeId> = (0..n_cells)
        .map(|c| {
            let cell = g.leaf(Op::Cell(c + 100));
            g.add(Op::Bits, vec![cell])
        })
        .collect();
    let mut roots = Vec::new();
    for r in 0..n_rows {
        let mut h = g.leaf(Op::Word(0x9e37_79b9_7f4a_7c15u64.wrapping_mul(r as u64 + 1)));
        for (c, b) in bits.iter().enumerate() {
            h = g.add(Op::Mix(c as u32 + 100, (r & 1) as u8), vec![h, *b]);
        }
        roots.push(h);
    }
    (g, roots)
}

fn check(g: &Graph, roots: &[NodeId], tag: &str, rng: &mut Lcg) -> usize {
    let (compiled, loaded) = compile_and_load(g, roots, tag).expect("compile+load");
    for _ in 0..8 {
        let cols = random_columns(&compiled.input_cells, rng);
        let input = pack_inputs(&compiled.input_cells, &cols);
        let got = run_asm(&loaded, &input, compiled.n_roots);
        let want = oracle(g, roots, &cols);
        assert_eq!(got.len(), want.len());
        for (r, (a, b)) in got.iter().zip(want.iter()).enumerate() {
            assert_eq!(a, b, "{tag}: root {r} mismatch\n asm ={a:016x?}\n want={b:016x?}");
        }
    }
    compiled.spill_slots
}

#[test]
fn asm_row_key_matches_primitives() {
    let mut rng = Lcg(0xdead_beef);
    for k in [1usize, 2, 3, 5, 8] {
        let cells: Vec<u32> = (0..k as u32).map(|i| i * 3 + 1).collect();
        let (g, roots) = build_fold(&cells);
        // `check` asserts bit-exactness against the primitives. (Spill count
        // is not asserted here: the instruction-level list scheduler
        // interleaves aggressively and may spill even small folds - spills
        // are free via remat/reload and off the critical path.)
        check(&g, &roots, &format!("small{k}"), &mut rng);
    }
    for k in [3usize, 6, 10] {
        let cells: Vec<u32> = (0..k as u32).map(|i| i * 2 + 5).collect();
        let (g, roots) = build_fold_arith(&cells, &mut rng);
        check(&g, &roots, &format!("arith{k}"), &mut rng);
    }
}

#[test]
fn asm_row_key_matches_under_spilling() {
    let mut rng = Lcg(0x0123_4567_89ab_cdef);
    // 14 cells shared by 14 rows: 28 live bit-halves plus per-row
    // accumulators blow past the 25 homes, forcing spills and exercising
    // the reload path. Still bit-exact against the primitives.
    let (g, roots) = build_wide(14, 14);
    let spill = check(&g, &roots, "wide", &mut rng);
    assert!(spill > 0, "a 14x14 wide key must force spilling, got {spill}");
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
                        u64::from_le_bytes(out[base + l * 8..base + l * 8 + 8].try_into().unwrap())
                    });
                    assert_eq!(got, z.to_array(), "{tag}: word root {ri}");
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
    let seed1 = g.leaf(Op::Word(0x9e37_79b9_7f4a_7c15));
    let seed2 = g.leaf(Op::Word(0xa076_1d64_78bd_642f));
    let mut roots = Vec::new();
    let mut h1 = seed1;
    let mut h2 = seed2;
    for (i, c) in cells.iter().enumerate() {
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
        // hash the floor and the interval
        let bfl = g.add(Op::Bits, vec![fl]);
        let biv = g.add(Op::Bits, vec![seli]);
        let bok = g.add(Op::Bits, vec![flok]);
        h1 = g.add(Op::Mix(*c, 0), vec![h1, bfl]);
        h1 = g.add(Op::Mix(*c + 1, 0), vec![h1, biv]);
        h2 = g.add(Op::Mix(*c, 1), vec![h2, bok]);
        // a spread of typed roots
        roots.push(mx);
        roots.push(fl);
        roots.push(lt);
        roots.push(ge);
        roots.push(spanok);
        roots.push(ok0);
        roots.push(ok1);
        roots.push(seli);
        if i == cells.len() - 1 {
            roots.push(h1);
            roots.push(h2);
        }
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
                        u64::from_le_bytes(out[base + l * 8..base + l * 8 + 8].try_into().unwrap())
                    });
                    assert_eq!(got, z.to_array(), "{tag}: word root {ri}");
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
    let seed = g.leaf(Op::Word(0x1234_5678_9abc_def0));
    let mut h = seed;
    for (i, &c) in cs.iter().enumerate() {
        let d = g.add(Op::Div, vec![c, three]);
        let r = g.add(Op::Rem, vec![c, three]);
        let sn = g.add(Op::Sin, vec![c]);
        roots.push(d);
        roots.push(r);
        roots.push(sn);
        // Also route through the hash so a call-out result feeds Bits/Mix.
        let bd = g.add(Op::Bits, vec![d]);
        h = g.add(Op::Mix(cells[i], 0), vec![h, bd]);
    }
    roots.push(h);
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
    let seed = g.leaf(Op::Word(0xdead_beef_0bad_f00d));
    let mut h = seed;
    for (i, w) in cs.windows(2).enumerate() {
        let xt = clamp(&mut g, w[0], 0, 127);
        let yt = clamp(&mut g, w[1], 0, 63);
        let mg = g.add(Op::Mget, vec![xt, yt]);
        let xp = clamp(&mut g, w[0], 0, 118);
        let yp = clamp(&mut g, w[1], 0, 118);
        let tf = g.add(Op::TileFlagAt, vec![xp, yp, w8, w8, flag0]);
        roots.push(mg);
        roots.push(tf);
        // route mget through the hash to exercise a call result feeding Mix
        let bm = g.add(Op::Bits, vec![mg]);
        h = g.add(Op::Mix(cells[i], 0), vec![h, bm]);
    }
    roots.push(h);

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
                        u64::from_le_bytes(out[base + l * 8..base + l * 8 + 8].try_into().unwrap())
                    });
                    assert_eq!(got, z.to_array(), "collision word root {ri}");
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

// ---------------------------------------------------------------------------
// Benchmark: asserts BOTH the asm backend and a self-contained rustc build
// of the identical graph match the primitives, then prints compile-time and
// runtime numbers. #[ignore] because it shells out to rustc (~0.3 s) and is
// only meaningful under an optimized profile.
// ---------------------------------------------------------------------------

/// Emit a standalone Rust source implementing `roots` of `g` over the same
/// packed ABI as the asm kernel, using `core::arch` AVX-512 intrinsics that
/// mirror the kernel primitives. This is the "rustc+LLVM path" for exactly
/// the computation the asm backend emits.
fn emit_rust_equiv(g: &Graph, roots: &[NodeId], input_cells: &[u32], sym: &str) -> String {
    let mut off = HashMap::new();
    for (i, c) in input_cells.iter().enumerate() {
        off.insert(*c, i * 64);
    }
    let mut s = String::new();
    s.push_str("#![allow(non_upper_case_globals, unused)]\n");
    s.push_str("use core::arch::x86_64::*;\n");
    s.push_str("#[inline(always)] unsafe fn mix(x: __m512i) -> __m512i {\n");
    s.push_str("  let a = _mm512_xor_si512(x, _mm512_srli_epi64::<30>(x));\n");
    s.push_str("  let a = _mm512_mullo_epi64(a, _mm512_set1_epi64(0xbf58476d1ce4e5b9u64 as i64));\n");
    s.push_str("  let b = _mm512_xor_si512(a, _mm512_srli_epi64::<27>(a));\n");
    s.push_str("  let b = _mm512_mullo_epi64(b, _mm512_set1_epi64(0x94d049bb133111ebu64 as i64));\n");
    s.push_str("  _mm512_xor_si512(b, _mm512_srli_epi64::<31>(b)) }\n");
    let _ =
        writeln!(s, "#[no_mangle] pub unsafe extern \"C\" fn {sym}(inp: *const u8, out: *mut u8) {{");
    let mut live = vec![false; g.len()];
    let mut st = roots.to_vec();
    while let Some(n) = st.pop() {
        if live[n as usize] {
            continue;
        }
        live[n as usize] = true;
        st.extend(g.get(n).args.iter().copied());
    }
    for id in 0..g.len() as NodeId {
        if !live[id as usize] {
            continue;
        }
        let node = g.get(id);
        let a = &node.args;
        match &node.op {
            Op::Const(lo, _) => {
                let _ = writeln!(s, "  let n{id} = _mm512_set1_epi32({lo}i32);");
            }
            Op::Cell(c) => {
                let _ = writeln!(
                    s,
                    "  let n{id} = _mm512_loadu_si512(inp.add({}) as *const _);",
                    off[c]
                );
            }
            Op::Add => {
                let _ = writeln!(s, "  let n{id} = _mm512_add_epi32(n{}, n{});", a[0], a[1]);
            }
            Op::Sub => {
                let _ = writeln!(s, "  let n{id} = _mm512_sub_epi32(n{}, n{});", a[0], a[1]);
            }
            Op::Min => {
                let _ = writeln!(s, "  let n{id} = _mm512_min_epi32(n{}, n{});", a[0], a[1]);
            }
            Op::Max => {
                let _ = writeln!(s, "  let n{id} = _mm512_max_epi32(n{}, n{});", a[0], a[1]);
            }
            Op::Neg => {
                let _ = writeln!(
                    s,
                    "  let n{id} = _mm512_sub_epi32(_mm512_setzero_si512(), n{});",
                    a[0]
                );
            }
            Op::Abs => {
                let _ = writeln!(s, "  let n{id} = _mm512_abs_epi32(n{});", a[0]);
            }
            Op::Flr => {
                let _ = writeln!(
                    s,
                    "  let n{id} = _mm512_and_si512(n{}, _mm512_set1_epi32(0xffff0000u32 as i32));",
                    a[0]
                );
            }
            Op::Bits => {
                let _ = writeln!(
                    s,
                    "  let w{id}_0 = _mm512_cvtepu32_epi64(_mm512_castsi512_si256(n{}));",
                    a[0]
                );
                let _ = writeln!(
                    s,
                    "  let w{id}_1 = _mm512_cvtepu32_epi64(_mm512_extracti64x4_epi64::<1>(n{}));",
                    a[0]
                );
            }
            Op::Word(w) => {
                let _ = writeln!(s, "  let w{id}_0 = _mm512_set1_epi64({}u64 as i64);", w);
                let _ = writeln!(s, "  let w{id}_1 = w{id}_0;");
            }
            Op::Mix(c, half) => {
                let (h, v) = (a[0], a[1]);
                for i in 0..2 {
                    if *half == 0 {
                        let _ = writeln!(
                            s,
                            "  let w{id}_{i} = mix(_mm512_xor_si512(w{h}_{i}, mix(_mm512_xor_si512(w{v}_{i}, _mm512_set1_epi64({}i64)))));",
                            *c as i64
                        );
                    } else {
                        let k = ((*c as u64) << 1) | 1;
                        let _ = writeln!(
                            s,
                            "  let w{id}_{i} = _mm512_add_epi64(w{h}_{i}, mix(_mm512_mullo_epi64(w{v}_{i}, _mm512_set1_epi64({}u64 as i64))));",
                            k
                        );
                    }
                }
            }
            other => panic!("emit_rust_equiv: unsupported {other:?}"),
        }
    }
    for (ri, r) in roots.iter().enumerate() {
        let _ = writeln!(s, "  _mm512_storeu_si512(out.add({}) as *mut _, w{r}_0);", ri * 128);
        let _ = writeln!(s, "  _mm512_storeu_si512(out.add({}) as *mut _, w{r}_1);", ri * 128 + 64);
    }
    s.push_str("}\n");
    s
}

fn scratch() -> std::path::PathBuf {
    let mut p = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    p.push("target");
    p.push("asm-scratch");
    let _ = std::fs::create_dir_all(&p);
    p
}

#[test]
#[ignore = "benchmark; run under an optimized profile"]
fn asm_backend_benchmark() {
    use std::time::Instant;
    let mut rng = Lcg(0xbeef_cafe);
    // Representative of kernel1's hashing mix (~1150 Mix ops, cf. 623+623).
    let (g, roots) = build_wide(24, 24);
    let sym = "kbench";

    // --- my backend: emit + assemble time ---
    let t = Instant::now();
    let compiled =
        crate::transpile::asm::compile(&g, &roots, &format!("kernel_{sym}"), &HashMap::new())
            .unwrap();
    let emit_ms = t.elapsed().as_secs_f64() * 1e3;
    let t = Instant::now();
    let so_asm = crate::transpile::asm::assemble(&compiled.asm, "bench_asm").unwrap();
    let assemble_ms = t.elapsed().as_secs_f64() * 1e3;
    let asm_loaded =
        crate::transpile::asm::Loaded::open(&so_asm, &format!("kernel_{sym}")).unwrap();

    // --- rustc build of the identical computation (slow; opt-in) ---
    // Set CELESTE_ASM_FULL=1 to also compile+time the LLVM equivalent.
    // Without it the fixed 678 ns/call baseline is assumed and only the asm
    // backend is (re)measured, so hill-climbing iterates in ~0.5 s.
    let full = std::env::var("CELESTE_ASM_FULL").is_ok();
    let dir = scratch();
    let rs = dir.join("bench_equiv.rs");
    let (rustc_ms, rust_loaded) = if full {
        let rust_src = emit_rust_equiv(&g, &roots, &compiled.input_cells, sym);
        let so_rust = dir.join("bench_equiv.so");
        std::fs::write(&rs, &rust_src).unwrap();
        let t = Instant::now();
        let out = std::process::Command::new("rustc")
            .args(["-O", "-C", "target-cpu=native", "--crate-type", "cdylib", "--edition", "2021"])
            .arg(&rs)
            .arg("-o")
            .arg(&so_rust)
            .output()
            .unwrap();
        let ms = t.elapsed().as_secs_f64() * 1e3;
        assert!(out.status.success(), "rustc failed:\n{}", String::from_utf8_lossy(&out.stderr));
        (Some(ms), Some(crate::transpile::asm::Loaded::open(&so_rust, sym).unwrap()))
    } else {
        (None, None)
    };

    // --- correctness: the asm backend (and rustc equiv if built) match ---
    for _ in 0..4 {
        let cols = random_columns(&compiled.input_cells, &mut rng);
        let input = pack_inputs(&compiled.input_cells, &cols);
        let want = oracle(&g, &roots, &cols);
        assert_eq!(run_asm(&asm_loaded, &input, compiled.n_roots), want, "asm backend");
        if let Some(rl) = &rust_loaded {
            assert_eq!(run_asm(rl, &input, compiled.n_roots), want, "rustc equiv");
        }
    }

    // --- runtime: ns per call, both dlopened ---
    let cols = random_columns(&compiled.input_cells, &mut rng);
    let input = pack_inputs(&compiled.input_cells, &cols);
    let mut out_buf = vec![0u8; compiled.n_roots * 128];
    let iters = 200_000u64;
    let mut bench = |f: crate::transpile::asm::KernelFn| -> f64 {
        for _ in 0..2000 {
            unsafe { f(input.as_ptr(), out_buf.as_mut_ptr(), std::ptr::null()) };
        }
        let t = Instant::now();
        for _ in 0..iters {
            unsafe {
                f(std::hint::black_box(input.as_ptr()), std::hint::black_box(out_buf.as_mut_ptr()), std::ptr::null())
            };
        }
        t.elapsed().as_nanos() as f64 / iters as f64
    };
    let ns_asm = bench(asm_loaded.func);
    let ns_rust = rust_loaded.as_ref().map(|rl| bench(rl.func));

    // Instruction mix of the asm kernel (mnemonic histogram + spills).
    let obj = std::process::Command::new("objdump").arg("-d").arg(&so_asm).output().unwrap();
    let dis = String::from_utf8_lossy(&obj.stdout);
    let n_spill = dis.matches("(%rsp)").count();
    let mut hist: HashMap<String, usize> = HashMap::new();
    for l in dis.lines() {
        if let Some(m) = l.split('\t').nth(2) {
            if let Some(mn) = m.split_whitespace().next() {
                if mn.starts_with('v') {
                    *hist.entry(mn.to_string()).or_default() += 1;
                }
            }
        }
    }
    let mut hv: Vec<_> = hist.into_iter().collect();
    hv.sort_by_key(|(_, c)| std::cmp::Reverse(*c));

    eprintln!("\n=== asm-backend benchmark (build_wide 24x24, {} graph nodes) ===", g.len());
    eprintln!("spill slots={}, frame={}B, rsp refs={}", compiled.spill_slots, compiled.spill_slots * 64, n_spill);
    if let (Some(rustc_ms), Some(ns_rust)) = (rustc_ms, ns_rust) {
        eprintln!(
            "COMPILE: emit={:.2} ms + assemble={:.2} ms = {:.2} ms | rustc -O = {:.1} ms ({:.0}x)",
            emit_ms, assemble_ms, emit_ms + assemble_ms, rustc_ms, rustc_ms / (emit_ms + assemble_ms)
        );
        eprintln!("RUNTIME: asm={:.1} ns/call, rustc-equiv={:.1} ns/call (rust/asm={:.2})",
            ns_asm, ns_rust, ns_rust / ns_asm);
    } else {
        eprintln!("COMPILE: emit={:.2} ms + assemble={:.2} ms", emit_ms, assemble_ms);
        eprintln!("RUNTIME: asm={:.1} ns/call (LLVM baseline 678)", ns_asm);
    }
    eprintln!("mnemonic mix: {:?}", &hv[..hv.len().min(8)]);

    let _ = std::fs::remove_file(&rs);
}
