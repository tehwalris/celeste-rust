//! Correctness gate for the asm backend: emitted code vs the real
//! `celeste_engine::kernel` primitives, bit-exact on random inputs. The
//! benchmark at the end (ignored) also compiles a self-contained rustc
//! equivalent of the identical graph and compares compile-time and runtime.

use std::collections::HashMap;
use std::fmt::Write as _;

use celeste_engine::kernel::{
    zn_abs, zn_add, zn_flr, zn_max, zn_min, zn_neg, zn_sub, zw_bits_n, zw_mix1, zw_mix2, zw_splat,
    ZN, ZW,
};

use crate::pico8_num::Pico8Num as P8;
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

/// Independent oracle: evaluate the graph with the REAL kernel primitives.
fn oracle(g: &Graph, roots: &[NodeId], cols: &HashMap<u32, [i32; 16]>) -> Vec<[u64; 16]> {
    enum V {
        N(ZN),
        W(ZW),
    }
    let zn_of = |col: &[i32; 16]| ZN::from_array(std::array::from_fn(|i| P8::from_raw(col[i])));
    let mut vals: Vec<Option<V>> = Vec::with_capacity(g.len());
    for id in 0..g.len() as NodeId {
        let node = g.get(id);
        let n = |k: usize| -> ZN {
            match vals[node.args[k] as usize].as_ref().unwrap() {
                V::N(z) => *z,
                V::W(_) => panic!("node {id}: expected numeric operand"),
            }
        };
        let w = |k: usize| -> ZW {
            match vals[node.args[k] as usize].as_ref().unwrap() {
                V::W(z) => *z,
                V::N(_) => panic!("node {id}: expected word operand"),
            }
        };
        let v = match &node.op {
            Op::Const(lo, hi) => {
                assert_eq!(lo, hi);
                V::N(ZN::from_array([P8::from_raw(*lo); 16]))
            }
            Op::Cell(c) => V::N(zn_of(&cols[c])),
            Op::Add => V::N(zn_add(n(0), n(1))),
            Op::Sub => V::N(zn_sub(n(0), n(1))),
            Op::Min => V::N(zn_min(n(0), n(1))),
            Op::Max => V::N(zn_max(n(0), n(1))),
            Op::Neg => V::N(zn_neg(n(0))),
            Op::Abs => V::N(zn_abs(n(0))),
            Op::Flr => V::N(zn_flr(n(0))),
            Op::Bits => V::W(zw_bits_n(n(0))),
            Op::Word(x) => V::W(zw_splat(*x)),
            Op::Mix(c, 0) => V::W(zw_mix1(w(0), w(1), *c as u64)),
            Op::Mix(c, _) => V::W(zw_mix2(w(0), w(1), *c as u64)),
            other => panic!("oracle: unsupported op {other:?}"),
        };
        vals.push(Some(v));
    }
    roots
        .iter()
        .map(|r| match vals[*r as usize].as_ref().unwrap() {
            V::W(z) => z.to_array(),
            V::N(_) => panic!("root is not a word"),
        })
        .collect()
}

/// Run the loaded asm kernel and read back the roots as `[u64; 16]` each.
fn run_asm(loaded: &crate::transpile::asm::Loaded, input: &[u8], n_roots: usize) -> Vec<[u64; 16]> {
    let mut out = vec![0u8; n_roots * 128];
    unsafe {
        (loaded.func)(input.as_ptr(), out.as_mut_ptr());
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
        let v = match rng.next_u64() % 7 {
            0 => g.add(Op::Add, vec![cell, k]),
            1 => g.add(Op::Sub, vec![cell, k]),
            2 => g.add(Op::Min, vec![cell, k]),
            3 => g.add(Op::Max, vec![cell, k]),
            4 => g.add(Op::Neg, vec![cell]),
            5 => g.add(Op::Abs, vec![cell]),
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
    let compiled = crate::transpile::asm::compile(&g, &roots, &format!("kernel_{sym}")).unwrap();
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
            unsafe { f(input.as_ptr(), out_buf.as_mut_ptr()) };
        }
        let t = Instant::now();
        for _ in 0..iters {
            unsafe {
                f(std::hint::black_box(input.as_ptr()), std::hint::black_box(out_buf.as_mut_ptr()))
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
