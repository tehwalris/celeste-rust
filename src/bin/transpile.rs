//! CLI for the tracer's analysis probes (`celeste_rust::trace::kernel`).
//!
//!   transpile --room-consts            the reachable constant lattice report
//!   transpile --spec-probe SHAPE       specialization collapse for one shape
//!   transpile --level-minus-one ...    the position-only cost-to-go table
//!
//! The kernel emitter is gone: the kernels are assembled at runtime from the
//! fused graph (`compiled::asm_kernel`), so there is nothing to generate.

use anyhow::{anyhow, Context, Result};

/// A speed precision argument as a level spec's `s<S>` (`20` both axes,
/// `20x` x only), through the level parser; 16 when absent.
fn spd_arg(s: Option<String>) -> Result<celeste_rust::interpreter::abstraction::SpdPrecision> {
    let s = s.unwrap_or_else(|| "16".to_string());
    let level = celeste_rust::interpreter::abstraction::Level::parse(&format!("r0s{s}")).map_err(|e| anyhow!("speed precision {s:?}: {e}"))?;
    Ok(level.spd)
}

fn main() -> Result<()> {
    let mut spec_probe: Option<usize> = None;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            // --spec-probe SHAPE: re-trace one shape with the player XY,
            // the springs, and a pm1 key pinned, and report the collapse
            // (plans/specialize.md). CELESTE_SPEC_PLAYER="x,y" and
            // CELESTE_SPEC_SPRINGS="x,y;x,y" override the pins.
            "--spec-probe" => {
                spec_probe = Some(
                    args.next()
                        .ok_or_else(|| anyhow!("--spec-probe SHAPE"))?
                        .parse()
                        .context("--spec-probe SHAPE index")?,
                );
            }
            // --key-build: build the level-0 bucketed kernel set as the
            // search does, with the per-phase build accounting.
            "--key-build" => {
                print!("{}", celeste_rust::trace::kernel::key_build(std::path::Path::new("."))?);
                return Ok(());
            }
            // --key-census [N [FILE [S]]]: the key fixpoint on its own; lower
            // the first N nodes; write the node set to FILE. S is the speed
            // precision as in a level spec's `s<S>`: `20` both axes, `20x`
            // x only (default 16).
            "--key-census" => {
                let lower: usize = args.next().map(|s| s.parse().context("--key-census N")).transpose()?.unwrap_or(0);
                let dump = args.next().map(std::path::PathBuf::from);
                let spd = spd_arg(args.next())?;
                let report = celeste_rust::trace::kernel::key_census(std::path::Path::new("."), lower, dump.as_deref(), spd)?;
                print!("{}", report);
                return Ok(());
            }
            // --key-probe FILE SEL [S]: trace and lower the nodes of a
            // --key-census node set at line indices SEL (comma-separated),
            // with the body breakdown. S is the speed precision the node set
            // was censused at, as in `--key-census` (default 16).
            "--key-probe" => {
                let dump = std::path::PathBuf::from(args.next().ok_or_else(|| anyhow!("--key-probe FILE SEL [W]"))?);
                let sel: Vec<usize> = args
                    .next()
                    .ok_or_else(|| anyhow!("--key-probe FILE SEL [W]"))?
                    .split(',')
                    .map(|s| s.trim().parse().context("--key-probe SEL index"))
                    .collect::<Result<_>>()?;
                let spd = spd_arg(args.next())?;
                let report = celeste_rust::trace::kernel::key_probe(std::path::Path::new("."), &dump, &sel, spd)?;
                print!("{}", report);
                return Ok(());
            }
            // --shape-diff A B [S]: where two shapes (by hash, as the
            // dispatch names them) differ, under the walk at speed precision
            // S as in `--key-census` (default 16).
            "--shape-diff" => {
                let hash = |s: Option<String>| -> Result<u64> {
                    let s = s.ok_or_else(|| anyhow!("--shape-diff A B [S]"))?;
                    u64::from_str_radix(s.trim_start_matches("0x"), 16).with_context(|| format!("--shape-diff shape hash {s:?}"))
                };
                let a = hash(args.next())?;
                let b = hash(args.next())?;
                let spd = spd_arg(args.next())?;
                print!("{}", celeste_rust::trace::kernel::shape_diff(std::path::Path::new("."), spd, a, b)?);
                return Ok(());
            }
            "--bucket-probe" => {
                let idx: usize = args
                    .next()
                    .ok_or_else(|| anyhow!("--bucket-probe SHAPE"))?
                    .parse()
                    .context("--bucket-probe SHAPE index")?;
                let report = celeste_rust::trace::kernel::bucket_probe(std::path::Path::new("."), idx)?;
                print!("{}", report);
                return Ok(());
            }
            // --level-minus-one S LEVEL_DIR CEILING FROM TO [MARKS]: the
            // position-only cost-to-go table for the start room
            // (plans/level-minus-one.md), with the player's speed in [-S, S]
            // px/frame, checked against LEVEL_DIR's recorded transitions and
            // compared with its frames FROM..=TO under CEILING (and against the
            // backward's MARKS file at that horizon). CELESTE_THREADS workers
            // (default 8).
            "--level-minus-one" => {
                let mut next = |what: &str| args.next().ok_or_else(|| anyhow!("--level-minus-one S LEVEL_DIR CEILING FROM TO: missing {what}"));
                let spd_px: i32 = next("S")?.parse().context("--level-minus-one S")?;
                let level_dir = std::path::PathBuf::from(next("LEVEL_DIR")?);
                let ceiling: u32 = next("CEILING")?.parse().context("--level-minus-one CEILING")?;
                let from: u32 = next("FROM")?.parse().context("--level-minus-one FROM")?;
                let to: u32 = next("TO")?.parse().context("--level-minus-one TO")?;
                let marks = args.next().map(std::path::PathBuf::from);
                let threads = std::env::var("CELESTE_THREADS").ok().map(|s| s.parse().context("CELESTE_THREADS")).transpose()?.unwrap_or(8);
                let opts = celeste_rust::trace::level_minus_one::Opts { spd_px, level_dir, ceiling, from, to, marks, threads };
                // The tracer recurses through the cart's AST: a big stack.
                let report = std::thread::Builder::new()
                    .stack_size(256 * 1024 * 1024)
                    .spawn(move || celeste_rust::trace::level_minus_one::probe(std::path::Path::new("."), &opts))
                    .context("spawn the level -1 probe")?
                    .join()
                    .map_err(|_| anyhow!("the level -1 probe panicked"))??;
                print!("{}", report);
                return Ok(());
            }
            "--room-consts" => {
                let report = celeste_rust::trace::kernel::room_constants(std::path::Path::new("."))?;
                print!("{}", report);
                return Ok(());
            }
            other => return Err(anyhow!("unknown argument {:?}", other)),
        }
    }

    if let Some(idx) = spec_probe {
        let parse_xy = |s: &str| -> (i16, i16) {
            let (a, b) = s.split_once(',').expect("x,y");
            (a.trim().parse().unwrap(), b.trim().parse().unwrap())
        };
        let player = std::env::var("CELESTE_SPEC_PLAYER")
            .ok()
            .map(|s| parse_xy(&s))
            .unwrap_or((40, 40));
        let springs: Vec<(i16, i16)> = std::env::var("CELESTE_SPEC_SPRINGS")
            .ok()
            .map(|s| s.split(';').map(parse_xy).collect())
            .unwrap_or_else(|| vec![(60, 40), (80, 40), (100, 40), (120, 40)]);
        let report = celeste_rust::trace::kernel::specialize_probe(
            std::path::Path::new("."),
            idx,
            player,
            &springs,
        )?;
        print!("{}", report);
        return Ok(());
    }

    Err(anyhow!("nothing to do - pass --room-consts or --spec-probe SHAPE"))
}
