//! CLI for the tracer's analysis probes (`celeste_rust::trace::kernel`).
//!
//!   transpile --room-consts            the reachable constant lattice report
//!   transpile --spec-probe SHAPE       specialization collapse for one shape
//!
//! The kernel emitter is gone: the kernels are assembled at runtime from the
//! fused graph (`compiled::asm_kernel`), so there is nothing to generate.

use anyhow::{anyhow, Context, Result};

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
