//! CLI for the kernel emitter (`celeste_rust::transpile`).
//!
//!   transpile --room-kernels DIR              the TRACED per-shape set
//!
//! The default target is a per-room generated crate,
//! `crates/celeste-kernels-room<xy>/src/traced`. The canonical regen is
//! `./regen-generated.sh`.

use anyhow::{anyhow, Context, Result};

fn main() -> Result<()> {
    let mut room_kernels: Option<String> = None;
    let mut spec_probe: Option<usize> = None;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            // --room-kernels DIR: the TRACED kernel set - one module per
            // heap shape the start room reaches, plus the `KERNELS`
            // table a dispatcher indexes. The tracer walks the room
            // itself: no recipe, no witness.
            "--room-kernels" => {
                room_kernels =
                    Some(args.next().ok_or_else(|| anyhow!("--room-kernels DIR"))?);
            }
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
            // --room-kernels-ladder DIR: the RUNG-AGNOSTIC set
            // (plans/kernel-ladder.md) - the same walk with the boundary
            // widenings left OUT of the graph, so the campaign boundary
            // applies whichever precision rung is configured. Target:
            // crates/celeste-kernels-room<xy>/src/ladder.
            "--room-kernels-ladder" => {
                let d = args.next().ok_or_else(|| anyhow!("--room-kernels-ladder DIR"))?;
                let sizes = celeste_rust::trace::kernel::write_room_kernels_ladder(
                    std::path::Path::new("."),
                    std::path::Path::new(&d),
                )?;
                eprintln!(
                    "ladder: {} kernels -> {:?} lines, {} total",
                    sizes.len(),
                    sizes,
                    sizes.iter().sum::<usize>()
                );
                return Ok(());
            }
            // --room-kernels-exact DIR: the EXACT-REM set for the
            // ladder's top rung (k = 16) - interval slots as plain
            // numbers, no rem forks. Target:
            // crates/celeste-kernels-room<xy>/src/exact.
            "--room-kernels-exact" => {
                let d = args.next().ok_or_else(|| anyhow!("--room-kernels-exact DIR"))?;
                let sizes = celeste_rust::trace::kernel::write_room_kernels_exact(
                    std::path::Path::new("."),
                    std::path::Path::new(&d),
                )?;
                eprintln!(
                    "exact: {} kernels -> {:?} lines, {} total",
                    sizes.len(),
                    sizes,
                    sizes.iter().sum::<usize>()
                );
                return Ok(());
            }
            // --merge-kernels SET OUT_DIR ROOM_DIR...: write
            // OUT_DIR/mod.rs (the AGGREGATOR, `crates/celeste-kernels`'s
            // side) from one or more ROOM_DIRs, each a directory
            // holding exactly one room*/ subdirectory the generators
            // above produced (a room crate's `src/SET`, or a matching
            // scratch dir). Rooms are generated one process each
            // (CELESTE_START_ROOM feeds a OnceLock); this is the
            // file-level step that puts them all into one dispatch
            // registry (`SETS`).
            "--merge-kernels" => {
                let set = args.next().ok_or_else(|| {
                    anyhow!("--merge-kernels SET OUT_DIR ROOM_DIR...")
                })?;
                let out = args.next().ok_or_else(|| {
                    anyhow!("--merge-kernels SET OUT_DIR ROOM_DIR...")
                })?;
                let room_dirs: Vec<std::path::PathBuf> =
                    args.by_ref().map(std::path::PathBuf::from).collect();
                anyhow::ensure!(
                    !room_dirs.is_empty(),
                    "--merge-kernels SET OUT_DIR ROOM_DIR...: need at least one ROOM_DIR"
                );
                let rooms = celeste_rust::trace::kernel::merge_kernel_sets(
                    &set,
                    std::path::Path::new(&out),
                    &room_dirs,
                )?;
                eprintln!("merged {} rooms: {:?} into {}", rooms.len(), rooms, out);
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

    let Some(dir) = room_kernels else {
        return Err(anyhow!("nothing to do - pass --room-kernels DIR"));
    };
    let dir = std::path::Path::new(&dir);
    let sizes = celeste_rust::trace::kernel::write_room_kernels(std::path::Path::new("."), dir)
        .with_context(|| format!("write the room kernel set to {}", dir.display()))?;
    eprintln!(
        "wrote {} shapes -> {:?} lines, {} total, in {}",
        sizes.len(),
        sizes,
        sizes.iter().sum::<usize>(),
        dir.display()
    );
    Ok(())
}
