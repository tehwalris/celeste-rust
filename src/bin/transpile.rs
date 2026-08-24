//! CLI for the kernel emitter (`celeste_rust::transpile`).
//!
//!   transpile --room-kernels DIR              the TRACED per-shape set
//!
//! The default target is the checked-in generated crate,
//! `crates/celeste-kernels/src/traced`. The canonical regen is
//! `./regen-generated.sh`.

use anyhow::{anyhow, Context, Result};

fn main() -> Result<()> {
    let mut room_kernels: Option<String> = None;
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
            other => return Err(anyhow!("unknown argument {:?}", other)),
        }
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
