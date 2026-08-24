//! CLI for the kernel emitters (`celeste_rust::transpile`).
//!
//!   transpile [--recipe R] --kernel W OUT     class kernel from witness W
//!   transpile --room-kernels DIR              the TRACED per-shape set
//!
//! Defaults land in the checked-in generated crate. The canonical regen is
//! `./regen-generated.sh`.

use anyhow::{anyhow, Context, Result};
use celeste_rust::transpile::kernel;

fn main() -> Result<()> {
    let mut recipe_path: Option<String> = None;
    let mut kernel_out: Option<(String, String)> = None;
    let mut room_kernels: Option<String> = None;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            // --room-kernels DIR: the TRACED kernel set - one module per
            // heap shape the start room reaches, plus the `KERNELS`
            // table a dispatcher indexes. Nothing to do with `--recipe`
            // or a witness: the tracer walks the room itself.
            "--room-kernels" => {
                room_kernels =
                    Some(args.next().ok_or_else(|| anyhow!("--room-kernels DIR"))?);
            }
            "--kernel" => {
                let witness = args.next().ok_or_else(|| anyhow!("--kernel WITNESS OUT"))?;
                let out = args.next().ok_or_else(|| anyhow!("--kernel WITNESS OUT"))?;
                kernel_out = Some((witness, out));
            }
            "--recipe" => {
                recipe_path =
                    Some(args.next().ok_or_else(|| anyhow!("--recipe needs a path"))?);
            }
            other => return Err(anyhow!("unknown argument {:?}", other)),
        }
    }

    if let Some(dir) = room_kernels {
        let dir = std::path::Path::new(&dir);
        let sizes = celeste_rust::trace::kernel::write_room_kernels(
            std::path::Path::new("."),
            dir,
        )
        .with_context(|| format!("write the room kernel set to {}", dir.display()))?;
        eprintln!(
            "wrote {} shapes -> {:?} lines, {} total, in {}",
            sizes.len(),
            sizes,
            sizes.iter().sum::<usize>(),
            dir.display()
        );
        return Ok(());
    }

    let Some((witness, out)) = kernel_out else {
        return Err(anyhow!("nothing to do - pass --room-kernels DIR or --kernel WITNESS OUT"));
    };
    let recipe = recipe_path.ok_or_else(|| anyhow!("--kernel needs --recipe"))?;
    let program = celeste_rust::program::frozen::rewritten(&recipe)
        .with_context(|| format!("apply {} (run from the repo root)", recipe))?;
    kernel::emit_kernel(&program, &witness, &out)
}
