//! THE EXTERNAL TOOLS, driven from here: a real PICO-8 (`pico8`, its chains
//! `chain`), UniversalClassicTas and Celia under LOVE (`love`), the
//! community's tasdatabase (`tasdb`). What they say is the verdict on a
//! witness; the game itself is this crate's.

pub mod category;
pub mod chain;
pub mod golden;
pub mod love;
pub mod pico8;
pub mod pyrandom;
pub mod tasdb;
pub mod uploads;

use anyhow::{Context, Result};
use std::path::PathBuf;
use std::process::{Child, Command, ExitStatus, Output, Stdio};
use std::time::{Duration, Instant};

/// A fresh scratch directory `$TMPDIR/NAME-PID-N` (the caller removes it).
pub fn scratch_dir(name: &str) -> Result<PathBuf> {
    static N: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    let n = N.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("{name}-{}-{n}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).with_context(|| format!("creating {}", dir.display()))?;
    Ok(dir)
}

/// `child` waited for; killed past `limit` (`None`).
fn wait(child: &mut Child, limit: Duration) -> Result<Option<ExitStatus>> {
    let start = Instant::now();
    loop {
        if let Some(s) = child.try_wait()? {
            return Ok(Some(s));
        }
        if start.elapsed() > limit {
            let _ = child.kill();
            let _ = child.wait();
            return Ok(None);
        }
        std::thread::sleep(Duration::from_millis(20));
    }
}

/// `cmd` run to its end, its output captured; `None` if it outlived
/// `limit` (then killed).
pub fn run_with_timeout(cmd: &mut Command, limit: Duration) -> Result<Option<Output>> {
    use std::io::Read;
    let what = format!("{cmd:?}");
    let mut child = cmd.stdin(Stdio::null()).stdout(Stdio::piped()).stderr(Stdio::piped()).spawn().with_context(|| format!("running {what}"))?;
    let pipe = |mut r: Box<dyn Read + Send>| {
        std::thread::spawn(move || {
            let mut v = Vec::new();
            let _ = r.read_to_end(&mut v);
            v
        })
    };
    let out = pipe(Box::new(child.stdout.take().expect("piped")));
    let err = pipe(Box::new(child.stderr.take().expect("piped")));
    let status = wait(&mut child, limit)?;
    let (stdout, stderr) = (out.join().expect("a reader"), err.join().expect("a reader"));
    Ok(status.map(|status| Output { status, stdout, stderr }))
}

/// `cmd` (its output already directed) run to its end; `None` if it
/// outlived `limit` (then killed).
pub fn wait_logged(cmd: &mut Command, limit: Duration) -> Result<Option<ExitStatus>> {
    let what = format!("{cmd:?}");
    let mut child = cmd.stdin(Stdio::null()).spawn().with_context(|| format!("running {what}"))?;
    wait(&mut child, limit)
}
