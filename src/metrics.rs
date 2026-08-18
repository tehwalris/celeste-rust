//! Always-on coarse phase metrics.
//!
//! `--profile` span tracing costs a few percent because it instruments hot
//! interpreter paths; this module instruments only PHASE boundaries (a
//! handful of `record` calls per frame or per sweep stage), so the overhead
//! is nanoseconds and it can stay on unconditionally. Every driver run
//! prints a phase summary at exit and appends one JSON line to
//! `metrics.jsonl` next to the checkpoints (when a checkpoint dir is known),
//! giving a machine-readable history of where wall time went - the
//! "forward vs backward vs merge vs IO" questions that previously required
//! log archaeology.

use std::collections::BTreeMap;
use std::sync::Mutex;
use std::time::Duration;

static PHASES: Mutex<BTreeMap<&'static str, (u64, u64)>> = Mutex::new(BTreeMap::new());

/// Add `dur` under `name`. Call at phase granularity only (per frame, per
/// sweep stage) - never inside per-lane or per-instruction loops.
pub fn record(name: &'static str, dur: Duration) {
    let mut phases = PHASES.lock().unwrap();
    let entry = phases.entry(name).or_insert((0, 0));
    entry.0 += dur.as_micros() as u64;
    entry.1 += 1;
}

/// Time a closure under `name`.
pub fn time<T>(name: &'static str, f: impl FnOnce() -> T) -> T {
    let t = std::time::Instant::now();
    let out = f();
    record(name, t.elapsed());
    out
}

/// Print the phase summary and, when `dir` is known, append one JSON line
/// to `<dir>/metrics.jsonl`: {"kind", "phases": {name: {"s", "calls"}},
/// "extra": ...}. Failures to write are loud on stderr but never fatal -
/// metrics must not kill a run that already computed its answer.
pub fn dump(kind: &str, dir: Option<&std::path::Path>, extra: &[(&str, String)]) {
    // The compiled frame body's own split, when it was asked for. It sits
    // INSIDE `fwd.interpret`, so it belongs next to the phase totals rather
    // than in them.
    crate::compiled::print_chunk_phase_times();
    crate::rewrite::verify::print_worker_phase_times();
    let phases = PHASES.lock().unwrap();
    if phases.is_empty() {
        return;
    }
    let mut rows: Vec<_> = phases.iter().collect();
    rows.sort_by_key(|(_, (us, _))| std::cmp::Reverse(*us));
    println!("phase totals ({}):", kind);
    for (name, (us, calls)) in &rows {
        println!(
            "  {:<28} {:>9.2}s {:>8} calls",
            name,
            *us as f64 / 1e6,
            calls
        );
    }
    let Some(dir) = dir else { return };
    let mut obj = serde_json::Map::new();
    obj.insert("kind".into(), serde_json::Value::String(kind.to_string()));
    let mut phase_obj = serde_json::Map::new();
    for (name, (us, calls)) in &rows {
        phase_obj.insert(
            name.to_string(),
            serde_json::json!({"s": *us as f64 / 1e6, "calls": calls}),
        );
    }
    obj.insert("phases".into(), serde_json::Value::Object(phase_obj));
    for (k, v) in extra {
        obj.insert(k.to_string(), serde_json::Value::String(v.clone()));
    }
    let line = serde_json::Value::Object(obj).to_string();
    let path = dir.join("metrics.jsonl");
    let result = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
        .and_then(|mut f| {
            use std::io::Write;
            writeln!(f, "{}", line)
        });
    if let Err(e) = result {
        eprintln!("metrics: could not append to {}: {}", path.display(), e);
    }
}
