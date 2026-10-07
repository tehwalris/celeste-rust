//! Always-on coarse phase metrics: wall time per named phase, recorded only
//! at phase boundaries so the overhead is negligible. A run prints the
//! totals at exit and appends one JSON line to `metrics.jsonl` next to the
//! checkpoints.

use std::collections::BTreeMap;
use std::sync::Mutex;
use std::time::Duration;

static PHASES: Mutex<BTreeMap<&'static str, (u64, u64)>> = Mutex::new(BTreeMap::new());

/// Add `dur` under `name`. Call at phase granularity only, never in
/// per-lane loops.
pub fn record(name: &'static str, dur: Duration) {
    let mut phases = PHASES.lock().unwrap();
    let entry = phases.entry(name).or_insert((0, 0));
    entry.0 += dur.as_micros() as u64;
    entry.1 += 1;
}

fn status_gb(field: &str) -> f64 {
    std::fs::read_to_string("/proc/self/status")
        .ok()
        .and_then(|s| {
            s.lines()
                .find(|l| l.starts_with(field))
                .and_then(|l| l.split_whitespace().nth(1)?.parse::<f64>().ok())
        })
        .map_or(0.0, |kb| kb / 1e6)
}

/// The process's current ANONYMOUS resident set (`RssAnon`), in GB: the
/// heap, what a memory cap is about. Not `VmRSS`, which counts mmapped files
/// the kernel reclaims first.
pub fn current_rss_gb() -> f64 {
    status_gb("RssAnon:")
}

/// The process's file-backed resident pages (`RssFile`), in GB.
pub fn current_file_rss_gb() -> f64 {
    status_gb("RssFile:")
}

/// This process's peak resident set (`VmHWM`, anonymous + file), in GB.

pub fn peak_rss_gb() -> f64 {
    status_gb("VmHWM:")
}

/// The highest `RssAnon` (in KB) sampled since the last `mem_phase`.
static PHASE_PEAK_KB: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// One line of memory at a phase boundary: the anonymous resident set now,
/// its highest value since the previous call (a sampler thread reads it every
/// 50 ms, started by the first call), the file-backed pages now, and
/// `VmHWM`. `[mem] TAG: anon A GB (phase peak P) file F GB, peak H GB`.
pub fn mem_phase(tag: &str) {
    use std::sync::atomic::Ordering::Relaxed;
    static SAMPLER: std::sync::Once = std::sync::Once::new();
    SAMPLER.call_once(|| {
        std::thread::spawn(|| loop {
            PHASE_PEAK_KB.fetch_max((current_rss_gb() * 1e6) as u64, Relaxed);
            std::thread::sleep(Duration::from_millis(50));
        });
    });
    let now = current_rss_gb();
    let peak = PHASE_PEAK_KB.swap((now * 1e6) as u64, Relaxed) as f64 / 1e6;
    eprintln!(
        "[mem] {tag}: anon {now:.2} GB (phase peak {:.2}) file {:.2} GB, peak {:.2} GB",
        peak.max(now),
        current_file_rss_gb(),
        peak_rss_gb()
    );
}

/// Print the phase summary and, when `dir` is known, append one JSON line
/// to `<dir>/metrics.jsonl`: {"kind", "phases": {name: {"s", "calls"}},
/// "extra": ...}. A write failure is reported, never fatal.
pub fn dump(kind: &str, dir: Option<&std::path::Path>, extra: &[(&str, String)]) {
    // Kernel coverage, when a compiled run was in play.
    crate::compiled::dispatch::print_kernel_hits();
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
