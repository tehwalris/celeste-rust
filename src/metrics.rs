//! The process's memory: its resident set, its peak, the live heap
//! (`Counting`) and the `[mem]` line at a phase boundary.

use std::time::Duration;

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

/// The highest LIVE heap (in KB, `live_heap_gb`) sampled since the last
/// `mem_phase`.
static PHASE_PEAK_LIVE_KB: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// A global allocator that COUNTS the bytes its allocations hold (the
/// binaries wrap mimalloc in it): the LIVE heap, against which the resident
/// set shows what the allocator keeps besides (freed pages not returned,
/// fragmentation). Per thread a shard of the count (a cache line each, no
/// sharing on the hot path); `live_heap_gb` sums them.
pub struct Counting<A>(pub A);

#[repr(align(64))]
struct Shard(std::sync::atomic::AtomicI64);

const SHARDS: usize = 64;
static LIVE: [Shard; SHARDS] = [const { Shard(std::sync::atomic::AtomicI64::new(0)) }; SHARDS];
static NEXT_SHARD: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

thread_local! {
    /// This thread's shard (`usize::MAX`: not yet given one). Const and
    /// without a destructor: safe to touch inside the allocator.
    static SHARD: std::cell::Cell<usize> = const { std::cell::Cell::new(usize::MAX) };
}

#[inline]
fn count(delta: i64) {
    use std::sync::atomic::Ordering::Relaxed;
    // A thread whose locals are gone (exiting) counts in shard 0.
    let k = SHARD
        .try_with(|c| {
            if c.get() == usize::MAX {
                c.set(NEXT_SHARD.fetch_add(1, Relaxed) % SHARDS);
            }
            c.get()
        })
        .unwrap_or(0);
    LIVE[k].0.fetch_add(delta, Relaxed);
}

// SAFETY: every call is forwarded to the wrapped allocator unchanged; the
// counting touches no allocation.
unsafe impl<A: std::alloc::GlobalAlloc> std::alloc::GlobalAlloc for Counting<A> {
    #[inline]
    unsafe fn alloc(&self, l: std::alloc::Layout) -> *mut u8 {
        let p = unsafe { self.0.alloc(l) };
        if !p.is_null() {
            count(l.size() as i64);
        }
        p
    }
    #[inline]
    unsafe fn alloc_zeroed(&self, l: std::alloc::Layout) -> *mut u8 {
        let p = unsafe { self.0.alloc_zeroed(l) };
        if !p.is_null() {
            count(l.size() as i64);
        }
        p
    }
    #[inline]
    unsafe fn dealloc(&self, p: *mut u8, l: std::alloc::Layout) {
        unsafe { self.0.dealloc(p, l) };
        count(-(l.size() as i64));
    }
    #[inline]
    unsafe fn realloc(&self, p: *mut u8, l: std::alloc::Layout, new: usize) -> *mut u8 {
        let q = unsafe { self.0.realloc(p, l, new) };
        if !q.is_null() {
            count(new as i64 - l.size() as i64);
        }
        q
    }
}

/// The LIVE heap (`Counting`), in GB: the bytes the process's allocations
/// hold now (0 in a binary without the counting allocator).
pub fn live_heap_gb() -> f64 {
    LIVE.iter().map(|s| s.0.load(std::sync::atomic::Ordering::Relaxed)).sum::<i64>() as f64 / 1e9
}

/// One line of memory at a phase boundary: the anonymous resident set now,
/// its highest value since the previous call (a sampler thread reads it every
/// 50 ms, started by the first call), the file-backed pages now, and
/// `VmHWM`; and the live heap now and its highest sampled value
/// (`Counting`). `[mem] TAG: anon A GB (phase peak P) live heap L GB (phase
/// peak Q) file F GB, peak H GB`.
pub fn mem_phase(tag: &str) {
    use std::sync::atomic::Ordering::Relaxed;
    static SAMPLER: std::sync::Once = std::sync::Once::new();
    SAMPLER.call_once(|| {
        std::thread::spawn(|| loop {
            PHASE_PEAK_KB.fetch_max((current_rss_gb() * 1e6) as u64, Relaxed);
            PHASE_PEAK_LIVE_KB.fetch_max((live_heap_gb().max(0.0) * 1e6) as u64, Relaxed);
            std::thread::sleep(Duration::from_millis(50));
        });
    });
    let (now, live) = (current_rss_gb(), live_heap_gb());
    let peak = PHASE_PEAK_KB.swap((now * 1e6) as u64, Relaxed) as f64 / 1e6;
    let peak_live = PHASE_PEAK_LIVE_KB.swap((live.max(0.0) * 1e6) as u64, Relaxed) as f64 / 1e6;
    eprintln!(
        "[mem] {tag}: anon {now:.2} GB (phase peak {:.2}) live heap {live:.2} GB (phase peak {:.2}) file {:.2} GB, peak {:.2} GB",
        peak.max(now),
        peak_live.max(live),
        current_file_rss_gb(),
        peak_rss_gb()
    );
}
