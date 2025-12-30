//! Low-overhead Chrome tracing profiler for CFG execution.
//!
//! This module provides minimal-overhead span-based profiling that produces
//! Chrome tracing JSON output. Unlike the full profiler, this only tracks
//! CFG execution spans with wall clock timing.
//!
//! Key design decisions for low overhead:
//! - Thread-local storage for per-thread spans (no mutex contention)
//! - Simple span structure with minimal metadata
//! - Lazy collection at the end of profiling
//! - Efficient pre-allocated buffers

use std::cell::RefCell;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::Instant;

use serde::Serialize;

// ============================================================================
// Configuration
// ============================================================================

/// Global flag for whether tracing is enabled
static TRACING_ENABLED: AtomicBool = AtomicBool::new(false);

/// Global start time (epoch) for computing relative timestamps.
/// Stored as nanos since some arbitrary point.
static TRACE_EPOCH_NANOS: AtomicU64 = AtomicU64::new(0);

/// Thread-local instant for the trace epoch
thread_local! {
    static LOCAL_EPOCH: RefCell<Option<Instant>> = const { RefCell::new(None) };
}

// ============================================================================
// Span Data
// ============================================================================

/// A lightweight trace span
#[derive(Debug, Clone)]
struct RawSpan {
    /// Name of the span (CFG name, function name)
    name: String,
    /// Category for grouping
    category: &'static str,
    /// Start time in microseconds from trace epoch
    start_us: u64,
    /// Duration in microseconds
    duration_us: u64,
    /// Thread ID
    tid: u64,
}

/// Chrome trace event format
#[derive(Debug, Serialize)]
struct ChromeTraceEvent {
    name: String,
    cat: &'static str,
    ph: &'static str,
    ts: u64,
    dur: u64,
    pid: u32,
    tid: u64,
}

// ============================================================================
// Thread-Local Span Collection
// ============================================================================

/// Per-thread span collection buffer
struct ThreadSpanBuffer {
    spans: Vec<RawSpan>,
    /// Stack of active span starts (for nesting)
    span_stack: Vec<(String, &'static str, Instant)>,
}

impl ThreadSpanBuffer {
    fn new() -> Self {
        Self {
            spans: Vec::with_capacity(10000),
            span_stack: Vec::with_capacity(64),
        }
    }

    #[inline]
    fn start_span(&mut self, name: String, category: &'static str) {
        self.span_stack.push((name, category, Instant::now()));
    }

    #[inline]
    fn end_span(&mut self, tid: u64, epoch: Instant) {
        if let Some((name, category, start)) = self.span_stack.pop() {
            let now = Instant::now();
            let start_us = start.duration_since(epoch).as_micros() as u64;
            let duration_us = now.duration_since(start).as_micros() as u64;

            self.spans.push(RawSpan {
                name,
                category,
                start_us,
                duration_us,
                tid,
            });
        }
    }
}

thread_local! {
    static THREAD_BUFFER: RefCell<ThreadSpanBuffer> = RefCell::new(ThreadSpanBuffer::new());
    static THREAD_ID: u64 = {
        // Get a unique thread ID
        static NEXT_THREAD_ID: AtomicU64 = AtomicU64::new(0);
        NEXT_THREAD_ID.fetch_add(1, Ordering::Relaxed)
    };
}

// ============================================================================
// Global Collection
// ============================================================================

use std::sync::Mutex;

lazy_static::lazy_static! {
    /// Collected spans from all threads
    static ref COLLECTED_SPANS: Mutex<Vec<RawSpan>> = Mutex::new(Vec::new());
    /// Global epoch instant (protected by mutex, only accessed during enable/collect)
    static ref EPOCH_INSTANT: Mutex<Option<Instant>> = Mutex::new(None);
}

// ============================================================================
// Public API
// ============================================================================

/// Enable tracing. Call this before starting execution.
pub fn enable_tracing() {
    let now = Instant::now();
    *EPOCH_INSTANT.lock().unwrap() = Some(now);
    LOCAL_EPOCH.with(|e| *e.borrow_mut() = Some(now));
    TRACING_ENABLED.store(true, Ordering::Release);
}

/// Check if tracing is enabled (cheap check)
#[inline]
pub fn is_tracing_enabled() -> bool {
    TRACING_ENABLED.load(Ordering::Acquire)
}

/// Reset tracing, clearing all collected data
pub fn reset_tracing() {
    let now = Instant::now();
    *EPOCH_INSTANT.lock().unwrap() = Some(now);
    LOCAL_EPOCH.with(|e| *e.borrow_mut() = Some(now));
    COLLECTED_SPANS.lock().unwrap().clear();
    THREAD_BUFFER.with(|buf| {
        let mut buf = buf.borrow_mut();
        buf.spans.clear();
        buf.span_stack.clear();
    });
}

/// Collect spans from the current thread's buffer into the global collection.
/// Call this at the end of execution on each thread.
pub fn collect_thread_spans() {
    if !is_tracing_enabled() {
        return;
    }

    THREAD_BUFFER.with(|buf| {
        let mut buf = buf.borrow_mut();
        if !buf.spans.is_empty() {
            let mut collected = COLLECTED_SPANS.lock().unwrap();
            collected.extend(buf.spans.drain(..));
        }
    });
}

/// Get all collected spans as Chrome tracing JSON
pub fn get_tracing_json() -> String {
    // First collect any remaining thread-local spans
    collect_thread_spans();

    let spans = COLLECTED_SPANS.lock().unwrap();
    let events: Vec<ChromeTraceEvent> = spans.iter().map(|span| {
        ChromeTraceEvent {
            name: span.name.clone(),
            cat: span.category,
            ph: "X", // Complete event
            ts: span.start_us,
            dur: span.duration_us,
            pid: 1,
            tid: span.tid,
        }
    }).collect();

    serde_json::to_string(&events).unwrap_or_else(|_| "[]".to_string())
}

// ============================================================================
// Span Guards
// ============================================================================

/// RAII guard for a trace span. Ends the span when dropped.
pub struct TraceSpan {
    active: bool,
}

impl TraceSpan {
    /// Start a new trace span
    #[inline]
    pub fn new(name: &str, category: &'static str) -> Self {
        if !is_tracing_enabled() {
            return Self { active: false };
        }

        THREAD_BUFFER.with(|buf| {
            buf.borrow_mut().start_span(name.to_string(), category);
        });

        Self { active: true }
    }

    /// Start a new trace span with an owned string (avoids allocation if already owned)
    #[inline]
    pub fn new_owned(name: String, category: &'static str) -> Self {
        if !is_tracing_enabled() {
            return Self { active: false };
        }

        THREAD_BUFFER.with(|buf| {
            buf.borrow_mut().start_span(name, category);
        });

        Self { active: true }
    }
}

impl Drop for TraceSpan {
    #[inline]
    fn drop(&mut self) {
        if !self.active {
            return;
        }

        let epoch = LOCAL_EPOCH.with(|e| {
            e.borrow().unwrap_or_else(|| {
                // Fallback: get from global epoch
                EPOCH_INSTANT.lock().unwrap().unwrap_or_else(Instant::now)
            })
        });

        let tid = THREAD_ID.with(|id| *id);

        THREAD_BUFFER.with(|buf| {
            buf.borrow_mut().end_span(tid, epoch);
        });
    }
}

// ============================================================================
// Convenience Functions
// ============================================================================

/// Create a trace span for CFG execution
#[inline]
pub fn trace_cfg(name: &str) -> TraceSpan {
    TraceSpan::new(name, "cfg")
}

/// Create a trace span for function calls
#[inline]
pub fn trace_call(name: &str) -> TraceSpan {
    TraceSpan::new(name, "call")
}

/// Create a trace span for function calls with owned name
#[inline]
pub fn trace_call_owned(name: String) -> TraceSpan {
    TraceSpan::new_owned(name, "call")
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;
    use std::time::Duration;

    #[test]
    fn test_tracing_disabled_by_default() {
        assert!(!is_tracing_enabled());
    }

    #[test]
    fn test_enable_tracing() {
        reset_tracing();
        enable_tracing();
        assert!(is_tracing_enabled());
    }

    #[test]
    fn test_span_collection() {
        reset_tracing();
        enable_tracing();

        {
            let _span = TraceSpan::new("test_span", "test");
            thread::sleep(Duration::from_millis(5));
        }

        collect_thread_spans();

        let json = get_tracing_json();
        assert!(json.contains("test_span"));
        assert!(json.contains("\"cat\":\"test\""));
    }

    #[test]
    fn test_nested_spans() {
        reset_tracing();
        enable_tracing();

        {
            let _outer = TraceSpan::new("outer", "test");
            {
                let _inner = TraceSpan::new("inner", "test");
                thread::sleep(Duration::from_millis(2));
            }
            thread::sleep(Duration::from_millis(2));
        }

        collect_thread_spans();

        let json = get_tracing_json();
        assert!(json.contains("outer"));
        assert!(json.contains("inner"));
    }

    #[test]
    fn test_disabled_no_overhead() {
        // Ensure tracing is disabled
        TRACING_ENABLED.store(false, Ordering::Release);

        // These should be no-ops
        {
            let _span = TraceSpan::new("should_not_appear", "test");
        }

        collect_thread_spans();

        let json = get_tracing_json();
        assert!(!json.contains("should_not_appear"));
    }
}
