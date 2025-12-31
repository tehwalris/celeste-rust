//! Module for capturing function inputs during interpretation for benchmarking.

use std::sync::Mutex;
use super::value::Value;
use super::state::State;
use serde::{Deserialize, Serialize};

/// A captured function call with its arguments
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CapturedCall {
    pub function_name: String,
    pub args: Vec<Value>,
}

/// A captured slow function call with full state for benchmarking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CapturedSlowCall {
    pub function_name: String,
    pub args: Vec<Value>,
    pub state: State,
    pub duration_us: u64,
}

/// Global state for input capture
struct CaptureState {
    enabled: bool,
    target_function: Option<String>,
    captured_calls: Vec<CapturedCall>,
    slow_call: Option<CapturedSlowCall>,
    slow_call_target: Option<String>,
    slow_call_threshold_us: u64,
    slow_call_offset: u32,
    slow_call_skip_count: u32,
}

static CAPTURE_STATE: Mutex<CaptureState> = Mutex::new(CaptureState {
    enabled: false,
    target_function: None,
    captured_calls: Vec::new(),
    slow_call: None,
    slow_call_target: None,
    slow_call_threshold_us: 0,
    slow_call_offset: 0,
    slow_call_skip_count: 0,
});

/// Enable capturing calls to a specific function (by name prefix, e.g., "sign")
pub fn enable_capture(function_prefix: &str) {
    let mut state = CAPTURE_STATE.lock().unwrap();
    state.enabled = true;
    state.target_function = Some(function_prefix.to_string());
    state.captured_calls.clear();
}

/// Disable capture and return all captured calls
pub fn disable_capture_and_get() -> Vec<CapturedCall> {
    let mut state = CAPTURE_STATE.lock().unwrap();
    state.enabled = false;
    state.target_function = None;
    std::mem::take(&mut state.captured_calls)
}

/// Check if we should capture this function call and if so, capture it
pub fn maybe_capture(function_name: &str, args: Vec<Value>) {
    let mut state = CAPTURE_STATE.lock().unwrap();
    if !state.enabled {
        return;
    }

    if let Some(ref prefix) = state.target_function {
        if function_name.starts_with(prefix) {
            state.captured_calls.push(CapturedCall {
                function_name: function_name.to_string(),
                args,
            });
        }
    }
}

/// Get the count of captured calls without draining
pub fn captured_count() -> usize {
    CAPTURE_STATE.lock().unwrap().captured_calls.len()
}

/// Enable slow call capture: capture the first call to function_prefix that exceeds threshold_ms
/// If offset > 0, skip the first `offset` slow calls before capturing
pub fn enable_slow_call_capture(function_prefix: &str, threshold_ms: u64, offset: u32) {
    let mut state = CAPTURE_STATE.lock().unwrap();
    state.slow_call = None;
    state.slow_call_target = Some(function_prefix.to_string());
    state.slow_call_threshold_us = threshold_ms * 1000;
    state.slow_call_offset = offset;
    state.slow_call_skip_count = 0;
}

/// Check if slow call capture is enabled for this function
pub fn should_capture_slow_call(function_name: &str) -> bool {
    let state = CAPTURE_STATE.lock().unwrap();
    if state.slow_call.is_some() {
        return false; // Already captured one
    }
    if let Some(ref target) = state.slow_call_target {
        function_name.starts_with(target)
    } else {
        false
    }
}

/// Record a slow call if it exceeds the threshold
/// Respects the offset - skips the first `offset` slow calls
pub fn maybe_record_slow_call(
    function_name: &str,
    args: Vec<Value>,
    state_before: State,
    duration_us: u64,
) {
    let mut capture_state = CAPTURE_STATE.lock().unwrap();
    if capture_state.slow_call.is_some() {
        return; // Already captured one
    }
    if duration_us >= capture_state.slow_call_threshold_us {
        // Check if we should skip this one
        if capture_state.slow_call_skip_count < capture_state.slow_call_offset {
            capture_state.slow_call_skip_count += 1;
            eprintln!(
                "[input_capture] Skipping slow call #{} to {} ({:.1}ms)",
                capture_state.slow_call_skip_count,
                function_name,
                duration_us as f64 / 1000.0
            );
            return;
        }
        eprintln!(
            "[input_capture] Captured slow call to {} ({:.1}ms) after skipping {}",
            function_name,
            duration_us as f64 / 1000.0,
            capture_state.slow_call_offset
        );
        capture_state.slow_call = Some(CapturedSlowCall {
            function_name: function_name.to_string(),
            args,
            state: state_before,
            duration_us,
        });
    }
}

/// Get the captured slow call, if any
pub fn get_slow_call() -> Option<CapturedSlowCall> {
    CAPTURE_STATE.lock().unwrap().slow_call.clone()
}

/// Save the captured slow call to a file
pub fn save_slow_call(path: &str) -> std::io::Result<bool> {
    let slow_call = CAPTURE_STATE.lock().unwrap().slow_call.clone();
    if let Some(call) = slow_call {
        let json = serde_json::to_string(&call).map_err(|e| {
            std::io::Error::new(std::io::ErrorKind::Other, e)
        })?;
        // Compress with zstd
        let compressed = zstd::encode_all(json.as_bytes(), 3).map_err(|e| {
            std::io::Error::new(std::io::ErrorKind::Other, e)
        })?;
        std::fs::write(path, &compressed)?;
        eprintln!("[input_capture] Saved slow call to {} ({} bytes)", path, compressed.len());
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Load a captured slow call from a file
pub fn load_slow_call(path: &str) -> std::io::Result<CapturedSlowCall> {
    let compressed = std::fs::read(path)?;
    let json = zstd::decode_all(&compressed[..]).map_err(|e| {
        std::io::Error::new(std::io::ErrorKind::Other, e)
    })?;
    let call: CapturedSlowCall = serde_json::from_slice(&json).map_err(|e| {
        std::io::Error::new(std::io::ErrorKind::Other, e)
    })?;
    Ok(call)
}
