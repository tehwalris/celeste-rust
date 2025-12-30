//! Module for capturing function inputs during interpretation for benchmarking.

use std::sync::Mutex;
use super::value::Value;
use serde::{Deserialize, Serialize};

/// A captured function call with its arguments
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CapturedCall {
    pub function_name: String,
    pub args: Vec<Value>,
}

/// Global state for input capture
struct CaptureState {
    enabled: bool,
    target_function: Option<String>,
    captured_calls: Vec<CapturedCall>,
}

static CAPTURE_STATE: Mutex<CaptureState> = Mutex::new(CaptureState {
    enabled: false,
    target_function: None,
    captured_calls: Vec::new(),
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
