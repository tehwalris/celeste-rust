//! Profiling infrastructure for the abstract interpreter.
//!
//! Three kinds of profiling:
//! 1. State Flow DAG - tracks how states split and merge during execution
//! 2. Fixed-Point Tree - tracks recursion through function calls
//! 3. Chrome Tracing - wall-clock span-based profiling

use std::cell::RefCell;
use std::time::{Duration, Instant};

use serde::Serialize;

// ============================================================================
// State Flow DAG
// ============================================================================

/// A node in the state flow DAG representing a set of states at a point in execution
#[derive(Debug, Clone, Serialize)]
pub struct DagNode {
    pub id: u64,
    /// Number of State objects at this node
    pub state_count: usize,
    /// Total vector_size across all states
    pub expanded_count: usize,
    /// What operation produced this node
    pub operation: DagOperation,
    /// Time spent processing states at this node (not including children)
    pub processing_time: Duration,
    /// Number of instructions executed at this node
    pub instruction_count: usize,
    /// Number of function calls made at this node
    pub call_count: usize,
    /// Parent node ID (if any)
    pub parent_id: Option<u64>,
    /// IDs of nodes that were merged to form this node (for join operations)
    pub merged_from: Vec<u64>,
    /// Fixed-point tree node ID that contains this DAG node
    pub tree_node_id: Option<u64>,
}

/// What operation created a DAG node
#[derive(Debug, Clone, Serialize)]
pub enum DagOperation {
    /// Initial state(s) entering a fixed-point
    Entry,
    /// States after executing a block
    BlockExecution { block_name: Option<String> },
    /// States after a conditional branch (includes which branch)
    ConditionalSplit { branch: bool },
    /// States after a builtin function call that returned multiple states
    BuiltinSplit { builtin_name: String },
    /// States after a closure call that returned multiple states
    ClosureSplit { function_name: String },
    /// States merged via vectorization
    Vectorization,
    /// States joined at a block entry
    Join,
}

// ============================================================================
// Fixed-Point Recursion Tree
// ============================================================================

/// A node in the fixed-point recursion tree
#[derive(Debug, Clone, Serialize)]
pub struct TreeNode {
    pub id: u64,
    /// Name of the function/CFG being executed (None for top-level)
    pub name: Option<String>,
    /// Parent tree node ID
    pub parent_id: Option<u64>,
    /// Depth in the tree (0 = root)
    pub depth: usize,
    /// Time spent in this fixed-point (including children)
    pub total_time: Duration,
    /// Time spent in this fixed-point (excluding children)
    pub self_time: Duration,
    /// Number of iterations in the fixed-point loop
    pub iterations: usize,
    /// Number of states processed
    pub states_processed: usize,
    /// Number of blocks executed
    pub blocks_executed: usize,
    /// Child tree node IDs
    pub children: Vec<u64>,
    /// DAG node IDs that belong to this tree node
    pub dag_nodes: Vec<u64>,
}

// ============================================================================
// Chrome Tracing Spans
// ============================================================================

/// A span for Chrome tracing output
#[derive(Debug, Clone, Serialize)]
pub struct Span {
    /// Name of the span
    pub name: String,
    /// Category for grouping in the trace viewer
    pub category: String,
    /// Start time in microseconds from profiling start
    pub start_us: u64,
    /// Duration in microseconds
    pub duration_us: u64,
    /// Thread ID (always 0 for now since we're single-threaded)
    pub tid: u32,
    /// Associated DAG node ID (if any)
    pub dag_node_id: Option<u64>,
    /// Associated tree node ID (if any)
    pub tree_node_id: Option<u64>,
    /// Additional metadata
    pub args: std::collections::HashMap<String, String>,
}

// ============================================================================
// Profiler State
// ============================================================================

/// The main profiler that collects all profiling data
#[derive(Debug)]
pub struct Profiler {
    /// Whether profiling is enabled
    enabled: bool,
    /// Start time for computing relative timestamps
    start_time: Instant,
    /// Next ID for DAG nodes
    next_dag_id: u64,
    /// Next ID for tree nodes
    next_tree_id: u64,
    /// All DAG nodes
    dag_nodes: Vec<DagNode>,
    /// All tree nodes
    tree_nodes: Vec<TreeNode>,
    /// All spans
    spans: Vec<Span>,
    /// Stack of active tree node IDs (for tracking nesting)
    tree_stack: Vec<u64>,
    /// Stack of active span start times (for computing durations)
    span_stack: Vec<(String, String, Instant, Option<u64>, Option<u64>)>,
    /// Current DAG node being processed (if any)
    current_dag_node: Option<u64>,
}

impl Default for Profiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Profiler {
    pub fn new() -> Self {
        Self {
            enabled: false,
            start_time: Instant::now(),
            next_dag_id: 0,
            next_tree_id: 0,
            dag_nodes: Vec::new(),
            tree_nodes: Vec::new(),
            spans: Vec::new(),
            tree_stack: Vec::new(),
            span_stack: Vec::new(),
            current_dag_node: None,
        }
    }

    pub fn enable(&mut self) {
        self.enabled = true;
        self.start_time = Instant::now();
    }

    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Reset all profiling data
    pub fn reset(&mut self) {
        self.start_time = Instant::now();
        self.next_dag_id = 0;
        self.next_tree_id = 0;
        self.dag_nodes.clear();
        self.tree_nodes.clear();
        self.spans.clear();
        self.tree_stack.clear();
        self.span_stack.clear();
        self.current_dag_node = None;
    }

    // ========================================================================
    // DAG Operations
    // ========================================================================

    /// Create a new DAG node
    pub fn create_dag_node(
        &mut self,
        state_count: usize,
        expanded_count: usize,
        operation: DagOperation,
        parent_id: Option<u64>,
    ) -> u64 {
        if !self.enabled {
            return 0;
        }
        let id = self.next_dag_id;
        self.next_dag_id += 1;
        let tree_node_id = self.tree_stack.last().copied();

        let node = DagNode {
            id,
            state_count,
            expanded_count,
            operation,
            processing_time: Duration::ZERO,
            instruction_count: 0,
            call_count: 0,
            parent_id,
            merged_from: Vec::new(),
            tree_node_id,
        };
        self.dag_nodes.push(node);

        // Also record in current tree node
        if let Some(tree_id) = tree_node_id {
            if let Some(tree_node) = self.tree_nodes.iter_mut().find(|n| n.id == tree_id) {
                tree_node.dag_nodes.push(id);
            }
        }

        id
    }

    /// Create a DAG node representing a merge/join of multiple nodes
    pub fn create_merged_dag_node(
        &mut self,
        state_count: usize,
        expanded_count: usize,
        operation: DagOperation,
        merged_from: Vec<u64>,
    ) -> u64 {
        if !self.enabled {
            return 0;
        }
        let id = self.next_dag_id;
        self.next_dag_id += 1;
        let tree_node_id = self.tree_stack.last().copied();

        let node = DagNode {
            id,
            state_count,
            expanded_count,
            operation,
            processing_time: Duration::ZERO,
            instruction_count: 0,
            call_count: 0,
            parent_id: None,
            merged_from,
            tree_node_id,
        };
        self.dag_nodes.push(node);

        if let Some(tree_id) = tree_node_id {
            if let Some(tree_node) = self.tree_nodes.iter_mut().find(|n| n.id == tree_id) {
                tree_node.dag_nodes.push(id);
            }
        }

        id
    }

    /// Update a DAG node's stats
    pub fn update_dag_node(
        &mut self,
        id: u64,
        processing_time: Duration,
        instruction_count: usize,
        call_count: usize,
    ) {
        if !self.enabled {
            return;
        }
        if let Some(node) = self.dag_nodes.iter_mut().find(|n| n.id == id) {
            node.processing_time = processing_time;
            node.instruction_count = instruction_count;
            node.call_count = call_count;
        }
    }

    pub fn set_current_dag_node(&mut self, id: Option<u64>) {
        self.current_dag_node = id;
    }

    pub fn current_dag_node(&self) -> Option<u64> {
        self.current_dag_node
    }

    // ========================================================================
    // Tree Operations
    // ========================================================================

    /// Enter a new fixed-point (push onto tree stack)
    pub fn enter_fixed_point(&mut self, name: Option<String>) -> u64 {
        if !self.enabled {
            return 0;
        }
        let id = self.next_tree_id;
        self.next_tree_id += 1;

        let parent_id = self.tree_stack.last().copied();
        let depth = self.tree_stack.len();

        let node = TreeNode {
            id,
            name,
            parent_id,
            depth,
            total_time: Duration::ZERO,
            self_time: Duration::ZERO,
            iterations: 0,
            states_processed: 0,
            blocks_executed: 0,
            children: Vec::new(),
            dag_nodes: Vec::new(),
        };
        self.tree_nodes.push(node);

        // Add as child of parent
        if let Some(parent_id) = parent_id {
            if let Some(parent) = self.tree_nodes.iter_mut().find(|n| n.id == parent_id) {
                parent.children.push(id);
            }
        }

        self.tree_stack.push(id);
        id
    }

    /// Exit a fixed-point (pop from tree stack)
    pub fn exit_fixed_point(&mut self, total_time: Duration, child_time: Duration) {
        if !self.enabled {
            return;
        }
        if let Some(id) = self.tree_stack.pop() {
            if let Some(node) = self.tree_nodes.iter_mut().find(|n| n.id == id) {
                node.total_time = total_time;
                node.self_time = total_time.saturating_sub(child_time);
            }
        }
    }

    /// Update tree node stats
    pub fn update_tree_node(&mut self, id: u64, iterations: usize, states_processed: usize, blocks_executed: usize) {
        if !self.enabled {
            return;
        }
        if let Some(node) = self.tree_nodes.iter_mut().find(|n| n.id == id) {
            node.iterations = iterations;
            node.states_processed = states_processed;
            node.blocks_executed = blocks_executed;
        }
    }

    pub fn current_tree_node(&self) -> Option<u64> {
        self.tree_stack.last().copied()
    }

    // ========================================================================
    // Span Operations
    // ========================================================================

    /// Start a new span
    pub fn start_span(&mut self, name: &str, category: &str) {
        if !self.enabled {
            return;
        }
        let dag_node_id = self.current_dag_node;
        let tree_node_id = self.tree_stack.last().copied();
        self.span_stack.push((
            name.to_string(),
            category.to_string(),
            Instant::now(),
            dag_node_id,
            tree_node_id,
        ));
    }

    /// End the current span
    pub fn end_span(&mut self) {
        self.end_span_with_args(std::collections::HashMap::new());
    }

    /// End the current span with additional metadata
    pub fn end_span_with_args(&mut self, args: std::collections::HashMap<String, String>) {
        if !self.enabled {
            return;
        }
        if let Some((name, category, start, dag_node_id, tree_node_id)) = self.span_stack.pop() {
            let end = Instant::now();
            let duration = end.duration_since(start);
            let start_us = start.duration_since(self.start_time).as_micros() as u64;
            let duration_us = duration.as_micros() as u64;

            self.spans.push(Span {
                name,
                category,
                start_us,
                duration_us,
                tid: 0,
                dag_node_id,
                tree_node_id,
                args,
            });
        }
    }

    // ========================================================================
    // Output
    // ========================================================================

    /// Get all DAG nodes
    pub fn dag_nodes(&self) -> &[DagNode] {
        &self.dag_nodes
    }

    /// Get all tree nodes
    pub fn tree_nodes(&self) -> &[TreeNode] {
        &self.tree_nodes
    }

    /// Get all spans
    pub fn spans(&self) -> &[Span] {
        &self.spans
    }

    /// Export to Chrome tracing JSON format
    pub fn to_chrome_tracing_json(&self) -> String {
        let events: Vec<ChromeTraceEvent> = self.spans.iter().map(|span| {
            let mut args = span.args.clone();
            if let Some(dag_id) = span.dag_node_id {
                args.insert("dag_node_id".to_string(), dag_id.to_string());
            }
            if let Some(tree_id) = span.tree_node_id {
                args.insert("tree_node_id".to_string(), tree_id.to_string());
            }

            ChromeTraceEvent {
                name: span.name.clone(),
                cat: span.category.clone(),
                ph: "X".to_string(), // Complete event
                ts: span.start_us,
                dur: span.duration_us,
                pid: 1,
                tid: span.tid,
                args: if args.is_empty() { None } else { Some(args) },
            }
        }).collect();

        serde_json::to_string_pretty(&events).unwrap_or_else(|_| "[]".to_string())
    }

    /// Export DAG to JSON
    pub fn dag_to_json(&self) -> String {
        serde_json::to_string_pretty(&self.dag_nodes).unwrap_or_else(|_| "[]".to_string())
    }

    /// Export tree to JSON
    pub fn tree_to_json(&self) -> String {
        serde_json::to_string_pretty(&self.tree_nodes).unwrap_or_else(|_| "[]".to_string())
    }

    /// Get summary statistics
    pub fn summary(&self) -> ProfileSummary {
        let total_dag_nodes = self.dag_nodes.len();
        let total_tree_nodes = self.tree_nodes.len();
        let max_tree_depth = self.tree_nodes.iter().map(|n| n.depth).max().unwrap_or(0);

        let total_processing_time: Duration = self.dag_nodes.iter()
            .map(|n| n.processing_time)
            .sum();

        let total_self_time: Duration = self.tree_nodes.iter()
            .map(|n| n.self_time)
            .sum();

        // Count splits by type
        let mut builtin_splits = 0;
        let mut closure_splits = 0;
        let mut conditional_splits = 0;
        let mut vectorizations = 0;

        for node in &self.dag_nodes {
            match &node.operation {
                DagOperation::BuiltinSplit { .. } => builtin_splits += 1,
                DagOperation::ClosureSplit { .. } => closure_splits += 1,
                DagOperation::ConditionalSplit { .. } => conditional_splits += 1,
                DagOperation::Vectorization => vectorizations += 1,
                _ => {}
            }
        }

        ProfileSummary {
            total_dag_nodes,
            total_tree_nodes,
            max_tree_depth,
            total_processing_time,
            total_self_time,
            builtin_splits,
            closure_splits,
            conditional_splits,
            vectorizations,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ProfileSummary {
    pub total_dag_nodes: usize,
    pub total_tree_nodes: usize,
    pub max_tree_depth: usize,
    pub total_processing_time: Duration,
    pub total_self_time: Duration,
    pub builtin_splits: usize,
    pub closure_splits: usize,
    pub conditional_splits: usize,
    pub vectorizations: usize,
}

/// Chrome trace event format
#[derive(Debug, Serialize)]
struct ChromeTraceEvent {
    name: String,
    cat: String,
    ph: String,
    ts: u64,
    dur: u64,
    pid: u32,
    tid: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    args: Option<std::collections::HashMap<String, String>>,
}

// ============================================================================
// Thread-Local Profiler
// ============================================================================

thread_local! {
    static PROFILER: RefCell<Profiler> = RefCell::new(Profiler::new());
}

/// Enable profiling for the current thread
pub fn enable_profiling() {
    PROFILER.with(|p| p.borrow_mut().enable());
}

/// Check if profiling is enabled
pub fn is_profiling_enabled() -> bool {
    PROFILER.with(|p| p.borrow().is_enabled())
}

/// Reset profiling data
pub fn reset_profiling() {
    PROFILER.with(|p| p.borrow_mut().reset());
}

/// Access the profiler
pub fn with_profiler<F, R>(f: F) -> R
where
    F: FnOnce(&mut Profiler) -> R,
{
    PROFILER.with(|p| f(&mut p.borrow_mut()))
}

/// Get a copy of the profiler's summary
pub fn get_profile_summary() -> ProfileSummary {
    PROFILER.with(|p| p.borrow().summary())
}

/// Get Chrome tracing JSON
pub fn get_chrome_tracing_json() -> String {
    PROFILER.with(|p| p.borrow().to_chrome_tracing_json())
}

/// Get DAG JSON
pub fn get_dag_json() -> String {
    PROFILER.with(|p| p.borrow().dag_to_json())
}

/// Get tree JSON
pub fn get_tree_json() -> String {
    PROFILER.with(|p| p.borrow().tree_to_json())
}

// ============================================================================
// Convenience macros and RAII guards
// ============================================================================

/// RAII guard for a span
pub struct SpanGuard {
    active: bool,
}

impl SpanGuard {
    pub fn new(name: &str, category: &str) -> Self {
        let active = is_profiling_enabled();
        if active {
            with_profiler(|p| p.start_span(name, category));
        }
        Self { active }
    }

    pub fn end_with_args(mut self, args: std::collections::HashMap<String, String>) {
        if self.active {
            with_profiler(|p| p.end_span_with_args(args));
            self.active = false;
        }
    }
}

impl Drop for SpanGuard {
    fn drop(&mut self) {
        if self.active {
            with_profiler(|p| p.end_span());
        }
    }
}

/// RAII guard for entering a fixed-point
pub struct FixedPointGuard {
    active: bool,
    id: u64,
    start: Instant,
    child_time: Duration,
}

impl FixedPointGuard {
    pub fn new(name: Option<String>) -> Self {
        let active = is_profiling_enabled();
        let id = if active {
            with_profiler(|p| p.enter_fixed_point(name))
        } else {
            0
        };
        Self {
            active,
            id,
            start: Instant::now(),
            child_time: Duration::ZERO,
        }
    }

    pub fn id(&self) -> u64 {
        self.id
    }

    pub fn add_child_time(&mut self, time: Duration) {
        self.child_time += time;
    }

    pub fn update_stats(&self, iterations: usize, states_processed: usize, blocks_executed: usize) {
        if self.active {
            with_profiler(|p| p.update_tree_node(self.id, iterations, states_processed, blocks_executed));
        }
    }
}

impl Drop for FixedPointGuard {
    fn drop(&mut self) {
        if self.active {
            let total_time = self.start.elapsed();
            with_profiler(|p| p.exit_fixed_point(total_time, self.child_time));
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_profiler_disabled_by_default() {
        let profiler = Profiler::new();
        assert!(!profiler.is_enabled());
    }

    #[test]
    fn test_profiler_enable() {
        let mut profiler = Profiler::new();
        profiler.enable();
        assert!(profiler.is_enabled());
    }

    #[test]
    fn test_dag_node_creation() {
        let mut profiler = Profiler::new();
        profiler.enable();

        let id = profiler.create_dag_node(5, 100, DagOperation::Entry, None);
        assert_eq!(id, 0);

        let nodes = profiler.dag_nodes();
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0].state_count, 5);
        assert_eq!(nodes[0].expanded_count, 100);
    }

    #[test]
    fn test_dag_node_parent_child() {
        let mut profiler = Profiler::new();
        profiler.enable();

        let parent_id = profiler.create_dag_node(1, 10, DagOperation::Entry, None);
        let child_id = profiler.create_dag_node(
            2,
            20,
            DagOperation::BlockExecution { block_name: Some("test".to_string()) },
            Some(parent_id),
        );

        let nodes = profiler.dag_nodes();
        assert_eq!(nodes[1].parent_id, Some(parent_id));
    }

    #[test]
    fn test_merged_dag_node() {
        let mut profiler = Profiler::new();
        profiler.enable();

        let id1 = profiler.create_dag_node(1, 10, DagOperation::Entry, None);
        let id2 = profiler.create_dag_node(2, 20, DagOperation::Entry, None);
        let merged_id = profiler.create_merged_dag_node(
            1,
            30,
            DagOperation::Vectorization,
            vec![id1, id2],
        );

        let nodes = profiler.dag_nodes();
        assert_eq!(nodes[2].merged_from, vec![id1, id2]);
    }

    #[test]
    fn test_tree_node_creation() {
        let mut profiler = Profiler::new();
        profiler.enable();

        let id = profiler.enter_fixed_point(Some("test_fn".to_string()));
        assert_eq!(id, 0);
        assert_eq!(profiler.current_tree_node(), Some(0));

        profiler.exit_fixed_point(Duration::from_millis(100), Duration::from_millis(10));
        assert_eq!(profiler.current_tree_node(), None);

        let nodes = profiler.tree_nodes();
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0].name, Some("test_fn".to_string()));
        assert_eq!(nodes[0].total_time, Duration::from_millis(100));
        assert_eq!(nodes[0].self_time, Duration::from_millis(90));
    }

    #[test]
    fn test_tree_nesting() {
        let mut profiler = Profiler::new();
        profiler.enable();

        let parent_id = profiler.enter_fixed_point(Some("parent".to_string()));
        let child_id = profiler.enter_fixed_point(Some("child".to_string()));

        let nodes = profiler.tree_nodes();
        assert_eq!(nodes[1].parent_id, Some(parent_id));
        assert_eq!(nodes[1].depth, 1);

        // Check parent has child in children list
        assert!(nodes[0].children.contains(&child_id));

        profiler.exit_fixed_point(Duration::from_millis(50), Duration::ZERO);
        profiler.exit_fixed_point(Duration::from_millis(100), Duration::from_millis(50));
    }

    #[test]
    fn test_spans() {
        let mut profiler = Profiler::new();
        profiler.enable();

        profiler.start_span("test_span", "test_category");
        std::thread::sleep(Duration::from_millis(10));
        profiler.end_span();

        let spans = profiler.spans();
        assert_eq!(spans.len(), 1);
        assert_eq!(spans[0].name, "test_span");
        assert_eq!(spans[0].category, "test_category");
        assert!(spans[0].duration_us >= 10000); // At least 10ms
    }

    #[test]
    fn test_span_with_args() {
        let mut profiler = Profiler::new();
        profiler.enable();

        profiler.start_span("test_span", "test_category");
        let mut args = std::collections::HashMap::new();
        args.insert("key".to_string(), "value".to_string());
        profiler.end_span_with_args(args);

        let spans = profiler.spans();
        assert_eq!(spans[0].args.get("key"), Some(&"value".to_string()));
    }

    #[test]
    fn test_chrome_tracing_json() {
        let mut profiler = Profiler::new();
        profiler.enable();

        profiler.start_span("test", "cat");
        profiler.end_span();

        let json = profiler.to_chrome_tracing_json();
        assert!(json.contains("\"name\": \"test\""));
        assert!(json.contains("\"cat\": \"cat\""));
        assert!(json.contains("\"ph\": \"X\""));
    }

    #[test]
    fn test_summary() {
        let mut profiler = Profiler::new();
        profiler.enable();

        profiler.create_dag_node(1, 10, DagOperation::Entry, None);
        profiler.create_dag_node(2, 20, DagOperation::BuiltinSplit { builtin_name: "test".to_string() }, None);
        profiler.create_dag_node(3, 30, DagOperation::Vectorization, None);

        profiler.enter_fixed_point(Some("test".to_string()));
        profiler.exit_fixed_point(Duration::from_millis(100), Duration::ZERO);

        let summary = profiler.summary();
        assert_eq!(summary.total_dag_nodes, 3);
        assert_eq!(summary.total_tree_nodes, 1);
        assert_eq!(summary.builtin_splits, 1);
        assert_eq!(summary.vectorizations, 1);
    }

    #[test]
    fn test_span_guard() {
        reset_profiling();
        enable_profiling();

        {
            let _guard = SpanGuard::new("test", "cat");
            std::thread::sleep(Duration::from_millis(5));
        }

        let json = get_chrome_tracing_json();
        assert!(json.contains("\"name\": \"test\""));
    }

    #[test]
    fn test_fixed_point_guard() {
        reset_profiling();
        enable_profiling();

        {
            let guard = FixedPointGuard::new(Some("test_fn".to_string()));
            guard.update_stats(5, 100, 50);
        }

        let json = get_tree_json();
        assert!(json.contains("\"name\": \"test_fn\""));
        assert!(json.contains("\"iterations\": 5"));
    }

    #[test]
    fn test_dag_tree_association() {
        let mut profiler = Profiler::new();
        profiler.enable();

        let tree_id = profiler.enter_fixed_point(Some("test".to_string()));
        let dag_id = profiler.create_dag_node(1, 10, DagOperation::Entry, None);

        let dag_nodes = profiler.dag_nodes();
        assert_eq!(dag_nodes[0].tree_node_id, Some(tree_id));

        let tree_nodes = profiler.tree_nodes();
        assert!(tree_nodes[0].dag_nodes.contains(&dag_id));

        profiler.exit_fixed_point(Duration::from_millis(100), Duration::ZERO);
    }

    #[test]
    fn test_disabled_profiler_noop() {
        let mut profiler = Profiler::new();
        // Don't enable

        let id = profiler.create_dag_node(1, 10, DagOperation::Entry, None);
        assert_eq!(id, 0); // Returns 0 when disabled
        assert!(profiler.dag_nodes().is_empty()); // No nodes created

        let tree_id = profiler.enter_fixed_point(Some("test".to_string()));
        assert_eq!(tree_id, 0);
        assert!(profiler.tree_nodes().is_empty());
    }
}
