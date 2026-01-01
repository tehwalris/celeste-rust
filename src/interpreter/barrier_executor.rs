//! Barrier-based execution: process states through barriers in order,
//! using PathCounter for abstract path enumeration.

use std::collections::{BTreeMap, HashMap};

use anyhow::Result;

use crate::ir::{BarrierId, Cfg, Label};

use super::{
    fixed_env::FixedEnv,
    state::State,
    value::Value,
    vectorize::vectorize_states,
};

/// PathCounter tracks which abstract path we're exploring.
/// When encountering UnknownBool, we consult the counter to pick true/false.
/// After a run completes, we increment and try again until exhausted.
#[derive(Clone, Debug)]
pub struct PathCounter {
    /// Current path: bit i = true means take "true" branch for choice i
    current_path: u64,
    /// Number of choices made so far in this run
    choices_made: usize,
    /// Maximum choices seen in any path (for knowing when we're done)
    max_choices: usize,
}

impl PathCounter {
    pub fn new() -> Self {
        Self {
            current_path: 0,
            choices_made: 0,
            max_choices: 0,
        }
    }

    /// Get the choice for the next UnknownBool branch.
    /// Returns true or false based on the current path.
    pub fn get_choice(&mut self) -> bool {
        let choice = (self.current_path >> self.choices_made) & 1 == 1;
        self.choices_made += 1;
        self.max_choices = self.max_choices.max(self.choices_made);
        choice
    }

    /// Check if the choice at index `choice_idx` is true in current path
    pub fn peek_choice(&self, choice_idx: usize) -> bool {
        (self.current_path >> choice_idx) & 1 == 1
    }

    /// Reset for a new run (called at barrier)
    pub fn reset_for_new_run(&mut self) {
        self.choices_made = 0;
    }

    /// Try to increment to the next path.
    /// Returns true if there are more paths to explore, false if exhausted.
    pub fn increment(&mut self) -> bool {
        if self.max_choices == 0 {
            // No choices were made - only one path exists
            return false;
        }

        // Increment the path counter
        self.current_path += 1;

        // Check if we've exhausted all paths (2^max_choices)
        if self.current_path >= (1u64 << self.max_choices) {
            return false;
        }

        // Reset for new run
        self.choices_made = 0;
        true
    }

    /// Check if this path is still valid (haven't made impossible choices)
    pub fn is_valid(&self) -> bool {
        // A path becomes invalid if we've made more choices than the path encoding allows
        self.choices_made <= 64
    }
}

/// Key for ordering barrier processing: (barrier_id, hit_count)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BarrierKey {
    pub barrier_id: BarrierId,
    pub hit_count: usize,
}

/// A state waiting at a barrier
#[derive(Clone, Debug)]
pub struct WaitingState {
    pub state: State,
    pub barrier_key: BarrierKey,
}

/// The implicit "END" barrier for states that complete execution
pub fn end_barrier_id() -> BarrierId {
    BarrierId::new(vec![i32::MAX])
}

/// The implicit "START" barrier for initial states
pub fn start_barrier_id() -> BarrierId {
    BarrierId::new(vec![i32::MIN])
}

/// Result of running a scalar state from one barrier to the next
#[derive(Clone, Debug)]
pub struct BarrierRunResult {
    /// The output state
    pub state: State,
    /// The destination barrier (or END)
    pub destination: BarrierId,
    /// Optional return value (if this was a function call that returned)
    pub return_value: Option<Value>,
}

/// Execute states through a CFG using barrier-based execution.
///
/// This replaces the flow-based fixed-point algorithm with a simpler approach:
/// 1. Process barriers in order of (barrier_id, hit_count)
/// 2. At each barrier, vectorize accumulated states
/// 3. For each scalar lane, enumerate all abstract paths using PathCounter
/// 4. Collect output states at their destination barriers
/// 5. Repeat until all states reach END barrier
pub fn execute_with_barriers(
    cfg: &Cfg,
    initial_states: Vec<State>,
    fixed_env: &FixedEnv,
) -> Result<Vec<(State, Option<Value>)>> {
    // Map from barrier key to waiting states
    let mut pending: BTreeMap<BarrierKey, Vec<State>> = BTreeMap::new();

    // Track per-state hit counts (state hash -> hit count per barrier)
    // For now, use a simpler approach: global hit count per barrier
    let mut barrier_hit_counts: HashMap<BarrierId, usize> = HashMap::new();

    // Initialize with states at START barrier
    let start_key = BarrierKey {
        barrier_id: start_barrier_id(),
        hit_count: 0,
    };
    pending.insert(start_key, initial_states);

    // Collect results (states that reached END)
    let mut results: Vec<(State, Option<Value>)> = Vec::new();

    loop {
        // Find the lowest barrier key with pending states
        let next_key = pending.iter()
            .find(|(_, states)| !states.is_empty())
            .map(|(k, _)| k.clone());

        let current_key = match next_key {
            Some(k) => k,
            None => break, // No more pending states
        };

        // Take the states at this barrier
        let states = pending.remove(&current_key).unwrap_or_default();
        if states.is_empty() {
            continue;
        }

        // Check if this is the END barrier
        if current_key.barrier_id == end_barrier_id() {
            // These states are done
            for state in states {
                results.push((state, None));
            }
            continue;
        }

        // Vectorize the states at this barrier
        let vectorized = vectorize_states(states);

        println!(
            "  Barrier {:?} hit={}: {} vectorized states ({} expanded)",
            current_key.barrier_id.0,
            current_key.hit_count,
            vectorized.len(),
            vectorized.iter().map(|s| s.vector_size).sum::<usize>()
        );

        // Process each vectorized state
        for vec_state in vectorized {
            // For each scalar lane in the vector
            for lane_idx in 0..vec_state.vector_size {
                // Extract scalar state for this lane
                let scalar_state = extract_scalar_lane(&vec_state, lane_idx);

                // Run with PathCounter to enumerate all paths
                let mut path_counter = PathCounter::new();
                loop {
                    // Clone the scalar state for this path
                    let run_state = scalar_state.clone();

                    // Run until next barrier or completion
                    let run_result = run_to_next_barrier(
                        cfg,
                        run_state,
                        fixed_env,
                        &mut path_counter,
                        &current_key.barrier_id,
                    )?;

                    // Determine destination key
                    let dest_hit = barrier_hit_counts
                        .get(&run_result.destination)
                        .copied()
                        .unwrap_or(0);
                    let dest_key = BarrierKey {
                        barrier_id: run_result.destination.clone(),
                        hit_count: dest_hit,
                    };

                    // Add to pending at destination
                    pending.entry(dest_key)
                        .or_default()
                        .push(run_result.state);

                    // Try next path
                    if !path_counter.increment() {
                        break;
                    }
                    path_counter.reset_for_new_run();
                }
            }
        }

        // Increment hit count for this barrier
        *barrier_hit_counts.entry(current_key.barrier_id).or_insert(0) += 1;
    }

    Ok(results)
}

/// Extract a scalar state from a vectorized state at the given lane index
fn extract_scalar_lane(state: &State, lane_idx: usize) -> State {
    if state.vector_size == 1 {
        return state.clone();
    }

    // TODO: Implement proper scalar extraction
    // For now, just return the state as-is (this will be incorrect for vectors)
    // This needs to extract lane `lane_idx` from all MaybeVector values
    let mut scalar = state.clone();
    scalar.vector_size = 1;
    scalar
}

/// Run a scalar state from the current position until it hits a barrier or completes.
/// Uses the PathCounter to resolve UnknownBool branches.
fn run_to_next_barrier(
    cfg: &Cfg,
    state: State,
    fixed_env: &FixedEnv,
    path_counter: &mut PathCounter,
    current_barrier: &BarrierId,
) -> Result<BarrierRunResult> {
    // TODO: Implement the actual interpreter run
    // For now, just return immediately at END barrier
    // This is a placeholder - the real implementation needs to:
    // 1. Run the interpreter block by block
    // 2. On UnknownBool branches, use path_counter.get_choice() to decide
    // 3. Stop when hitting a barrier block or Return
    // 4. Return the state and destination

    Ok(BarrierRunResult {
        state,
        destination: end_barrier_id(),
        return_value: None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_counter_single_choice() {
        let mut pc = PathCounter::new();

        // First run: choice 0 = false (bit 0 of 0)
        assert!(!pc.get_choice());
        assert!(pc.increment());

        // Second run: choice 0 = true (bit 0 of 1)
        assert!(pc.get_choice());
        assert!(!pc.increment()); // No more paths
    }

    #[test]
    fn test_path_counter_two_choices() {
        let mut pc = PathCounter::new();

        // Run 0 (path 00): false, false
        assert!(!pc.get_choice());
        assert!(!pc.get_choice());
        assert!(pc.increment());

        // Run 1 (path 01): true, false
        assert!(pc.get_choice());
        assert!(!pc.get_choice());
        assert!(pc.increment());

        // Run 2 (path 10): false, true
        assert!(!pc.get_choice());
        assert!(pc.get_choice());
        assert!(pc.increment());

        // Run 3 (path 11): true, true
        assert!(pc.get_choice());
        assert!(pc.get_choice());
        assert!(!pc.increment()); // No more paths (2^2 = 4 paths done)
    }

    #[test]
    fn test_barrier_key_ordering() {
        let k1 = BarrierKey {
            barrier_id: BarrierId::new(vec![1]),
            hit_count: 0,
        };
        let k2 = BarrierKey {
            barrier_id: BarrierId::new(vec![1]),
            hit_count: 1,
        };
        let k3 = BarrierKey {
            barrier_id: BarrierId::new(vec![2]),
            hit_count: 0,
        };

        assert!(k1 < k2); // Same barrier, lower hit count
        assert!(k2 < k3); // Lower barrier ID
        assert!(k1 < k3);
    }
}
