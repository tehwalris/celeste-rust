//! PathCounter: tracks which path we're exploring through choice points.
//!
//! Works like a mixed-radix counter where each digit has its own maximum.
//! Example: [(2,0), (3,1)] means first choice had 2 options and we took 0,
//! second choice had 3 options and we took 1.

use serde::{Deserialize, Serialize};

/// Tracks the path through choice points during execution.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PathCounter {
    /// Each element is (num_options, chosen_option).
    /// chosen_option is always < num_options.
    choices: Vec<(usize, usize)>,
}

impl PathCounter {
    /// Create a new empty path counter.
    pub fn new() -> Self {
        Self { choices: Vec::new() }
    }

    /// Get the choices made so far.
    pub fn choices(&self) -> &[(usize, usize)] {
        &self.choices
    }

    /// Get the number of choices made so far.
    pub fn len(&self) -> usize {
        self.choices.len()
    }

    /// Check if no choices have been made.
    pub fn is_empty(&self) -> bool {
        self.choices.is_empty()
    }

    /// Record a choice during execution.
    /// If we're replaying, returns the previously recorded choice.
    /// If we're exploring, records a new choice (always picks 0 for new choices).
    pub fn record_choice(&mut self, num_options: usize, position: usize) -> usize {
        assert!(num_options > 0, "Must have at least one option");

        if position < self.choices.len() {
            // Replaying: verify num_options matches and return recorded choice
            let (recorded_options, recorded_choice) = self.choices[position];
            assert_eq!(
                recorded_options, num_options,
                "Number of options mismatch at position {}: expected {}, got {}",
                position, recorded_options, num_options
            );
            recorded_choice
        } else {
            // New choice: record it with option 0
            assert_eq!(position, self.choices.len(), "Choices must be sequential");
            self.choices.push((num_options, 0));
            0
        }
    }

    /// Increment the counter to explore the next path.
    /// Returns true if there are more paths to explore, false if we've exhausted all paths.
    pub fn increment(&mut self) -> bool {
        // Work backwards, trying to increment each digit
        while let Some((num_options, chosen)) = self.choices.pop() {
            let next_choice = chosen + 1;
            if next_choice < num_options {
                // Can increment this digit
                self.choices.push((num_options, next_choice));
                return true;
            }
            // This digit overflows, continue to previous
        }
        // All digits overflowed - we've explored everything
        false
    }

    /// Check if this counter has been fully explored (would overflow on increment).
    pub fn is_complete(&self) -> bool {
        // A counter is complete if all choices are at their maximum
        self.choices.iter().all(|(num_options, chosen)| chosen + 1 >= *num_options)
    }

    /// Get the total number of paths this counter represents.
    /// Returns None if there are no choices (single path).
    pub fn total_paths(&self) -> usize {
        if self.choices.is_empty() {
            1
        } else {
            self.choices.iter().map(|(n, _)| n).product()
        }
    }

    /// Create a counter from a list of choices.
    pub fn from_choices(choices: Vec<(usize, usize)>) -> Self {
        for (num_options, chosen) in &choices {
            assert!(*chosen < *num_options, "Invalid choice: {} >= {}", chosen, num_options);
        }
        Self { choices }
    }

    /// Truncate to only the first n choices.
    /// Used when a path ends earlier than expected.
    pub fn truncate(&mut self, n: usize) {
        self.choices.truncate(n);
    }
}

impl Default for PathCounter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_counter() {
        let counter = PathCounter::new();
        assert!(counter.is_empty());
        assert_eq!(counter.total_paths(), 1);
    }

    #[test]
    fn test_single_binary_choice() {
        let mut counter = PathCounter::new();

        // First exploration: record choice 0
        let choice = counter.record_choice(2, 0);
        assert_eq!(choice, 0);
        assert_eq!(counter.choices(), &[(2, 0)]);

        // Increment to explore choice 1
        assert!(counter.increment());
        assert_eq!(counter.choices(), &[(2, 1)]);

        // Replay: get choice 1
        let mut counter2 = counter.clone();
        let choice = counter2.record_choice(2, 0);
        assert_eq!(choice, 1);

        // Increment again: should overflow
        assert!(!counter.increment());
        assert!(counter.is_empty());
    }

    #[test]
    fn test_multiple_choices() {
        let mut counter = PathCounter::new();

        // First path: record choices (0, 0)
        assert_eq!(counter.record_choice(2, 0), 0);
        assert_eq!(counter.record_choice(3, 1), 0);
        assert_eq!(counter.choices(), &[(2, 0), (3, 0)]);

        // Enumerate paths by incrementing
        // Note: increment() works on the recorded choices. When last digit
        // overflows, it truncates. In real use, execution would then record
        // new choices.
        let mut paths = vec![counter.choices().to_vec()];
        while counter.increment() {
            paths.push(counter.choices().to_vec());
        }

        // We get 4 paths from increment alone:
        // 1. [(2,0), (3,0)] - initial
        // 2. [(2,0), (3,1)] - increment last digit
        // 3. [(2,0), (3,2)] - increment last digit
        // 4. [(2,1)] - last digit overflow, increment first, truncate
        // Then (2,1) would overflow to empty, ending iteration
        assert_eq!(paths.len(), 4);
        assert_eq!(paths[0], vec![(2, 0), (3, 0)]);
        assert_eq!(paths[1], vec![(2, 0), (3, 1)]);
        assert_eq!(paths[2], vec![(2, 0), (3, 2)]);
        assert_eq!(paths[3], vec![(2, 1)]);  // Truncated - execution would add more choices
    }

    #[test]
    fn test_total_paths() {
        let counter = PathCounter::from_choices(vec![(2, 0), (3, 0), (4, 0)]);
        assert_eq!(counter.total_paths(), 24); // 2 * 3 * 4
    }

    #[test]
    fn test_is_complete() {
        let counter = PathCounter::from_choices(vec![(2, 1), (3, 2)]);
        assert!(counter.is_complete());

        let counter2 = PathCounter::from_choices(vec![(2, 0), (3, 2)]);
        assert!(!counter2.is_complete());
    }
}
