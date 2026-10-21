//! TEST HELPERS the unit tests share: a scratch directory that cleans up
//! after a failing test too, and the tests' pseudo-random streams (each
//! test keeps its own seed and output transform: its cases stay its own).

use std::path::{Path, PathBuf};

/// A test's scratch directory, `celeste-NAME-PID` under the temp dir (not
/// created): removed at the start (a killed run's) and when dropped (a
/// failed assertion's too).
pub(crate) struct TestDir(PathBuf);

impl TestDir {
    pub(crate) fn new(name: &str) -> Self {
        let dir = std::env::temp_dir().join(format!("celeste-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        TestDir(dir)
    }
}

impl std::ops::Deref for TestDir {
    type Target = Path;
    fn deref(&self) -> &Path {
        &self.0
    }
}

impl AsRef<Path> for TestDir {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

impl Drop for TestDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// One step of the tests' LCG (Knuth's MMIX constants): the new state.
pub(crate) fn lcg(s: &mut u64) -> u64 {
    *s = s.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
    *s
}

/// One step of the tests' xorshift64 (13, 7, 17): the new state.
pub(crate) fn xorshift(x: &mut u64) -> u64 {
    *x ^= *x << 13;
    *x ^= *x >> 7;
    *x ^= *x << 17;
    *x
}
