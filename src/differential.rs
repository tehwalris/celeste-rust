//! ONE DIFFERENTIAL HARNESS (the `rewrite check` group): an implementation
//! held against an independent oracle over a sample. The sample is picked
//! (`spread`, `stride`), each item runs in parallel with an oracle engine
//! per worker (`run`), an item the oracle cannot take is SKIPPED with its
//! reason (`Ran::Skipped`: over the reference engine's path cap), the
//! items' reports go out in item order, and the verdict is decided in one
//! place (`verdict`).

use anyhow::Result;

/// One item's outcome.
pub enum Ran<T> {
    Done(T),
    /// Not compared, and why.
    Skipped(String),
}

/// `n` of `len` items, evenly spaced (`k * len / n`): `check ref`'s rows,
/// `check widen`'s emitted rows.
pub fn spread(len: usize, n: usize) -> Vec<usize> {
    let n = n.min(len);
    (0..n).map(|k| k * len / n).collect()
}

/// `n` of `len` items, every `len / n`-th from the first: `check arcs`'
/// records of a frame.
pub fn stride(len: usize, n: usize) -> Vec<usize> {
    (0..len).step_by((len / n.max(1)).max(1)).take(n).collect()
}

/// Items `0..n` through `f` in parallel (`threads` workers, each with its
/// own oracle engine from `engine`), in item order. An item the reference
/// engine cannot take (`TooManyPaths`, over its path cap) is SKIPPED with
/// the reason; any other error fails the check.
pub fn run<E, T: Send>(threads: usize, n: usize, engine: impl Fn() -> Result<E> + Sync, f: impl Fn(&mut E, usize) -> Result<T> + Sync) -> Result<Vec<Ran<T>>> {
    crate::par::Pool::new(threads).map_with(n, engine, |e, k| match f(e, k) {
        Err(err) if err.downcast_ref::<crate::engines::reference::TooManyPaths>().is_some() => Ok(Ran::Skipped(err.to_string())),
        other => other.map(Ran::Done),
    })
}

/// THE VERDICT of a check that compared `compared` items: `bad` of them
/// disagreeing fails it with `why`, and so does comparing NOTHING (every
/// item SKIPPED, or none to pick): a green check must have held the
/// implementation against its oracle at least once (plans/correctness.md
/// C14).
pub fn verdict(compared: usize, bad: usize, why: impl FnOnce() -> String) -> Result<()> {
    anyhow::ensure!(bad == 0, "{}", why());
    anyhow::ensure!(compared > 0, "nothing compared: every item was SKIPPED (or there was none): a check that compared nothing is no pass");
    Ok(())
}

/// The fields of `b` that differ from `a` (both by name), as `name: a ->
/// b` (`-`: absent).
pub fn field_diff(a: &std::collections::BTreeMap<String, String>, b: &std::collections::BTreeMap<String, String>) -> Vec<String> {
    a.iter().filter(|(k, v)| b.get(*k) != Some(v)).map(|(k, v)| format!("{k}: {v} -> {}", b.get(k).map_or("-", |s| s.as_str()))).collect()
}

#[cfg(test)]
mod tests {
    #[test]
    fn samples_are_picked_as_the_checks_always_picked_them() {
        assert_eq!(super::spread(10, 4), [0, 2, 5, 7]);
        assert_eq!(super::spread(3, 8), [0, 1, 2]);
        assert_eq!(super::stride(10, 4), [0, 2, 4, 6]);
        assert_eq!(super::stride(3, 8), [0, 1, 2]);
        assert!(super::verdict(5, 1, || "bad".into()).is_err());
        assert!(super::verdict(5, 0, String::new).is_ok());
    }

    #[test]
    fn a_check_that_compared_nothing_fails() {
        let e = super::verdict(0, 0, String::new).expect_err("nothing compared passed");
        assert!(e.to_string().contains("nothing compared"), "{e}");
    }
}
