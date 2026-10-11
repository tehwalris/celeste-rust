//! One scoped worker pool: `n` items pulled by index from an atomic cursor,
//! each worker with its own state (a reference engine, a scratch buffer),
//! the results in index order. The first error stops the pulling and is
//! returned; a worker's panic is resumed in the caller with its own message.

use anyhow::Result;
use std::sync::atomic::{AtomicUsize, Ordering};

/// A pool of `threads` workers (at most one per item, at least one).
pub struct Pool {
    threads: usize,
    stack: Option<usize>,
}

impl Pool {
    pub fn new(threads: usize) -> Self {
        Pool { threads: threads.max(1), stack: None }
    }

    /// One worker per available core.
    pub fn per_core() -> Self {
        Self::new(std::thread::available_parallelism().map_or(8, |p| p.get()))
    }

    /// Workers with a `bytes` stack (the tracer recurses deeply).
    pub fn stack(mut self, bytes: usize) -> Self {
        self.stack = Some(bytes);
        self
    }

    /// The workers a map over `n` items runs.
    pub fn workers(&self, n: usize) -> usize {
        self.threads.min(n.max(1))
    }

    /// `f(i)` for every `i < n`, results by index.
    pub fn map<T: Send>(&self, n: usize, f: impl Fn(usize) -> Result<T> + Sync) -> Result<Vec<T>> {
        self.map_with(n, || Ok(()), |_, i| f(i))
    }

    /// `f(&mut state, i)` for every `i < n`, each worker's `state` from
    /// `init` (built in the worker), results by index.
    pub fn map_with<S, T: Send>(&self, n: usize, init: impl Fn() -> Result<S> + Sync, f: impl Fn(&mut S, usize) -> Result<T> + Sync) -> Result<Vec<T>> {
        let next = AtomicUsize::new(0);
        let work = || -> Result<Vec<(usize, T)>> {
            let mut state = init()?;
            let mut mine = Vec::new();
            loop {
                let i = next.fetch_add(1, Ordering::Relaxed);
                if i >= n {
                    return Ok(mine);
                }
                match f(&mut state, i) {
                    Ok(v) => mine.push((i, v)),
                    Err(e) => {
                        next.store(n, Ordering::Relaxed);
                        return Err(e);
                    }
                }
            }
        };
        let parts = std::thread::scope(|s| {
            let handles: Vec<_> = (0..self.workers(n))
                .map(|_| {
                    let mut b = std::thread::Builder::new();
                    if let Some(bytes) = self.stack {
                        b = b.stack_size(bytes);
                    }
                    b.spawn_scoped(s, &work).expect("spawn a pool worker")
                })
                .collect();
            handles.into_iter().map(|h| h.join().unwrap_or_else(|p| std::panic::resume_unwind(p))).collect::<Result<Vec<_>>>()
        })?;
        let mut all: Vec<(usize, T)> = parts.into_iter().flatten().collect();
        all.sort_unstable_by_key(|p| p.0);
        debug_assert!(all.iter().enumerate().all(|(k, p)| p.0 == k));
        Ok(all.into_iter().map(|p| p.1).collect())
    }
}

#[cfg(test)]
mod tests {
    use super::Pool;

    /// Results come back in index order whatever the workers' interleaving,
    /// each worker's state is its own, and an error is returned.
    #[test]
    fn a_pool_maps_in_order_with_per_worker_state() {
        let out = Pool::new(4).map(1000, |i| Ok(i * 2)).unwrap();
        assert_eq!(out, (0..1000).map(|i| i * 2).collect::<Vec<_>>());
        // Each worker counts its own items: the counts sum to n.
        let seen = Pool::new(3)
            .map_with(
                100,
                || Ok(0usize),
                |k, _| {
                    *k += 1;
                    Ok(*k)
                },
            )
            .unwrap();
        assert_eq!(seen.len(), 100);
        assert!(seen.iter().all(|&k| (1..=100).contains(&k)));
        let err = Pool::new(2).map(50, |i| if i == 7 { anyhow::bail!("item {i}") } else { Ok(i) });
        assert_eq!(err.unwrap_err().to_string(), "item 7");
        assert!(Pool::new(8).map(0, Ok).unwrap().is_empty());
    }
}
