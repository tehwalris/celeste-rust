//! One scoped worker pool: `n` items pulled by index from an atomic cursor,
//! each worker with its own state (a reference engine, a scratch buffer),
//! the results in index order (`map`, `map_with`, `map_on`), or an unordered fold
//! into per-worker accumulators (`drain`). The first error stops the
//! pulling and is returned; a worker's panic is resumed in the caller with
//! its own message.
#![warn(unreachable_pub)]

use anyhow::Result;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

/// A pool of `threads` workers (at most one per item, at least one).
pub struct Pool {
    threads: usize,
    stack: Option<usize>,
}

impl Pool {
    pub fn new(threads: usize) -> Self {
        Pool { threads: threads.max(1), stack: None }
    }

    /// Workers with a `bytes` stack (the tracer recurses deeply).
    pub(crate) fn stack(mut self, bytes: usize) -> Self {
        self.stack = Some(bytes);
        self
    }

    /// The workers a map over `n` items runs.
    pub(crate) fn workers(&self, n: usize) -> usize {
        self.threads.min(n.max(1))
    }

    /// `f(i)` for every `i < n`, results by index.
    pub(crate) fn map<T: Send>(&self, n: usize, f: impl Fn(usize) -> Result<T> + Sync) -> Result<Vec<T>> {
        self.map_with(n, || Ok(()), |_, i| f(i))
    }

    /// `f(lo, hi)` over `0..n` in chunks of `chunk`, results in chunk order.
    pub(crate) fn chunks<T: Send>(&self, n: usize, chunk: usize, f: impl Fn(usize, usize) -> T + Sync) -> Vec<T> {
        let chunk = chunk.max(1);
        self.map(n.div_ceil(chunk), |c| Ok(f(c * chunk, ((c + 1) * chunk).min(n)))).expect("an infallible map")
    }

    /// `f(&mut state, i)` for every `i < n`, each worker's `state` from
    /// `init` (built in the worker), results by index.
    pub fn map_with<S, T: Send>(&self, n: usize, init: impl Fn() -> Result<S> + Sync, f: impl Fn(&mut S, usize) -> Result<T> + Sync) -> Result<Vec<T>> {
        let next = AtomicUsize::new(0);
        let slots = slots(n);
        self.spawn(vec![(); self.workers(n)], |()| pull(&next, n, &slots, &mut init()?, &f))?;
        Ok(results(slots))
    }

    /// `f(&mut states[w], i)` for every `i < n` on `min(states.len(), n)`
    /// workers (at most this pool's), worker `w` holding `states[w]`
    /// throughout (an engine each, built by the caller); results by index.
    pub(crate) fn map_on<S: Send, T: Send>(&self, states: &mut [S], n: usize, f: impl Fn(&mut S, usize) -> Result<T> + Sync) -> Result<Vec<T>> {
        assert!(n == 0 || !states.is_empty(), "a map over {n} items on no state");
        let next = AtomicUsize::new(0);
        let slots = slots(n);
        let k = self.workers(n).min(states.len());
        self.spawn(states.iter_mut().take(k).collect(), |s| pull(&next, n, &slots, s, &f))?;
        Ok(results(slots))
    }

    /// AN UNORDERED FOLD: each worker builds its accumulator (`init`),
    /// pulls items `from..n` IN ORDER into it (`f`) while `more()`, and
    /// finishes it (`done`, in the worker: a sort runs in parallel). The
    /// finished accumulators (one a worker) and the first item not taken
    /// (every one before it was). WHICH accumulator holds an item depends on
    /// the scheduling, and so does where `more()` stops: callers join the
    /// accumulators commutatively and associatively only.
    pub(crate) fn drain<S, T: Send>(
        &self,
        from: usize,
        n: usize,
        init: impl Fn() -> S + Sync,
        f: impl Fn(&mut S, usize) -> Result<()> + Sync,
        more: impl Fn() -> bool + Sync,
        done: impl Fn(S) -> T + Sync,
    ) -> Result<(Vec<T>, usize)> {
        let next = AtomicUsize::new(from);
        let out = self.spawn(vec![(); self.workers(n.saturating_sub(from))], |()| {
            let mut acc = init();
            while more() {
                let i = next.fetch_add(1, Ordering::Relaxed);
                if i >= n {
                    break;
                }
                if let Err(e) = f(&mut acc, i) {
                    next.store(n, Ordering::Relaxed);
                    return Err(e);
                }
            }
            Ok(done(acc))
        })?;
        Ok((out, next.into_inner().min(n)))
    }

    /// `work(w)` for each of `per` on its own scoped worker (with this
    /// pool's stack), results in `per`'s order. Every worker is joined; a
    /// panic is resumed here with its own message, else the first error
    /// (in worker order) returned.
    fn spawn<W: Send, R: Send>(&self, per: Vec<W>, work: impl Fn(W) -> Result<R> + Sync) -> Result<Vec<R>> {
        let work = &work;
        let joined: Vec<std::thread::Result<Result<R>>> = std::thread::scope(|s| {
            let handles: Vec<_> = per
                .into_iter()
                .map(|w| {
                    let mut b = std::thread::Builder::new();
                    if let Some(bytes) = self.stack {
                        b = b.stack_size(bytes);
                    }
                    b.spawn_scoped(s, move || work(w)).expect("spawn a pool worker")
                })
                .collect();
            handles.into_iter().map(|h| h.join()).collect()
        });
        let mut out = Vec::with_capacity(joined.len());
        let mut first_err = None;
        for r in joined {
            match r {
                Err(p) => std::panic::resume_unwind(p),
                Ok(Ok(v)) => out.push(v),
                Ok(Err(e)) => first_err = first_err.or(Some(e)),
            }
        }
        first_err.map_or(Ok(out), Err)
    }
}

/// One result slot per item, allocated by the CALLER before any worker
/// runs: a worker keeps nothing live between items (a result vector
/// growing in its glibc arena pins the memory earlier items freed: +6 GB,
/// 16 workers).
fn slots<T>(n: usize) -> Vec<Mutex<Option<T>>> {
    (0..n).map(|_| Mutex::new(None)).collect()
}

fn results<T>(slots: Vec<Mutex<Option<T>>>) -> Vec<T> {
    slots.into_iter().map(|m| m.into_inner().expect("a result slot").expect("every item ran")).collect()
}

/// A worker's loop: items pulled from `next` below `n` into their slots
/// with `f(state, i)`; an error stops every worker's pulling.
fn pull<S, T>(next: &AtomicUsize, n: usize, slots: &[Mutex<Option<T>>], state: &mut S, f: &impl Fn(&mut S, usize) -> Result<T>) -> Result<()> {
    loop {
        let i = next.fetch_add(1, Ordering::Relaxed);
        if i >= n {
            return Ok(());
        }
        match f(state, i) {
            Ok(v) => *slots[i].lock().expect("a result slot") = Some(v),
            Err(e) => {
                next.store(n, Ordering::Relaxed);
                return Err(e);
            }
        }
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
        // Each worker counts its own items.
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

    /// The panic's message, from a payload.
    fn message(p: Box<dyn std::any::Any + Send>) -> String {
        p.downcast_ref::<String>().cloned().or_else(|| p.downcast_ref::<&str>().map(|s| s.to_string())).unwrap_or_default()
    }

    /// `map_on`: results by index at 1, 3 and 32 states; each item once,
    /// on one worker that holds its state throughout (a state is never on
    /// two threads); an error stops the others early and is returned; a
    /// panic is resumed with its message.
    #[test]
    fn a_map_on_states_runs_each_item_once_on_its_workers_state() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        for k in [1, 3, 32] {
            let mut states: Vec<(Option<std::thread::ThreadId>, Vec<usize>)> = vec![(None, Vec::new()); k];
            let out = Pool::new(k)
                .map_on(&mut states, 1000, |st, i| {
                    let me = std::thread::current().id();
                    assert_eq!(*st.0.get_or_insert(me), me, "a state on two workers");
                    st.1.push(i);
                    Ok(i * 3)
                })
                .unwrap();
            assert_eq!(out, (0..1000).map(|i| i * 3).collect::<Vec<_>>(), "{k} states");
            let mut all: Vec<usize> = states.iter().flat_map(|s| s.1.iter().copied()).collect();
            all.sort_unstable();
            assert_eq!(all, (0..1000).collect::<Vec<_>>(), "{k} states: each item once");
            let ran = AtomicUsize::new(0);
            let err = Pool::new(k).map_on(&mut states, 100_000, |_, i| {
                ran.fetch_add(1, Ordering::Relaxed);
                if i == 0 { anyhow::bail!("item {i}") } else { Ok(i) }
            });
            assert_eq!(err.unwrap_err().to_string(), "item 0");
            assert!(ran.load(Ordering::Relaxed) < 100_000, "{k} states: the error stopped the others");
            let p = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| Pool::new(k).map_on(&mut states, 100, |_, i| if i == 5 { panic!("boom {i}") } else { Ok(i) })));
            assert_eq!(message(p.expect_err("a panic")), "boom 5");
        }
        assert!(Pool::new(4).map_on(&mut Vec::<()>::new(), 0, |_, i| Ok(i)).unwrap().is_empty());
    }

    /// A drain takes each item of `from..n` exactly once into some
    /// accumulator, at 1, 3 and 32 workers; with `more()` false it stops,
    /// every item before the one it returns taken; an item's error is
    /// returned; a worker's panic is resumed with its message.
    #[test]
    fn a_drain_takes_each_item_once_and_stops() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        for workers in [1, 3, 32] {
            let pool = Pool::new(workers);
            let (accs, end) = pool.drain(5, 1000, Vec::new, |a: &mut Vec<usize>, i| Ok(a.push(i)), || true, |a| a).unwrap();
            assert_eq!(end, 1000);
            assert!(accs.len() <= workers);
            let mut all: Vec<usize> = accs.concat();
            all.sort_unstable();
            assert_eq!(all, (5..1000).collect::<Vec<_>>(), "{workers} workers");
            // Early stop: past 10 items taken, no worker takes another.
            let taken = AtomicUsize::new(0);
            let (accs, end) = pool
                .drain(5, 1000, Vec::new, |a: &mut Vec<usize>, i| {
                    taken.fetch_add(1, Ordering::Relaxed);
                    Ok(a.push(i))
                }, || taken.load(Ordering::Relaxed) < 10, |a| a)
                .unwrap();
            let mut all: Vec<usize> = accs.concat();
            all.sort_unstable();
            assert_eq!(all, (5..end).collect::<Vec<_>>(), "{workers} workers: what was taken is what came before the end");
            assert!((15..15 + workers).contains(&end), "{workers} workers: stopped at {end}");
            let err = pool.drain(0, 100, || (), |_, i| if i == 7 { anyhow::bail!("item {i}") } else { Ok(()) }, || true, |a| a);
            assert_eq!(err.unwrap_err().to_string(), "item 7");
            let p = std::panic::catch_unwind(|| pool.drain(0, 100, || (), |_, i| if i == 5 { panic!("boom {i}") } else { Ok(()) }, || true, |a| a));
            assert_eq!(message(p.expect_err("a panic")), "boom 5");
        }
        assert_eq!(Pool::new(4).drain(9, 9, || 0, |_, _| Ok(()), || true, |a| a).unwrap().1, 9);
    }
}
