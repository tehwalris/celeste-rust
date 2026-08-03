//! A persistent worker pool for per-lane loops.
//!
//! `std::thread::scope` costs tens of microseconds per spawned thread, which
//! is fine for a few hundred merge calls per run but catastrophic when every
//! large vector instruction pays it (measured: +75% wall clock at 39 frames
//! when `map2`/`select` spawned scoped threads per operation). This pool
//! parks its workers between jobs; dispatching a job is a mutex + condvar
//! wake, a few microseconds.
//!
//! # Safety
//!
//! `run` erases the closure's lifetime to hand it to the workers, which is
//! sound because `run` does not return until every chunk has completed - the
//! borrow outlives all worker access. The closure runs on multiple threads
//! concurrently, hence the `Sync` bound; chunk indices partition the work,
//! so data disjointness is the *caller's* contract, expressed through the
//! usual `&mut` rules of whatever the closure writes to.

use std::sync::{Condvar, Mutex, OnceLock};

/// How many chunks a pooled job is split into (and how many workers serve
/// it). Memory-bound loops saturate well below the core count.
pub fn pool_threads() -> usize {
    std::thread::available_parallelism()
        .map(|p| p.get())
        .unwrap_or(1)
        .min(8)
}

struct Job {
    /// Type-erased `&(dyn Fn(usize) + Sync)` with the lifetime removed.
    f: *const (dyn Fn(usize) + Sync),
    chunks: usize,
}
// The pointer crosses threads only while `run` blocks on the barrier.
unsafe impl Send for Job {}

struct PoolState {
    /// Incremented per job so parked workers can tell "new job" from
    /// spurious wakeups.
    generation: u64,
    job: Option<Job>,
    next_chunk: usize,
    outstanding: usize,
}

struct Pool {
    /// Serializes whole jobs: `run` holds this for its full duration, so
    /// concurrent callers (tests, mainly - the interpreter has one driver
    /// thread) queue up instead of corrupting each other's job slot.
    submit: Mutex<()>,
    state: Mutex<PoolState>,
    work_cv: Condvar,
    done_cv: Condvar,
}

static POOL: OnceLock<&'static Pool> = OnceLock::new();

fn pool() -> &'static Pool {
    POOL.get_or_init(|| {
        let pool: &'static Pool = Box::leak(Box::new(Pool {
            submit: Mutex::new(()),
            state: Mutex::new(PoolState {
                generation: 0,
                job: None,
                next_chunk: 0,
                outstanding: 0,
            }),
            work_cv: Condvar::new(),
            done_cv: Condvar::new(),
        }));
        for _ in 0..pool_threads() {
            std::thread::Builder::new()
                .name("lane-pool".to_string())
                .spawn(move || worker(pool))
                .expect("spawn lane-pool worker");
        }
        pool
    })
}

fn worker(pool: &'static Pool) {
    let mut seen_generation = 0u64;
    loop {
        let mut state = pool.state.lock().unwrap();
        loop {
            if state.generation != seen_generation && state.job.is_some() {
                seen_generation = state.generation;
                break;
            }
            state = pool.work_cv.wait(state).unwrap();
        }
        loop {
            let (f, chunks) = match &state.job {
                Some(job) => (job.f, job.chunks),
                None => break,
            };
            if state.next_chunk >= chunks {
                break;
            }
            let chunk = state.next_chunk;
            state.next_chunk += 1;
            drop(state);
            // Safety: `run` blocks until `outstanding` reaches zero, so the
            // erased borrow is alive for the duration of this call.
            unsafe { (*f)(chunk) };
            state = pool.state.lock().unwrap();
            state.outstanding -= 1;
            if state.outstanding == 0 {
                pool.done_cv.notify_all();
            }
        }
        drop(state);
    }
}

/// Run `f(0), f(1), ..., f(chunks-1)` across the pool, returning when all
/// chunks are done. The calling thread works too, so a single-chunk job
/// never leaves this thread.
pub fn run(chunks: usize, f: &(dyn Fn(usize) + Sync)) {
    if chunks <= 1 {
        for i in 0..chunks {
            f(i);
        }
        return;
    }
    let pool = pool();
    let _submit = pool.submit.lock().unwrap();
    {
        let mut state = pool.state.lock().unwrap();
        debug_assert!(state.job.is_none(), "lane-pool jobs do not nest");
        // Safety: see module docs - `run` outlives all worker access.
        let erased: *const (dyn Fn(usize) + Sync) =
            unsafe { std::mem::transmute::<&(dyn Fn(usize) + Sync), _>(f) };
        state.job = Some(Job { f: erased, chunks });
        state.generation += 1;
        state.next_chunk = 0;
        state.outstanding = chunks;
        pool.work_cv.notify_all();
    }
    // Participate: take chunks like a worker instead of just blocking.
    loop {
        let mut state = pool.state.lock().unwrap();
        let take = match &state.job {
            Some(job) if state.next_chunk < job.chunks => {
                let chunk = state.next_chunk;
                state.next_chunk += 1;
                Some(chunk)
            }
            _ => None,
        };
        match take {
            Some(chunk) => {
                drop(state);
                f(chunk);
                let mut state = pool.state.lock().unwrap();
                state.outstanding -= 1;
                if state.outstanding == 0 {
                    pool.done_cv.notify_all();
                }
            }
            None => {
                while state.outstanding > 0 {
                    state = pool.done_cv.wait(state).unwrap();
                }
                state.job = None;
                return;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[test]
    fn runs_every_chunk_exactly_once() {
        let counter = AtomicUsize::new(0);
        let seen: Vec<AtomicUsize> = (0..37).map(|_| AtomicUsize::new(0)).collect();
        super::run(37, &|i| {
            seen[i].fetch_add(1, Ordering::SeqCst);
            counter.fetch_add(1, Ordering::SeqCst);
        });
        assert_eq!(counter.load(Ordering::SeqCst), 37);
        assert!(seen.iter().all(|s| s.load(Ordering::SeqCst) == 1));
    }

    #[test]
    fn sequential_jobs_reuse_the_pool() {
        for round in 0..50 {
            let counter = AtomicUsize::new(0);
            super::run(8, &|_| {
                counter.fetch_add(1, Ordering::SeqCst);
            });
            assert_eq!(counter.load(Ordering::SeqCst), 8, "round {}", round);
        }
    }
}
