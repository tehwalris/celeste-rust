//! Canonical, id-free fingerprints shared by `vprod` and `sweep` (included
//! with `#[path]`): an edge is (source id, target KEY, transfer), so two runs
//! whose new-state ids differ (admission order, thread count, renumbering)
//! still compare equal when their edge SETS are equal.
#![allow(dead_code)]

#[inline]
pub fn mix64(mut x: u64) -> u64 {
    x = (x ^ (x >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    x = (x ^ (x >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    x ^ (x >> 31)
}

/// The (source id, transfer) half of an edge's hash. `src_id` is production's
/// packed id (`pack_id(56, seq, row)`, as src.bin stores it).
#[inline]
pub fn sx_hash(src_id: u64, xfer: u32) -> u64 {
    mix64(src_id ^ mix64(xfer as u64 ^ 0x5bd1_e995_0000_0001))
}

/// The target half: its 16-B row key (lo, hi).
#[inline]
pub fn key_hash(lo: u64, hi: u64) -> u64 {
    mix64(lo ^ mix64(hi ^ 0x2545_f491_4f6c_dd1d))
}

#[inline]
pub fn edge_hash(sx: u64, kh: u64) -> u64 {
    mix64(sx.rotate_left(23) ^ kh)
}

#[inline]
pub fn note_hash(src_id: u64, from: u32) -> u64 {
    mix64(src_id ^ mix64(from as u64 ^ 0x9e37_79b9_7f4a_7c15))
}

/// (count, distinct, wrapping sum of the distinct hashes): sorts `h` in
/// parallel (bucketed by the top 8 bits).
pub fn summarize(h: Vec<u64>, threads: usize) -> (u64, u64, u64) {
    let n = h.len() as u64;
    let mut buckets: Vec<Vec<u64>> = (0..256).map(|_| Vec::new()).collect();
    {
        let mut cnt = [0usize; 256];
        for &x in &h {
            cnt[(x >> 56) as usize] += 1;
        }
        for (b, c) in buckets.iter_mut().zip(cnt) {
            b.reserve_exact(c);
        }
        for &x in &h {
            buckets[(x >> 56) as usize].push(x);
        }
    }
    drop(h);
    let next = std::sync::atomic::AtomicUsize::new(0);
    let cells: Vec<std::sync::Mutex<Vec<u64>>> = buckets.into_iter().map(std::sync::Mutex::new).collect();
    let (distinct, sum) = std::thread::scope(|s| {
        let hs: Vec<_> = (0..threads)
            .map(|_| {
                let (cells, next) = (&cells, &next);
                s.spawn(move || {
                    let (mut d, mut sum) = (0u64, 0u64);
                    loop {
                        let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        let Some(c) = cells.get(i) else { break };
                        let mut v = std::mem::take(&mut *c.lock().unwrap());
                        v.sort_unstable();
                        v.dedup();
                        d += v.len() as u64;
                        sum = v.iter().fold(sum, |a, &x| a.wrapping_add(x));
                    }
                    (d, sum)
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().unwrap()).fold((0u64, 0u64), |a, b| (a.0 + b.0, a.1.wrapping_add(b.1)))
    });
    (n, distinct, sum)
}

/// This process's AnonHugePages (kB), from /proc/self/smaps_rollup.
pub fn anon_huge_kb() -> u64 {
    std::fs::read_to_string("/proc/self/smaps_rollup")
        .ok()
        .and_then(|s| s.lines().find(|l| l.starts_with("AnonHugePages:")).and_then(|l| l.split_whitespace().nth(1)?.parse().ok()))
        .unwrap_or(0)
}

/// madvise(MADV_HUGEPAGE) over the pages of `[p, p + len)`.
pub fn advise_huge(p: *const u8, len: usize) {
    extern "C" {
        fn madvise(addr: *mut core::ffi::c_void, len: usize, advice: i32) -> i32;
    }
    let a = (p as usize) & !4095;
    let end = p as usize + len;
    unsafe {
        madvise(a as *mut _, end - a, 14);
    }
}
