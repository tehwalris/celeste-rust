# Benchmark Data

Collected on 2024-12-29 with rayon parallelization (32 threads), 100GB memory limit.

## Frame Timing Data

| Frame | Time (seconds) | Expanded States | Notes |
|-------|----------------|-----------------|-------|
| 25 | 0.025 | 24 | First frame with state explosion |
| 26 | 0.15 | 204 | |
| 27 | 0.84 | 878 | |
| 28 | 3.2 | 2,864 | |
| 29 | 8.3 | 7,260 | |
| 30 | 18 | 15,250 | |
| 31 | 30 | 27,024 | |
| 32 | 48 | 44,558 | |
| 36 | 193 | 149,664 | |
| 37 | 331 | 226,473 | |
| 38 | 549 | 340,521 | ~9 minutes |
| 39 | 1030 | 533,960 | ~17 minutes |
| 40 | OOM | - | Hit 100GB memory limit |

## Exponential Fit (frames 30-39)

**Time model:** `t = 5.48e-05 * exp(0.4245 * frame)`
- Growth factor: ~1.53x per frame
- R² = 0.9564

**State count model:** `s = 0.255 * exp(0.3716 * frame)`
- Growth factor: ~1.45x per frame

## Extrapolated Predictions

| Frame | Predicted Time | Predicted States |
|-------|----------------|------------------|
| 40 | 22 minutes | 0.8M |
| 45 | 3 hours | - |
| 50 | 1 day | 34M |
| 60 | 73 days | 1.5B |
| 70 | 14 years | 63B |
| 80 | 976 years | 2.7T |

## Checkpoint Sizes

| Frame | Compressed Size (zstd -19) |
|-------|---------------------------|
| 5-24 | ~2.2 KB each |
| 25 | 2.6 KB |
| 30 | 42 KB |
| 35 | 257 KB |
| 36 | 317 KB |
| 37 | 438 KB |
| 38 | 626 KB |
| 39 | 901 KB |

Total checkpoint storage for frames 5-39: ~2.6 MB

## Profile Analysis (Frame 31, ~22 seconds)

Profiled with `perf record -g --call-graph dwarf -F 99`.

### Breakdown by Category

| Category | % of Time | Main Functions |
|----------|-----------|----------------|
| Rayon/Crossbeam overhead | ~15% | with_handle (9%), try_advance (4.3%), steal (1.6%) |
| im data structure ops | ~11% | bitmap::Iter (5.4%), SparseChunk clone (3.5%), hash_key (2.8%) |
| Heap operations | ~8% | Heap::get_opt (7.3%), map_in_place (0.7%) |
| State clone/drop | ~7% | State::clone (3.5%), drop_in_place (2.2%) |
| Hashing | ~3% | SipHasher::write (2.6%) |
| Vectorization | ~3% | shape_of_state (2.9%) |
| Memory allocation | ~3% | Vec::from_iter (3.4%), malloc/free (1.4%) |
| Actual interpretation | ~2% | interpret_prepared_cfg_inner (1.1%) |

### Key Insights

1. **~15% is parallelism overhead** - crossbeam epoch management is expensive
2. **~11% is im data structure operations** - immutable HashMap/HAMT is costly to iterate/clone
3. **Only ~2% is actual interpretation** - core interpreter logic is NOT the bottleneck
4. **Most time is in state management** - cloning, hashing, dropping, vectorization

### Potential Optimizations

1. Reduce state cloning (use Rc/Arc)
2. Batch parallel work to reduce crossbeam overhead
3. Use FxHash instead of SipHash for im HashMap keys
4. Simplify shape_of_state computation
5. Consider arena allocation
