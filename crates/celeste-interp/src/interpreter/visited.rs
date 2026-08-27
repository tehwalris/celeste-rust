//! The visited-set engine: what replaces the in-RAM row-table hash map.
//!
//! See `plans/visited-redesign.md` for the design and the measurements
//! that shaped it. Summary:
//!
//! * `frames/fNNN.rowkeys` holds frame N's newly-discovered 128-bit row
//!   keys WITH their dense ids, sorted by key, uncompressed (hashes
//!   measured 1.05x compressible - zstd bought nothing, and membership
//!   probing needs random access). The files are mmap'd; their pages are
//!   kernel-evictable cache, not pinned process RSS.
//! * An in-RAM fingerprint-run index (`FpRuns`, 10 B/row against the
//!   map's measured ~33 B/row) answers "which frame might hold this
//!   key". A MISS is exact - key.0 present is necessary for the full key
//!   to be present. A HIT is confirmed INLINE against the home frame's
//!   mmap before any lane is dropped, so no lossy structure ever decides
//!   a drop. At room (2,0) scale a lossy fp64 decision would falsely
//!   drop ~0.4 genuinely-new rows per campaign; confirmed-inline it
//!   drops none, which is exactly today's 128-bit guarantee.
//! * Ids are NOT positional in the saved frame files (the boundary merge
//!   reorders lanes after the subtract), so the id in each record is
//!   recorded at assignment time and is the only portable id record in a
//!   dir without `visited.bin`.
//!
//! `Visited` wraps either engine behind one API so the two can be
//! differentially gated (`CELESTE_VISITED_ENGINE`); the decisions they
//! make are byte-identical by construction - phase 1 is only ever a
//! filter, and the order-preserving serial dedup is authoritative in
//! both.

use anyhow::{anyhow, Context, Result};
use std::io::Write;
use std::path::{Path, PathBuf};

use super::row_table::RowTable;

const ROWKEYS_MAGIC: &[u8; 4] = b"C8RK";
/// Version of the `.rowkeys` layout itself, independent of the
/// checkpoint FORMAT_VERSION (the serde payloads did not change when
/// this file type was introduced).
const ROWKEYS_VERSION: u32 = 1;
const HEADER_BYTES: usize = 32;
const RECORD_BYTES: usize = 24;

/// Env-selected engine for runs WITH a checkpoint dir. `mmap` (the
/// default) is the fp-run + mmap engine; `map` is the historic in-RAM
/// hash map, kept selectable for differential gating. Runs without a
/// dir always use the in-RAM map (the mmap engine's data structure IS
/// the files). Deliberately NOT part of the campaign fingerprint, for
/// the same reason the variant list is not: the trajectories are
/// certified byte-identical and checkpoints are interchangeable.
///
/// Measured at room (1,0) f100->f101 (386M offered lanes, 225M rows):
/// map 110.4s / 17.5 GB peak; mmap 95.9s / 16.5 GB peak, of which the
/// pinned (unreclaimable) share is ~2.3 GB of fp-runs against the map's
/// ~7.4 GB - the rest of the mmap engine's residency is evictable page
/// cache, so under cgroup pressure it degrades to I/O instead of an
/// OOM kill.
pub fn mmap_engine_selected() -> bool {
    match std::env::var("CELESTE_VISITED_ENGINE") {
        Ok(v) if v == "map" => false,
        Ok(v) if v == "mmap" || v.is_empty() => true,
        Ok(v) => panic!("CELESTE_VISITED_ENGINE={} (expected 'map' or 'mmap')", v),
        Err(_) => true,
    }
}

pub fn rowkeys_path(dir: &Path, frame: u32) -> PathBuf {
    dir.join("frames").join(format!("f{:03}.rowkeys", frame))
}

/// Write frame `frame`'s new row keys (given in ID ORDER, with
/// `first_id` the id of the first) as a sorted, mmap-able record file.
/// Atomic via rename, like every other checkpoint artifact.
pub fn save_frame_rowkeys(
    dir: &Path,
    frame: u32,
    keys_in_id_order: &[(u64, u64)],
    first_id: u32,
) -> Result<()> {
    let mut records: Vec<((u64, u64), u32)> = keys_in_id_order
        .iter()
        .enumerate()
        .map(|(i, k)| (*k, first_id + i as u32))
        .collect();
    records.sort_unstable_by_key(|(k, _)| *k);
    let mut body = Vec::with_capacity(records.len() * RECORD_BYTES);
    for ((lo, hi), id) in &records {
        body.extend_from_slice(&lo.to_le_bytes());
        body.extend_from_slice(&hi.to_le_bytes());
        body.extend_from_slice(&id.to_le_bytes());
        body.extend_from_slice(&0u32.to_le_bytes());
    }
    let checksum = xxhash_rust::xxh3::xxh3_64(&body);

    let fdir = dir.join("frames");
    std::fs::create_dir_all(&fdir)?;
    let tmp = fdir.join(format!("tmp-f{:03}.rowkeys", frame));
    {
        let mut w = std::io::BufWriter::new(std::fs::File::create(&tmp)?);
        w.write_all(ROWKEYS_MAGIC)?;
        w.write_all(&ROWKEYS_VERSION.to_le_bytes())?;
        w.write_all(&(records.len() as u64).to_le_bytes())?;
        w.write_all(&checksum.to_le_bytes())?;
        w.write_all(&[0u8; HEADER_BYTES - 24])?;
        w.write_all(&body)?;
        w.flush()?;
    }
    std::fs::rename(&tmp, rowkeys_path(dir, frame))?;
    Ok(())
}

/// Interpolation search on a sorted slice of uniformly-distributed u64s
/// (they are hashes - the textbook best case): first index with
/// `slice[i] >= target`. Expected O(log log n) iterations; a shrink
/// guard falls back to binary halving if the distribution ever
/// misbehaves, so worst case stays O(log n).
fn interp_lower_bound(slice: &[u64], target: u64) -> usize {
    let (mut lo, mut hi) = (0usize, slice.len());
    while lo < hi {
        let first = slice[lo];
        let last = slice[hi - 1];
        if target <= first {
            return lo;
        }
        if target > last {
            return hi;
        }
        // first < target <= last, hi - lo >= 1. Invariant either way:
        // the answer is in [lo, hi], and each step shrinks the window
        // by at least one, so termination is unconditional.
        let span = (last - first) as u128;
        let off = (target - first) as u128;
        let mut mid = lo + ((off * (hi - lo - 1) as u128) / span) as usize;
        mid = mid.clamp(lo, hi - 1);
        if slice[mid] < target {
            lo = mid + 1;
        } else {
            hi = mid;
        }
        // Pathology guard: one standard bisection step per iteration
        // caps the worst case at O(log n) even if the distribution
        // defeats interpolation; on uniform hashes it costs one
        // comparison and rarely changes anything.
        if hi - lo > 8 {
            let half = lo + (hi - lo) / 2;
            if slice[half] < target {
                lo = half + 1;
            } else {
                hi = half;
            }
        }
    }
    lo
}

/// One frame's sorted key records, memory-mapped, plus a PINNED sample
/// index: the key of every `SAMPLE_STRIDE`-th record, held in RAM. A
/// lookup searches the samples without touching the mmap at all, then
/// touches at most one ~3 KB block of the file. Under memory pressure
/// the worst case is therefore ONE page fault per confirm, never a
/// chain of dependent ones.
pub struct FrameKeys {
    mmap: memmap2::Mmap,
    count: usize,
    /// `samples[i]` = key.0 of record `i * SAMPLE_STRIDE` (key.0 alone
    /// is enough to bracket; the block scan compares full keys).
    samples: Vec<u64>,
}

/// 128 records x 24 B = 3 KB per block: within one 4 KB page most of
/// the time, never more than two. RAM cost: 8 B per 128 records =
/// ~0.03% of the file.
const SAMPLE_STRIDE: usize = 128;

impl FrameKeys {
    /// Open and validate the header (magic, version, count vs file
    /// length). The content checksum is NOT verified here - it is
    /// verified by the one consumer that reads the file end-to-end
    /// anyway (`verify_checksum`, called from the run-index rebuild).
    pub fn open(dir: &Path, frame: u32) -> Result<Self> {
        let path = rowkeys_path(dir, frame);
        let file = std::fs::File::open(&path)
            .with_context(|| format!("opening {} (run `rewrite migrate-visited` for pre-rowkeys dirs)", path.display()))?;
        // Safety: the file is opened read-only and never truncated by
        // this process; every write path goes through tmp + rename.
        let mmap = unsafe { memmap2::Mmap::map(&file)? };
        if mmap.len() < HEADER_BYTES {
            return Err(anyhow!("{}: truncated header", path.display()));
        }
        if &mmap[0..4] != ROWKEYS_MAGIC {
            return Err(anyhow!("{}: bad magic", path.display()));
        }
        let version = u32::from_le_bytes(mmap[4..8].try_into().unwrap());
        if version != ROWKEYS_VERSION {
            return Err(anyhow!(
                "{}: rowkeys version {} != expected {}",
                path.display(),
                version,
                ROWKEYS_VERSION
            ));
        }
        let count = u64::from_le_bytes(mmap[8..16].try_into().unwrap()) as usize;
        let expect = HEADER_BYTES + count * RECORD_BYTES;
        if mmap.len() != expect {
            return Err(anyhow!(
                "{}: {} bytes for {} records (expected {})",
                path.display(),
                mmap.len(),
                count,
                expect
            ));
        }
        let mut samples = Vec::with_capacity(count.div_ceil(SAMPLE_STRIDE));
        for i in (0..count).step_by(SAMPLE_STRIDE) {
            let off = HEADER_BYTES + i * RECORD_BYTES;
            samples.push(u64::from_le_bytes(mmap[off..off + 8].try_into().unwrap()));
        }
        Ok(Self { mmap, count, samples })
    }

    pub fn count(&self) -> usize {
        self.count
    }

    /// All `(key, id)` records in file order (ascending by key). For
    /// gates that cross-check a frame's RECORDED keys against a
    /// recomputation (native-probe `--key-gate`, D1).
    pub fn iter(&self) -> impl Iterator<Item = ((u64, u64), u32)> + '_ {
        (0..self.count).map(|i| self.record(i))
    }

    fn record(&self, i: usize) -> ((u64, u64), u32) {
        let off = HEADER_BYTES + i * RECORD_BYTES;
        let b = &self.mmap[off..off + RECORD_BYTES];
        (
            (
                u64::from_le_bytes(b[0..8].try_into().unwrap()),
                u64::from_le_bytes(b[8..16].try_into().unwrap()),
            ),
            u32::from_le_bytes(b[16..20].try_into().unwrap()),
        )
    }

    /// Exact confirm for the FULL 128-bit key, behind every fp hit.
    ///
    /// The pinned sample index brackets the key to one
    /// `SAMPLE_STRIDE`-record block WITHOUT touching the mmap, then one
    /// block is scanned. Keys sharing a key.0 can straddle a sample
    /// boundary, so the scan follows equal-key.0 runs across it.
    pub fn contains(&self, key: (u64, u64)) -> Option<u32> {
        if self.count == 0 {
            return None;
        }
        // First sample >= key.0, then back up one block: the key (or the
        // start of its key.0 run) lives at or after samples[b-1]'s block.
        let s = interp_lower_bound(&self.samples, key.0);
        let start = s.saturating_sub(1) * SAMPLE_STRIDE;
        let mut i = start;
        // Skip records below key.0 within the block (at most one block,
        // by the bracketing above).
        let end = (start + 2 * SAMPLE_STRIDE).min(self.count);
        while i < end {
            let (k, id) = self.record(i);
            match k.0.cmp(&key.0) {
                std::cmp::Ordering::Less => i += 1,
                std::cmp::Ordering::Greater => return None,
                std::cmp::Ordering::Equal => {
                    // Walk the equal-key.0 run (usually length 1);
                    // records are sorted by (key.0, key.1).
                    let mut j = i;
                    loop {
                        let (k, id2) = if j == i { (k, id) } else { self.record(j) };
                        if k.0 != key.0 || k.1 > key.1 {
                            return None;
                        }
                        if k.1 == key.1 {
                            return Some(id2);
                        }
                        j += 1;
                        if j >= self.count {
                            return None;
                        }
                    }
                }
            }
        }
        // key.0 >= everything in the block span: only possible when the
        // run continues past it (pathological duplication); fall back.
        self.contains_binary(key)
    }

    /// Plain binary search - the fallback and the reference the tests
    /// compare the fast path against.
    fn contains_binary(&self, key: (u64, u64)) -> Option<u32> {
        let (mut lo, mut hi) = (0usize, self.count);
        while lo < hi {
            let mid = lo + (hi - lo) / 2;
            let (k, id) = self.record(mid);
            match k.cmp(&key) {
                std::cmp::Ordering::Less => lo = mid + 1,
                std::cmp::Ordering::Greater => hi = mid,
                std::cmp::Ordering::Equal => return Some(id),
            }
        }
        None
    }

    /// All records, in file (key-sorted) order.
    pub fn records(&self) -> impl Iterator<Item = ((u64, u64), u32)> + '_ {
        (0..self.count).map(move |i| self.record(i))
    }

    /// Verify the xxh3 content checksum (one sequential pass).
    pub fn verify_checksum(&self, what: &str) -> Result<()> {
        let stored = u64::from_le_bytes(self.mmap[16..24].try_into().unwrap());
        let actual = xxhash_rust::xxh3::xxh3_64(&self.mmap[HEADER_BYTES..]);
        if stored != actual {
            return Err(anyhow!("{}: rowkeys checksum mismatch", what));
        }
        Ok(())
    }
}

/// Sorted runs of `(fp64 = key.0, home frame)`, size-tiered so the run
/// count stays O(log frames). A probe visits every entry matching the
/// fp, newest run first.
#[derive(Default)]
pub struct FpRuns {
    /// Parallel arrays per run, each sorted by fp. Newest run last.
    runs: Vec<(Vec<u64>, Vec<u16>)>,
}

impl FpRuns {
    pub fn entries(&self) -> usize {
        self.runs.iter().map(|(f, _)| f.len()).sum()
    }

    /// Visit the home frame of every entry with this fp (there can be
    /// several: distinct rows may share key.0). Returns true if any
    /// visit returned true (used for "confirmed somewhere, stop").
    pub fn probe(&self, fp: u64, mut visit: impl FnMut(u16) -> bool) -> bool {
        for (fps, frames) in self.runs.iter().rev() {
            let start = interp_lower_bound(fps, fp);
            let mut i = start;
            while i < fps.len() && fps[i] == fp {
                if visit(frames[i]) {
                    return true;
                }
                i += 1;
            }
        }
        false
    }

    /// Append one frame's fps (any order) as a new run, merging tiers
    /// so sizes stay geometrically decreasing.
    pub fn push_frame(&mut self, mut fps: Vec<u64>, frame: u16) {
        fps.sort_unstable();
        let frames = vec![frame; fps.len()];
        self.runs.push((fps, frames));
        // Size-tiered compaction: merge while the previous run is not
        // at least twice the newer one, so run sizes decay by >=2x and
        // the run count is O(log total).
        while self.runs.len() >= 2 {
            let n = self.runs.len();
            if self.runs[n - 2].0.len() >= 2 * self.runs[n - 1].0.len() {
                break;
            }
            let (b_fps, b_frames) = self.runs.pop().unwrap();
            let (a_fps, a_frames) = self.runs.pop().unwrap();
            let mut fps = Vec::with_capacity(a_fps.len() + b_fps.len());
            let mut frames = Vec::with_capacity(a_frames.len() + b_frames.len());
            let (mut i, mut j) = (0usize, 0usize);
            while i < a_fps.len() && j < b_fps.len() {
                if a_fps[i] <= b_fps[j] {
                    fps.push(a_fps[i]);
                    frames.push(a_frames[i]);
                    i += 1;
                } else {
                    fps.push(b_fps[j]);
                    frames.push(b_frames[j]);
                    j += 1;
                }
            }
            fps.extend_from_slice(&a_fps[i..]);
            frames.extend_from_slice(&a_frames[i..]);
            fps.extend_from_slice(&b_fps[j..]);
            frames.extend_from_slice(&b_frames[j..]);
            self.runs.push((fps, frames));
        }
    }
}

/// The mmap-backed engine: fp-runs in RAM, full keys on disk, exact
/// confirm inline. Holds no per-key heap allocation at all.
pub struct MmapVisited {
    dir: PathBuf,
    runs: FpRuns,
    /// `frames[f - 1]` = frame f's mmap'd keys.
    frames: Vec<FrameKeys>,
    watermarks: Vec<u32>,
    row_count: u64,
    /// This boundary's new keys: dedup set + id-order list. Cleared at
    /// `end_frame`. The set is the authoritative within-boundary
    /// first-lane-wins decision, same as the map's `insert_new`.
    batch_set: rustc_hash::FxHashSet<(u64, u64)>,
    batch_order: Vec<(u64, u64)>,
}

impl MmapVisited {
    pub fn create(dir: &Path) -> Self {
        Self {
            dir: dir.to_path_buf(),
            runs: FpRuns::default(),
            frames: Vec::new(),
            watermarks: Vec::new(),
            row_count: 0,
            batch_set: Default::default(),
            batch_order: Vec::new(),
        }
    }

    /// Resume: open every frame's `.rowkeys`, verify counts against the
    /// watermarks and each file's checksum, and rebuild the fp-runs.
    pub fn open(dir: &Path, watermarks: Vec<u32>) -> Result<Self> {
        let mut engine = Self::create(dir);
        for (i, &w) in watermarks.iter().enumerate() {
            let frame = i as u32 + 1;
            let fk = FrameKeys::open(dir, frame)?;
            fk.verify_checksum(&format!("f{:03}.rowkeys", frame))?;
            let prev = if i == 0 { 0 } else { watermarks[i - 1] };
            if fk.count() as u32 != w - prev {
                return Err(anyhow!(
                    "f{:03}.rowkeys has {} records but the watermarks say {}",
                    frame,
                    fk.count(),
                    w - prev
                ));
            }
            // key.0 of a key-sorted file is already fp-sorted; push_frame
            // re-sorts, which is a no-op pass on sorted input.
            let fps: Vec<u64> = fk.records().map(|((lo, _), _)| lo).collect();
            engine.runs.push_frame(fps, u16::try_from(frame).expect("frame fits u16"));
            engine.frames.push(fk);
        }
        engine.row_count = watermarks.last().copied().unwrap_or(0) as u64;
        engine.watermarks = watermarks;
        Ok(engine)
    }

    /// Phase-1 membership: is this key in any completed frame? Exact.
    pub fn contains_historic(&self, key: (u64, u64)) -> bool {
        self.runs.probe(key.0, |frame| {
            self.frames[frame as usize - 1].contains(key).is_some()
        })
    }

    /// CONTRACT: callers only offer keys that passed
    /// `contains_historic` this boundary (phase 1 is EXACT for this
    /// engine, so nothing historic can leak through). The batch set then
    /// resolves within-boundary duplicates, which is the same
    /// first-lane-wins decision the map's `insert_new` makes. Offering
    /// an unfiltered historic key here would assign it a second id.
    fn insert_new(&mut self, key: (u64, u64)) -> bool {
        debug_assert!(
            !self.contains_historic(key),
            "insert_new offered a historic key - the phase-1 filter was skipped"
        );
        if self.batch_set.insert(key) {
            self.batch_order.push(key);
            true
        } else {
            false
        }
    }

    fn end_frame(&mut self) -> Result<()> {
        let frame = self.watermarks.len() as u32 + 1;
        let first_id = self.row_count as u32;
        let keys = std::mem::take(&mut self.batch_order);
        self.batch_set = Default::default();
        save_frame_rowkeys(&self.dir, frame, &keys, first_id)?;
        let fk = FrameKeys::open(&self.dir, frame)?;
        let fps: Vec<u64> = keys.iter().map(|(lo, _)| *lo).collect();
        self.runs.push_frame(fps, u16::try_from(frame).expect("frame fits u16"));
        self.frames.push(fk);
        self.row_count += keys.len() as u64;
        self.watermarks.push(self.row_count as u32);
        Ok(())
    }
}

/// The engine facade the forward pass runs against.
pub struct Visited {
    /// Where `.rowkeys` are written at each boundary; `None` = no
    /// artifacts (the plain runner without a checkpoint dir). The mmap
    /// engine requires a dir - its data structure IS the files.
    dir: Option<PathBuf>,
    engine: Engine,
}

enum Engine {
    Map(RowTable),
    Mmap(MmapVisited),
}

/// Frozen-frontier flush for the MAP engine: buffer a frame's new rows and
/// bulk-flush at `end_frame` instead of inserting incrementally, so
/// mid-frame probes hit a frozen table (the mmap engine already does this).
/// Opt-in via `CELESTE_FRONTIER_BUFFERED=1`; byte-identical either way. Off
/// by default so no running campaign changes without asking.
fn map_buffered_on() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| {
        std::env::var("CELESTE_FRONTIER_BUFFERED").map_or(false, |v| v != "0")
    })
}

impl Visited {
    /// The historic in-RAM engine, no artifacts.
    pub fn in_memory() -> Self {
        let mut table = RowTable::default();
        table.set_buffered(map_buffered_on());
        Self { dir: None, engine: Engine::Map(table) }
    }

    /// The historic engine, writing `.rowkeys` at each boundary so the
    /// artifacts are interchangeable with the mmap engine's.
    pub fn map_with_dir(mut table: RowTable, dir: &Path) -> Self {
        table.set_buffered(map_buffered_on());
        Self { dir: Some(dir.to_path_buf()), engine: Engine::Map(table) }
    }

    /// Fresh mmap engine.
    pub fn mmap_new(dir: &Path) -> Self {
        Self {
            dir: Some(dir.to_path_buf()),
            engine: Engine::Mmap(MmapVisited::create(dir)),
        }
    }

    /// Resumed mmap engine (from a checkpoint's watermarks).
    pub fn mmap_open(dir: &Path, watermarks: Vec<u32>) -> Result<Self> {
        Ok(Self {
            dir: Some(dir.to_path_buf()),
            engine: Engine::Mmap(MmapVisited::open(dir, watermarks)?),
        })
    }

    pub fn is_mmap(&self) -> bool {
        matches!(self.engine, Engine::Mmap(_))
    }

    /// Whether phase 1 should dedup within the chunk BEFORE the global
    /// probe. True for the mmap engine, whose global probe costs several
    /// times a map lookup - see the order discussion in
    /// `vectorize::visited_row_keys`. Filter-order only; the candidate
    /// set is identical either way.
    pub fn local_dedup_first(&self) -> bool {
        self.is_mmap()
    }

    /// Attach (or change) the artifact dir. Used by `bench` once the
    /// checkpoint dir is known - the engine is constructed before it is.
    pub fn set_dir(&mut self, dir: &Path) {
        if let Engine::Mmap(m) = &mut self.engine {
            assert!(
                m.frames.is_empty() && m.watermarks.is_empty(),
                "cannot move an mmap engine's dir after frames were written"
            );
            m.dir = dir.to_path_buf();
        }
        self.dir = Some(dir.to_path_buf());
    }

    /// Phase-1 membership against COMPLETED frames (plus, for the map
    /// engine, whatever this boundary already inserted - phase 1 is
    /// only a filter, so the difference cannot change any decision).
    pub fn contains_historic(&self, key: (u64, u64)) -> bool {
        match &self.engine {
            Engine::Map(t) => t.id_of(key).is_some(),
            Engine::Mmap(m) => m.contains_historic(key),
        }
    }

    /// Serial, order-preserving insert: true exactly when the key is
    /// new. This is the authoritative decision in both engines.
    pub fn insert_new(&mut self, key: (u64, u64)) -> bool {
        match &mut self.engine {
            Engine::Map(t) => t.insert_new(key).is_some(),
            Engine::Mmap(m) => m.insert_new(key),
        }
    }

    /// Close the frame: persist the new keys (when a dir is attached),
    /// advance the watermark.
    pub fn end_frame(&mut self) -> Result<()> {
        match &mut self.engine {
            Engine::Map(t) => {
                let (keys, first_id) = if t.is_buffered() {
                    // Frozen-frontier path: bulk-flush this frame's buffered
                    // rows into the table now (they were probe-invisible all
                    // frame). Same keys, same discovery-order ids.
                    t.end_frame_buffered()
                } else {
                    let keys = t.take_recent();
                    let first_id = t.len() as u32 - keys.len() as u32;
                    t.end_frame();
                    (keys, first_id)
                };
                if let Some(dir) = &self.dir {
                    let frame = t.watermarks().len() as u32;
                    save_frame_rowkeys(dir, frame, &keys, first_id)?;
                }
                Ok(())
            }
            Engine::Mmap(m) => m.end_frame(),
        }
    }

    pub fn len(&self) -> usize {
        match &self.engine {
            Engine::Map(t) => t.len(),
            Engine::Mmap(m) => m.row_count as usize,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn watermarks(&self) -> &[u32] {
        match &self.engine {
            Engine::Map(t) => t.watermarks(),
            Engine::Mmap(m) => &m.watermarks,
        }
    }

    /// The in-RAM table, when this is the map engine - what
    /// `checkpoint::save` serializes as `visited.bin`. The mmap engine
    /// has no map; its checkpoint is the `.rowkeys` files themselves.
    pub fn row_table(&self) -> Option<&RowTable> {
        match &self.engine {
            Engine::Map(t) => Some(t),
            Engine::Mmap(_) => None,
        }
    }
}

/// Rebuild a full `RowTable` map from a dir's `.rowkeys` files - the
/// compatibility path for consumers that want the map (sweep, bands)
/// when `visited.bin` is absent. Verifies every file's checksum and the
/// watermark bijection.
pub fn load_rowkeys_table(dir: &Path, watermarks: &[u32]) -> Result<RowTable> {
    let total = watermarks.last().copied().unwrap_or(0) as usize;
    let mut rows_by_id: Vec<(u64, u64)> = vec![(0, 0); total];
    let mut placed = vec![false; total];
    for (i, &w) in watermarks.iter().enumerate() {
        let frame = i as u32 + 1;
        let prev = if i == 0 { 0 } else { watermarks[i - 1] };
        let fk = FrameKeys::open(dir, frame)?;
        fk.verify_checksum(&format!("f{:03}.rowkeys", frame))?;
        if fk.count() as u32 != w - prev {
            return Err(anyhow!(
                "f{:03}.rowkeys has {} records but the watermarks say {}",
                frame,
                fk.count(),
                w - prev
            ));
        }
        for (key, id) in fk.records() {
            let id = id as usize;
            if id < prev as usize || id >= w as usize {
                return Err(anyhow!(
                    "f{:03}.rowkeys claims id {} outside [{}, {})",
                    frame,
                    id,
                    prev,
                    w
                ));
            }
            if std::mem::replace(&mut placed[id], true) {
                return Err(anyhow!("id {} appears twice in the rowkeys files", id));
            }
            rows_by_id[id] = key;
        }
    }
    Ok(RowTable::from_parts(rows_by_id, watermarks.to_vec()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rowkeys_roundtrip_and_search() {
        let dir = tempdir_labeled("roundtrip");
        let keys = vec![(5, 5), (1, 9), (1, 2), (7, 0)];
        save_frame_rowkeys(&dir, 1, &keys, 10).unwrap();
        let fk = FrameKeys::open(&dir, 1).unwrap();
        fk.verify_checksum("t").unwrap();
        assert_eq!(fk.count(), 4);
        // Ids follow the ID-ORDER input, not the sorted file order.
        assert_eq!(fk.contains((5, 5)), Some(10));
        assert_eq!(fk.contains((1, 9)), Some(11));
        assert_eq!(fk.contains((1, 2)), Some(12));
        assert_eq!(fk.contains((7, 0)), Some(13));
        assert_eq!(fk.contains((1, 3)), None);
        assert_eq!(fk.contains((0, 0)), None);
        assert_eq!(fk.contains((9, 9)), None);
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn fp_runs_probe_and_merge() {
        let mut runs = FpRuns::default();
        runs.push_frame(vec![3, 1, 7], 1);
        runs.push_frame(vec![2, 3], 2);
        runs.push_frame(vec![9], 3);
        let mut homes = Vec::new();
        runs.probe(3, |f| {
            homes.push(f);
            false
        });
        homes.sort_unstable();
        assert_eq!(homes, vec![1, 2], "fp 3 lives in frames 1 and 2");
        assert!(!runs.probe(4, |_| true));
        assert!(runs.probe(9, |f| f == 3));
        assert_eq!(runs.entries(), 6);
        // Compaction happened (all three pushes were similar sizes).
        assert!(runs.runs.len() <= 2, "size-tiering should have merged");
    }

    /// Offer keys the way production does: phase 1 (`contains_historic`,
    /// exact) filters, and only survivors reach the order-preserving
    /// `insert_new`. Returns each key's survives-as-new decision.
    fn offer(engine: &mut Visited, keys: &[(u64, u64)]) -> Vec<bool> {
        keys.iter()
            .map(|k| !engine.contains_historic(*k) && engine.insert_new(*k))
            .collect()
    }

    #[test]
    fn mmap_engine_matches_map_engine_decisions() {
        let dir = tempdir_labeled("decisions");
        let mut map = Visited::in_memory();
        let mut mm = Visited::mmap_new(&dir);
        // Frame 1: three distinct keys, one within-batch dup.
        let f1 = [(1u64, 1u64), (2, 2), (1, 1), (3, 3)];
        // Frame 2: one historic dup, one new, one within-batch dup.
        let f2 = [(2u64, 2u64), (4, 4), (4, 4)];
        for engine in [&mut map, &mut mm] {
            assert_eq!(offer(engine, &f1), vec![true, true, false, true]);
            engine.end_frame().unwrap();
        }
        for engine in [&mut map, &mut mm] {
            assert!(engine.contains_historic((1, 1)));
            assert!(engine.contains_historic((3, 3)));
            assert!(!engine.contains_historic((4, 4)));
            assert_eq!(offer(engine, &f2), vec![false, true, false]);
            engine.end_frame().unwrap();
        }
        assert_eq!(map.len(), mm.len());
        assert_eq!(map.watermarks(), mm.watermarks());
        // The rebuilt map from the mmap engine's files equals the map
        // engine's table, id for id.
        let rebuilt = load_rowkeys_table(&dir, mm.watermarks()).unwrap();
        let table = map.row_table().unwrap();
        assert_eq!(rebuilt.rows_by_id(), table.rows_by_id());
        assert_eq!(rebuilt.watermarks(), table.watermarks());
        // A resumed mmap engine sees the same membership.
        let resumed = Visited::mmap_open(&dir, mm.watermarks().to_vec()).unwrap();
        for k in [(1, 1), (2, 2), (3, 3), (4, 4)] {
            assert!(resumed.contains_historic(k));
        }
        assert!(!resumed.contains_historic((5, 5)));
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn map_engine_with_dir_writes_identical_rowkeys() {
        let dir_a = tempdir_labeled("a");
        let dir_b = tempdir_labeled("b");
        let mut map = Visited::map_with_dir(RowTable::default(), &dir_a);
        let mut mm = Visited::mmap_new(&dir_b);
        for engine in [&mut map, &mut mm] {
            offer(engine, &[(9u64, 9u64), (8, 8)]);
            engine.end_frame().unwrap();
        }
        let a = std::fs::read(rowkeys_path(&dir_a, 1)).unwrap();
        let b = std::fs::read(rowkeys_path(&dir_b, 1)).unwrap();
        assert_eq!(a, b, "the two engines' rowkeys files must be byte-identical");
        std::fs::remove_dir_all(&dir_a).unwrap();
        std::fs::remove_dir_all(&dir_b).unwrap();
    }

    /// The tuned probe paths must agree with their reference
    /// implementations everywhere: interp_lower_bound vs
    /// partition_point, and the sample-index contains() vs plain binary
    /// search - including keys that share key.0 (forced duplicates) and
    /// keys absent on either side of every present key.
    #[test]
    fn tuned_probes_match_reference() {
        // Deterministic LCG so the test is reproducible.
        let mut x: u64 = 0x243f_6a88_85a3_08d3;
        let mut next = move || {
            x = x.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            x
        };
        for n in [0usize, 1, 2, 127, 128, 129, 1000, 5000] {
            let mut fps: Vec<u64> = (0..n).map(|_| next() >> 8).collect();
            fps.sort_unstable();
            for _ in 0..500 {
                let t = next() >> 8;
                assert_eq!(
                    interp_lower_bound(&fps, t),
                    fps.partition_point(|&v| v < t),
                    "lower bound diverged at n={}",
                    n
                );
            }
            for &v in fps.iter().take(50) {
                for t in [v.wrapping_sub(1), v, v.wrapping_add(1)] {
                    assert_eq!(
                        interp_lower_bound(&fps, t),
                        fps.partition_point(|&x| x < t)
                    );
                }
            }
        }

        let dir = tempdir_labeled("tuned");
        // Keys with forced key.0 collisions so equal-key.0 runs cross
        // sample-block boundaries.
        let mut keys: Vec<(u64, u64)> = (0..4000u64)
            .map(|_| ((next() % 512) << 32, next()))
            .collect();
        keys.sort_unstable();
        keys.dedup();
        let n = keys.len();
        // save_frame_rowkeys wants id order = arbitrary; use as-is.
        save_frame_rowkeys(&dir, 1, &keys, 0).unwrap();
        let fk = FrameKeys::open(&dir, 1).unwrap();
        assert_eq!(fk.count(), n);
        for (i, &k) in keys.iter().enumerate() {
            assert_eq!(fk.contains(k), fk.contains_binary(k), "present key {}", i);
            assert!(fk.contains(k).is_some());
            for probe in [(k.0, k.1.wrapping_add(1)), (k.0.wrapping_add(1), k.1), (k.0, k.1.wrapping_sub(1))] {
                assert_eq!(fk.contains(probe), fk.contains_binary(probe));
            }
        }
        std::fs::remove_dir_all(&dir).unwrap();
    }

    fn tempdir_labeled(label: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "celeste-visited-test-{}-{:?}-{}",
            std::process::id(),
            std::thread::current().id(),
            label
        ));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }
}
