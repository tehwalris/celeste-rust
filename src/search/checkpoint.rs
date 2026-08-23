//! Checkpointing for the abstract search (`AbstractRun`).
//!
//! Format (see plans/refinement-plan.md): JSON strictly for metadata, dense
//! binary for the big tables.
//!
//! * `meta.json` - format version, config fingerprint, frame number, counts,
//!   watermarks, per-file byte lengths. Human-readable; everything a reader
//!   needs to validate before touching a byte of binary.
//! * `visited.bin` - the row table's 128-bit keys as two columnar u64-LE
//!   arrays in dense-id order (the id is the array index, so it is
//!   implicit). Only written by the in-RAM map engine; the mmap engine's
//!   keys live in the shared `frames/*.rowkeys` files instead
//!   (`interpreter::visited`), so each checkpoint dir stops duplicating
//!   the full key set (12.4 GB x 19 dirs at room (2,0) f073).
//! * `states.bin` - the boundary states (serde/bincode).
//!
//! Every `.bin` starts with an 16-byte header (magic + format version +
//! payload length) followed by one zstd frame with zstd's content checksum
//! enabled. The reader refuses ANY mismatch - magic, version, fingerprint,
//! byte lengths, row counts - loudly. "Won't be read wrong across versions"
//! is achieved by strict refusal, not by migration cleverness.
//!
//! A checkpoint is written atomically per directory: files land in
//! `<dir>/tmp-f<N>/` and the directory is renamed to `<dir>/f<N>/` last, so
//! a crash mid-write never leaves a loadable-looking half checkpoint.

use anyhow::{anyhow, Context, Result};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use crate::interpreter::row_table::RowTable;
use crate::interpreter::state::State;

const MAGIC: &[u8; 4] = b"C8TB";
/// Bump whenever the meaning or layout of ANY checkpoint content changes,
/// including the serde shape of `State`.
///
/// Version 3: the 2026-08 hash-breaking batch - `sin` registered
/// unconditionally (every state gains a builtin global, changing every
/// row hash), compile-time builtin pinning, and the fingerprint computed
/// from `CampaignConfig` with every field hashed unconditionally.
///
/// Version 4: the row key became order-independent
/// (`virtual_merge::row_key_hashes`) so a state's uniform columns are
/// folded once instead of once per lane. Every key in every visited set
/// changes, so a v3 checkpoint must be refused rather than resumed - its
/// row ids would refer to keys this build can no longer compute.
/// 4 -> 5: provenance hints (`Nil(Some(_))`, `NilPointer(name)`) are
/// erased at the frame boundary (`erase_provenance_hints`), so row keys
/// differ from the first hinted nil onward - room (1,0) diverges at f25.
/// Not a serde change; the bump exists to refuse pre-erasure checkpoints,
/// whose trajectories are a (very slightly) different search.
pub const FORMAT_VERSION: u32 = 5;

#[derive(serde::Serialize, serde::Deserialize)]
pub struct Meta {
    pub format_version: u32,
    /// Fingerprint of everything that determines the search's trajectory:
    /// recipe text, lua sources, partition cells, and the relevant env
    /// flags. A resume under a different configuration must fail loudly.
    pub fingerprint: String,
    /// The last completed frame.
    pub frame: u32,
    pub row_count: u64,
    pub watermarks: Vec<u32>,
    pub state_count: u64,
    pub lane_count: u64,
    pub deopt_states: u64,
    pub deopt_lanes: u64,
    /// `Some` = the visited keys are in this dir's `visited.bin` (the
    /// historic layout). `None` = they are in the shared
    /// `frames/*.rowkeys` files (see `interpreter::visited`), and this
    /// dir carries no per-checkpoint copy of them. Old binaries refuse
    /// new metas (the field is required for them); new binaries read
    /// both.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub visited_bin_len: Option<u64>,
    pub states_bin_len: u64,
}

/// Everything that determines the search trajectory, read from the
/// environment once per call. The fingerprint is computed FROM this
/// struct, every field hashed unconditionally by value - if something can
/// change the reachable set, it belongs in here; if it is in here, two
/// runs that disagree on it can never share checkpoints. (The old
/// fingerprint hashed env-var *presence* and skipped default values for
/// back-compat, which is how probe commands run without the campaign env
/// produced valid-looking but mismatching hashes - twice.)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CampaignConfig {
    /// The start room defines the whole search universe (lua
    /// substitution, collision cache, win predicate).
    pub start_room: (i16, i16),
    /// The rem abstraction level (the refinement ladder's k).
    pub precision: crate::interpreter::abstraction::RemPrecision,
    /// The spd rung below level 0 (plans/spd-rung.md). Exact for every
    /// campaign that predates the rung; hashed unconditionally like
    /// everything else here.
    pub spd_precision: crate::interpreter::abstraction::SpdPrecision,
    /// Frontier-only search (subtract-visited); CELESTE_FRONTIER_ONLY.
    pub frontier_only: bool,
    /// Deopt frames run collect-first; CELESTE_DEOPT_COLLECT_FIRST.
    pub deopt_collect_first: bool,
    /// The lane cap `chunk_states` will use, and the fruit-shape cap
    /// (CELESTE_MAX_STATE_LANES, CELESTE_FRUIT_CHUNK_LANES).
    ///
    /// CHUNKING IS SEMANTIC on rooms with fruit: a comparison against a
    /// widened interval that straddles in any lane of a chunk used to
    /// collapse to a whole-value `UnknownBool` and send the entire chunk
    /// down both edges, so how lanes are grouped decided how coarse the
    /// over-approximation was. The partition fixes remove that mechanism,
    /// but "much less left to change" is not "nothing", and the caps also
    /// steer the deopt and the merge. Two runs that disagree on them must
    /// never share checkpoints.
    ///
    /// Stored as the EFFECTIVE values, not env-var presence, because the
    /// default is a function of the thread count: 1M serial, 8k parallel.
    /// A run that sets neither variable still has a definite cap, and two
    /// runs at different thread counts differ without either naming a
    /// chunk setting - which presence-hashing would have missed, the same
    /// way it missed things twice before.
    pub max_state_lanes: usize,
    pub fruit_chunk_lanes: usize,
    /// The compiled-forward engine's identity (CELESTE_COMPILED_FORWARD):
    /// `None` when the frame body is the interpreter (the legacy
    /// fingerprint - existing interpreter checkpoints stay valid), else a
    /// hash of the compile recipe text plus the fused artifact's embedded
    /// self-fingerprint when the `fused` feature is active. Two runs whose
    /// frame bodies are different ENGINES must never share checkpoints,
    /// even though a gated engine's row sets are identical - the gate is
    /// empirical and per-artifact, not structural. Consequence: gates that
    /// resume an interpreter checkpoint under the compiled engine now
    /// re-run from frame 1 instead (2 min at H=68; correctness beats
    /// convenience).
    pub compiled_engine: Option<u64>,
    /// A SYNTHETIC win target (CELESTE_WIN_AT_XY), for cheap pipeline tests.
    /// A run that finishes at an arbitrary position is a DIFFERENT SEARCH -
    /// different absorbing set, different B(H) seed, different g - so its
    /// artifacts must never be resumable from, or comparable to, a real
    /// campaign's. Hashing it is what makes that impossible rather than
    /// merely discouraged.
    pub synthetic_win: Option<(i16, i16)>,
}

impl CampaignConfig {
    pub fn from_env() -> Self {
        Self {
            start_room: crate::game_runner::start_room(),
            precision: crate::interpreter::abstraction::rem_precision_from_env(),
            spd_precision: crate::interpreter::abstraction::spd_precision_from_env(),
            frontier_only: std::env::var_os("CELESTE_FRONTIER_ONLY").is_some(),
            deopt_collect_first: std::env::var_os("CELESTE_DEOPT_COLLECT_FIRST")
                .is_some(),
            max_state_lanes: crate::search::run::effective_chunk_cap(),
            fruit_chunk_lanes: crate::search::run::effective_fruit_chunk_cap(),
            compiled_engine: Self::compiled_engine_fingerprint(),
            synthetic_win: crate::interpreter::abstraction::synthetic_win_xy(),
        }
    }

    /// See the `compiled_engine` field. Reads the compile recipe TEXT (the
    /// class kernels are byte-gated against it by `generated_is_current` +
    /// the suite, so the text is a faithful proxy for them); the fused
    /// artifact contributes its own embedded hash because it is generated
    /// per campaign and never checked in, and the traced set contributes
    /// one because no file the fingerprint reads determines it.
    fn compiled_engine_fingerprint() -> Option<u64> {
        match std::env::var("CELESTE_COMPILED_FORWARD") {
            Err(_) => None,
            Ok(v) if v == "0" => None,
            Ok(_) => {
                use std::hash::{Hash, Hasher};
                let mut h = rustc_hash::FxHasher::default();
                std::fs::read_to_string("rewrites-compile.jsonl")
                    .unwrap_or_default()
                    .hash(&mut h);
                crate::compiled::dispatch::fused_artifact_fingerprint().hash(&mut h);
                crate::compiled::dispatch::traced_set_fingerprint().hash(&mut h);
                Some(h.finish())
            }
        }
    }
}

/// What determines the search trajectory; hashed into the fingerprint.
pub fn config_fingerprint(recipe_text: &str) -> String {
    config_fingerprint_for(recipe_text, &CampaignConfig::from_env())
}

/// `config_fingerprint` for an explicit precision level - the band loader
/// validates the PREVIOUS level's checkpoints, whose fingerprint differs
/// from the current run's only in the precision components.
pub fn config_fingerprint_with_precision(
    recipe_text: &str,
    precision: crate::interpreter::abstraction::LadderPrecision,
) -> String {
    let mut config = CampaignConfig::from_env();
    config.precision = precision.rem;
    config.spd_precision = precision.spd;
    config_fingerprint_for(recipe_text, &config)
}

/// Every fingerprint THIS campaign would have had at a precision level
/// coarser than or equal to the current one, coarsest first.
///
/// For artifacts a coarser level may lawfully share with a finer one (the
/// position graph; see `sweep_time::borrow_pos_graph`). A fingerprint is a
/// hash, so "differs only in the precision components" cannot be read off
/// it - but the level space is 238 entries, so the honest check is to
/// enumerate them. Everything else the fingerprint covers still has to
/// match EXACTLY, which is the point: this loosens precision and nothing
/// else.
pub fn coarser_precision_fingerprints(
    recipe_text: &str,
) -> Vec<(crate::interpreter::abstraction::LadderPrecision, String)> {
    coarser_precision_fingerprints_for(recipe_text, &CampaignConfig::from_env())
}

pub fn coarser_precision_fingerprints_for(
    recipe_text: &str,
    base: &CampaignConfig,
) -> Vec<(crate::interpreter::abstraction::LadderPrecision, String)> {
    use crate::interpreter::abstraction::LadderPrecision;
    let here = LadderPrecision { spd: base.spd_precision, rem: base.precision };
    LadderPrecision::all()
        .filter(|level| level.coarser_or_equal(here))
        .map(|level| {
            let mut config = base.clone();
            config.precision = level.rem;
            config.spd_precision = level.spd;
            (level, config_fingerprint_for(recipe_text, &config))
        })
        .collect()
}

pub fn config_fingerprint_for(recipe_text: &str, config: &CampaignConfig) -> String {
    use std::hash::{Hash, Hasher};
    let mut h = rustc_hash::FxHasher::default();
    FORMAT_VERSION.hash(&mut h);
    recipe_text.hash(&mut h);
    for path in ["lua/builtin_level_3.lua", "lua/builtin_level_4.lua", "lua/celeste-minimal.lua"] {
        std::fs::read_to_string(path).unwrap_or_default().hash(&mut h);
    }
    config.start_room.hash(&mut h);
    format!("{:?}", config.precision).hash(&mut h);
    format!("{:?}", config.spd_precision).hash(&mut h);
    config.frontier_only.hash(&mut h);
    config.deopt_collect_first.hash(&mut h);
    config.max_state_lanes.hash(&mut h);
    config.fruit_chunk_lanes.hash(&mut h);
    // Conditionally, NOT `Option::hash`: `None` must reproduce the legacy
    // stream byte-for-byte so every existing interpreter checkpoint stays
    // valid. (The usual objection to presence-hashing - that two runs can
    // differ without either naming the setting - does not apply: the
    // engine is exactly named by the value hashed when it is on.)
    if let Some(engine) = config.compiled_engine {
        engine.hash(&mut h);
    }
    config.synthetic_win.hash(&mut h);
    format!("{:016x}", h.finish())
}

fn write_bin(path: &Path, payload_writer: impl FnOnce(&mut dyn Write) -> Result<()>) -> Result<u64> {
    let file = std::fs::File::create(path)?;
    let mut file = std::io::BufWriter::new(file);
    file.write_all(MAGIC)?;
    file.write_all(&FORMAT_VERSION.to_le_bytes())?;
    // Placeholder for the payload length; patched after writing.
    file.write_all(&0u64.to_le_bytes())?;
    let mut counter = CountingWriter { inner: &mut file, written: 0 };
    let mut zw = zstd::Encoder::new(&mut counter, 1)?;
    zw.include_checksum(true)?;
    payload_writer(&mut zw)?;
    zw.finish()?;
    let payload_len = counter.written;
    let mut file = file.into_inner().map_err(|e| anyhow!("flush: {}", e))?;
    use std::io::Seek;
    file.seek(std::io::SeekFrom::Start(8))?;
    file.write_all(&payload_len.to_le_bytes())?;
    file.flush()?;
    Ok(payload_len)
}

struct CountingWriter<W: Write> {
    inner: W,
    written: u64,
}

impl<W: Write> Write for CountingWriter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let n = self.inner.write(buf)?;
        self.written += n as u64;
        Ok(n)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}

/// Save one frame's post-subtract boundary states (`<dir>/frames/fNNN.bin`).
/// Self-contained: header + zstd content checksum; no meta entry.
pub fn save_frame_states(dir: &Path, frame: u32, states: &[State]) -> Result<()> {
    let fdir = dir.join("frames");
    std::fs::create_dir_all(&fdir)?;
    let tmp = fdir.join(format!("tmp-f{:03}.bin", frame));
    write_bin(&tmp, |w| bincode::serialize_into(w, states).context("serializing frame states"))?;
    std::fs::rename(&tmp, fdir.join(format!("f{:03}.bin", frame)))?;
    Ok(())
}

/// Load one frame's saved boundary states.
pub fn load_frame_states(dir: &Path, frame: u32) -> Result<Vec<State>> {
    let path = dir.join("frames").join(format!("f{:03}.bin", frame));
    let r = read_bin_header_len(&path)?;
    bincode::deserialize_from(r).with_context(|| format!("deserializing {}", path.display()))
}

/// `read_bin` trusting the header's own payload length (for self-contained
/// files that have no meta.json entry); integrity comes from the zstd
/// content checksum.
fn read_bin_header_len(path: &Path) -> Result<impl Read> {
    let mut file = std::io::BufReader::new(std::fs::File::open(path)?);
    let mut magic = [0u8; 4];
    file.read_exact(&mut magic)?;
    if &magic != MAGIC {
        return Err(anyhow!("{}: bad magic", path.display()));
    }
    let mut buf4 = [0u8; 4];
    file.read_exact(&mut buf4)?;
    let version = u32::from_le_bytes(buf4);
    if version != FORMAT_VERSION {
        return Err(anyhow!(
            "{}: format version {} != expected {}",
            path.display(),
            version,
            FORMAT_VERSION
        ));
    }
    let mut buf8 = [0u8; 8];
    file.read_exact(&mut buf8)?;
    Ok(zstd::Decoder::new(file)?)
}

fn read_bin(path: &Path, expected_payload_len: u64) -> Result<impl Read> {
    let mut file = std::io::BufReader::new(std::fs::File::open(path)?);
    let mut magic = [0u8; 4];
    file.read_exact(&mut magic)?;
    if &magic != MAGIC {
        return Err(anyhow!("{}: bad magic", path.display()));
    }
    let mut buf4 = [0u8; 4];
    file.read_exact(&mut buf4)?;
    let version = u32::from_le_bytes(buf4);
    if version != FORMAT_VERSION {
        return Err(anyhow!(
            "{}: format version {} != expected {}",
            path.display(),
            version,
            FORMAT_VERSION
        ));
    }
    let mut buf8 = [0u8; 8];
    file.read_exact(&mut buf8)?;
    let payload_len = u64::from_le_bytes(buf8);
    if payload_len != expected_payload_len {
        return Err(anyhow!(
            "{}: payload length {} != {} from meta.json",
            path.display(),
            payload_len,
            expected_payload_len
        ));
    }
    Ok(zstd::Decoder::new(file)?)
}

pub struct Checkpoint {
    pub meta: Meta,
    pub visited: RowTable,
    pub states: Vec<State>,
}

/// The meta and the visited TABLE of a checkpoint, without the
/// fingerprint gate. For `migrate-visited` ONLY: migration derives
/// per-frame key files from a dir's own data and cross-checks them
/// against that same dir's table, so a fingerprint (which guards
/// cross-CONFIGURATION mixing) protects nothing here - and requiring it
/// would force the campaign env to be reconstructed just to convert old
/// artifacts. Format version and byte lengths are still refused loudly.
pub fn load_meta_and_table_unvalidated(dir: &Path, frame: u32) -> Result<(Meta, RowTable)> {
    let cdir = dir.join(format!("f{:03}", frame));
    let meta: Meta = serde_json::from_str(
        &std::fs::read_to_string(cdir.join("meta.json"))
            .with_context(|| format!("reading {}/meta.json", cdir.display()))?,
    )?;
    if meta.format_version != FORMAT_VERSION {
        return Err(anyhow!(
            "checkpoint format version {} != expected {}",
            meta.format_version,
            FORMAT_VERSION
        ));
    }
    let Some(len) = meta.visited_bin_len else {
        return Err(anyhow!(
            "{} has no visited.bin - it is already rowkeys-era",
            cdir.display()
        ));
    };
    let mut r = read_bin(&cdir.join("visited.bin"), len)?;
    let mut buf8 = [0u8; 8];
    r.read_exact(&mut buf8)?;
    let count = u64::from_le_bytes(buf8) as usize;
    if count as u64 != meta.row_count {
        return Err(anyhow!("visited.bin row count {} != meta {}", count, meta.row_count));
    }
    let mut lows = vec![0u64; count];
    let mut highs = vec![0u64; count];
    let mut buf = vec![0u8; count * 8];
    r.read_exact(&mut buf)?;
    for (i, chunk) in buf.chunks_exact(8).enumerate() {
        lows[i] = u64::from_le_bytes(chunk.try_into().unwrap());
    }
    r.read_exact(&mut buf)?;
    for (i, chunk) in buf.chunks_exact(8).enumerate() {
        highs[i] = u64::from_le_bytes(chunk.try_into().unwrap());
    }
    let rows_by_id: Vec<(u64, u64)> = lows.into_iter().zip(highs).collect();
    let visited = RowTable::from_parts(rows_by_id, meta.watermarks.clone());
    Ok((meta, visited))
}

/// Write a checkpoint for `frame` under `dir` (atomically, via rename).
///
/// `visited` is `None` for runs without frontier-only search. With the
/// map engine, the full key table is serialized as `visited.bin`; with
/// the mmap engine the keys are already on disk as `frames/*.rowkeys`
/// and only the counts go into the meta.
pub fn save(
    dir: &Path,
    frame: u32,
    fingerprint: &str,
    states: &[State],
    visited: Option<&crate::interpreter::visited::Visited>,
    deopt_events: (usize, usize),
) -> Result<PathBuf> {
    let final_dir = dir.join(format!("f{:03}", frame));
    let tmp_dir = dir.join(format!("tmp-f{:03}", frame));
    if tmp_dir.exists() {
        std::fs::remove_dir_all(&tmp_dir)?;
    }
    std::fs::create_dir_all(&tmp_dir)?;

    let visited_bin_len = match visited.and_then(|v| v.row_table()) {
        Some(table) => {
            let rows = table.rows_by_id();
            Some(
                write_bin(&tmp_dir.join("visited.bin"), |w| {
                    w.write_all(&(rows.len() as u64).to_le_bytes())?;
                    // Columnar: all low halves, then all high halves.
                    for (lo, _) in &rows {
                        w.write_all(&lo.to_le_bytes())?;
                    }
                    for (_, hi) in &rows {
                        w.write_all(&hi.to_le_bytes())?;
                    }
                    Ok(())
                })
                .context("writing visited.bin")?,
            )
        }
        None => None,
    };

    let states_bin_len = write_bin(&tmp_dir.join("states.bin"), |w| {
        bincode::serialize_into(w, states).context("serializing states")
    })
    .context("writing states.bin")?;

    let meta = Meta {
        format_version: FORMAT_VERSION,
        fingerprint: fingerprint.to_string(),
        frame,
        row_count: visited.map_or(0, |v| v.len() as u64),
        watermarks: visited.map_or_else(Vec::new, |v| v.watermarks().to_vec()),
        state_count: states.len() as u64,
        lane_count: states.iter().map(|s| s.vector_size as u64).sum(),
        deopt_states: deopt_events.0 as u64,
        deopt_lanes: deopt_events.1 as u64,
        visited_bin_len,
        states_bin_len,
    };
    std::fs::write(tmp_dir.join("meta.json"), serde_json::to_string_pretty(&meta)?)?;

    if final_dir.exists() {
        std::fs::remove_dir_all(&final_dir)?;
    }
    std::fs::rename(&tmp_dir, &final_dir)?;
    Ok(final_dir)
}

/// The latest complete checkpoint under `dir`, if any.
pub fn latest(dir: &Path) -> Result<Option<u32>> {
    if !dir.exists() {
        return Ok(None);
    }
    let mut best = None;
    for entry in std::fs::read_dir(dir)? {
        let name = entry?.file_name();
        let name = name.to_string_lossy();
        if let Some(rest) = name.strip_prefix('f') {
            if let Ok(frame) = rest.parse::<u32>() {
                if dir.join(&*name).join("meta.json").exists() {
                    best = best.max(Some(frame));
                }
            }
        }
    }
    Ok(best)
}

/// Load and validate the checkpoint for `frame`. Refuses on any mismatch.
pub fn load(dir: &Path, frame: u32, fingerprint: &str) -> Result<Checkpoint> {
    let cdir = dir.join(format!("f{:03}", frame));
    let meta: Meta = serde_json::from_str(
        &std::fs::read_to_string(cdir.join("meta.json"))
            .with_context(|| format!("reading {}/meta.json", cdir.display()))?,
    )?;
    if meta.format_version != FORMAT_VERSION {
        return Err(anyhow!(
            "checkpoint format version {} != expected {}",
            meta.format_version,
            FORMAT_VERSION
        ));
    }
    if meta.fingerprint != fingerprint {
        return Err(anyhow!(
            "checkpoint fingerprint {} != current configuration {} - the recipe, \
             lua sources or search flags differ from the run that wrote it",
            meta.fingerprint,
            fingerprint
        ));
    }
    if meta.frame != frame {
        return Err(anyhow!("checkpoint says frame {}, directory says {}", meta.frame, frame));
    }

    let visited = match meta.visited_bin_len {
        Some(len) => {
            let mut r = read_bin(&cdir.join("visited.bin"), len)?;
            let mut buf8 = [0u8; 8];
            r.read_exact(&mut buf8)?;
            let count = u64::from_le_bytes(buf8) as usize;
            if count as u64 != meta.row_count {
                return Err(anyhow!(
                    "visited.bin row count {} != meta {}",
                    count,
                    meta.row_count
                ));
            }
            let mut lows = vec![0u64; count];
            let mut highs = vec![0u64; count];
            let mut buf = vec![0u8; count * 8];
            r.read_exact(&mut buf)?;
            for (i, chunk) in buf.chunks_exact(8).enumerate() {
                lows[i] = u64::from_le_bytes(chunk.try_into().unwrap());
            }
            r.read_exact(&mut buf)?;
            for (i, chunk) in buf.chunks_exact(8).enumerate() {
                highs[i] = u64::from_le_bytes(chunk.try_into().unwrap());
            }
            let rows_by_id: Vec<(u64, u64)> = lows.into_iter().zip(highs).collect();
            RowTable::from_parts(rows_by_id, meta.watermarks.clone())
        }
        None if meta.row_count == 0 => RowTable::default(),
        None => {
            // The keys live in the shared frames/*.rowkeys files; rebuild
            // the map for consumers that want one (sweep, bands). Checked
            // against the meta's counts and each file's checksum inside.
            let table =
                crate::interpreter::visited::load_rowkeys_table(dir, &meta.watermarks)?;
            if table.len() as u64 != meta.row_count {
                return Err(anyhow!(
                    "rowkeys files hold {} rows but meta says {}",
                    table.len(),
                    meta.row_count
                ));
            }
            table
        }
    };

    let r = read_bin(&cdir.join("states.bin"), meta.states_bin_len)?;
    let states: Vec<State> = bincode::deserialize_from(r).context("deserializing states")?;
    if states.len() as u64 != meta.state_count {
        return Err(anyhow!("states.bin count {} != meta {}", states.len(), meta.state_count));
    }
    let lanes: u64 = states.iter().map(|s| s.vector_size as u64).sum();
    if lanes != meta.lane_count {
        return Err(anyhow!("states.bin lanes {} != meta {}", lanes, meta.lane_count));
    }

    Ok(Checkpoint { meta, visited, states })
}

/// `load` without materializing the visited keys: meta + boundary
/// states only, fully validated. The resume path of the mmap engine
/// uses this - its membership structure comes from the `.rowkeys` files
/// directly, and rebuilding a 25 GiB map just to throw it away was the
/// point of not having one.
pub fn load_light(dir: &Path, frame: u32, fingerprint: &str) -> Result<(Meta, Vec<State>)> {
    let cdir = dir.join(format!("f{:03}", frame));
    let meta: Meta = serde_json::from_str(
        &std::fs::read_to_string(cdir.join("meta.json"))
            .with_context(|| format!("reading {}/meta.json", cdir.display()))?,
    )?;
    if meta.format_version != FORMAT_VERSION {
        return Err(anyhow!(
            "checkpoint format version {} != expected {}",
            meta.format_version,
            FORMAT_VERSION
        ));
    }
    if meta.fingerprint != fingerprint {
        return Err(anyhow!(
            "checkpoint fingerprint {} != current configuration {} - the recipe, \
             lua sources or search flags differ from the run that wrote it",
            meta.fingerprint,
            fingerprint
        ));
    }
    if meta.frame != frame {
        return Err(anyhow!("checkpoint says frame {}, directory says {}", meta.frame, frame));
    }
    let r = read_bin(&cdir.join("states.bin"), meta.states_bin_len)?;
    let states: Vec<State> = bincode::deserialize_from(r).context("deserializing states")?;
    if states.len() as u64 != meta.state_count {
        return Err(anyhow!("states.bin count {} != meta {}", states.len(), meta.state_count));
    }
    let lanes: u64 = states.iter().map(|s| s.vector_size as u64).sum();
    if lanes != meta.lane_count {
        return Err(anyhow!("states.bin lanes {} != meta {}", lanes, meta.lane_count));
    }
    Ok((meta, states))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn base() -> CampaignConfig {
        CampaignConfig {
            start_room: (0, 0),
            precision: crate::interpreter::abstraction::RemPrecision::Bits(0),
            spd_precision: crate::interpreter::abstraction::SpdPrecision::Exact,
            frontier_only: true,
            deopt_collect_first: true,
            max_state_lanes: 8_000,
            fruit_chunk_lanes: 8_000,
            compiled_engine: None,
            synthetic_win: None,
        }
    }

    /// Chunking is SEMANTIC on rooms with fruit, so two runs that group
    /// lanes differently can produce different over-approximations and must
    /// never share checkpoints. Before this, they hashed the same and a
    /// resume across a cap change was silently accepted - and the quiet
    /// direction (a sweep chunked FINER than its forward pass) produces a
    /// SUBSET of the edges, so `g` overestimates and the band prunes viable
    /// rows with nothing reporting it.
    #[test]
    fn the_chunk_caps_change_the_fingerprint() {
        let a = config_fingerprint_for("recipe", &base());

        let mut finer = base();
        finer.max_state_lanes = 4_000;
        assert_ne!(a, config_fingerprint_for("recipe", &finer), "cap must count");

        let mut fruit = base();
        fruit.fruit_chunk_lanes = 1_000;
        assert_ne!(a, config_fingerprint_for("recipe", &fruit), "fruit cap must count");

        // And the same configuration still agrees with itself, or every
        // resume in the campaign would break.
        assert_eq!(a, config_fingerprint_for("recipe", &base()));
    }

    /// The precision exemption a borrowed position graph runs on
    /// (`--pos-graph-from`) must loosen PRECISION AND NOTHING ELSE. It is
    /// stated as an enumeration of levels, so the thing to check is that
    /// the set contains exactly the coarser-or-equal ones and that no
    /// other config difference can sneak in through it.
    #[test]
    fn the_borrow_exemption_covers_precision_and_only_precision() {
        use crate::interpreter::abstraction::{LadderPrecision, RemPrecision, SpdPrecision};
        let mut k4 = base();
        k4.precision = RemPrecision::Bits(4);
        let accepted = coarser_precision_fingerprints_for("recipe", &k4);
        let levels: Vec<LadderPrecision> = accepted.iter().map(|(l, _)| *l).collect();

        // Level 0 - the one the ladder actually shares - and k=4 itself.
        for rem in [RemPrecision::Bits(0), RemPrecision::Bits(4)] {
            assert!(
                levels.contains(&LadderPrecision { spd: SpdPrecision::Exact, rem }),
                "{:?} must be borrowable at k=4",
                rem
            );
        }
        // A FINER level is not: its table is a subset, and a missing pair
        // silently loses predecessors.
        assert!(!levels.iter().any(|l| l.rem == RemPrecision::Bits(5)));
        assert!(!levels.iter().any(|l| l.rem == RemPrecision::Exact));
        // spd Exact is this config's level, so no WidthLog2 rung (all of
        // which are COARSER in spd) may be excluded... they are coarser, so
        // they are all in.
        assert!(levels.contains(&LadderPrecision {
            spd: SpdPrecision::WidthLog2(16),
            rem: RemPrecision::Bits(0)
        }));

        // Nothing else is loosened: a different room, cap or win target
        // produces no fingerprint in the accepted set at ANY level.
        let fps: std::collections::HashSet<&str> =
            accepted.iter().map(|(_, f)| f.as_str()).collect();
        for mut other in [base(), base(), base(), base()].into_iter().enumerate().map(
            |(i, mut c)| {
                match i {
                    0 => c.start_room = (2, 0),
                    1 => c.max_state_lanes = 4_000,
                    2 => c.synthetic_win = Some((64, 44)),
                    _ => c.frontier_only = false,
                }
                c
            },
        ) {
            other.precision = RemPrecision::Bits(0);
            assert!(
                !fps.contains(config_fingerprint_for("recipe", &other).as_str()),
                "a level-0 table from a DIFFERENT search must not be borrowable"
            );
        }
        // ...and a different recipe likewise.
        let mut l0 = k4.clone();
        l0.precision = RemPrecision::Bits(0);
        assert!(!fps.contains(config_fingerprint_for("other recipe", &l0).as_str()));
        assert!(fps.contains(config_fingerprint_for("recipe", &l0).as_str()));
    }

    /// The default cap is a function of the THREAD COUNT (1M serial, 8k
    /// parallel), so two runs can differ without either naming a chunk
    /// setting. That is why the config stores effective values rather than
    /// env-var presence.
    #[test]
    fn the_serial_and_parallel_defaults_are_distinguishable() {
        let mut serial = base();
        serial.max_state_lanes = 1_000_000;
        assert_ne!(
            config_fingerprint_for("recipe", &serial),
            config_fingerprint_for("recipe", &base()),
            "the serial default must not hash like the parallel one"
        );
    }
}
