//! THE STATISTICS: a registry (`registry!`) of every named count, size and
//! time a phase reports, and its `Stats`. The forward's (`S`): each wave
//! worker keeps its own `Stats` (its unit sink's: no atomics in the hot
//! loop), merged into the frame's at its end; everything that reports a
//! frame renders the registry: the `[fwd]` line, the tree's `forward.tsv`,
//! `bench frame`'s `[cost]` and its exact metrics. The arc phase's
//! (`backward::A`): the `[arc]` lines.
//!
//! A stat's NAME is its column (`forward.tsv`, which `export-ui` reads by
//! name) and says its rendered unit: `_ms` a time, `_gb` a size, `_s` a
//! phase's worker-seconds (`--phases`, TSC ticks: off, a timer is a branch).

/// How a stat is kept and shown.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Unit {
    /// A number.
    Count,
    /// Bytes, shown in GB.
    Bytes,
    /// A wall time (ns), shown in ms.
    Wall,
    /// Time summed over the workers (ns), shown in ms.
    Work,
    /// `--phases`: TSC ticks summed over the workers, shown in seconds.
    Ticks,
    /// A number, merged as the largest.
    Max,
}

/// A registry's statistics (`registry!`), each its index in its `Stats`.
pub trait Registry: Copy + 'static {
    /// The stats, in their order (the columns').
    const ALL: &'static [Self];
    fn index(self) -> usize;
    fn name(self) -> &'static str;
    fn unit(self) -> Unit;
    /// The element a time's cost is per (`None`: none).
    fn per(self) -> Option<Self>;
}

/// A REGISTRY: `NAME { Variant "column_name" Unit Per; ... }` makes the
/// enum `NAME` of its statistics (`Registry`; `Per`: what a time's cost is
/// per, itself for none).
macro_rules! registry {
    ($(#[$m:meta])* $e:ident { $($v:ident $name:literal $unit:ident $per:ident;)* }) => {
        $(#[$m])*
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(usize)]
        pub enum $e {
            $($v,)*
        }
        impl $crate::storage::stats::Registry for $e {
            const ALL: &'static [$e] = &[$($e::$v,)*];
            fn index(self) -> usize {
                self as usize
            }
            fn name(self) -> &'static str {
                [$($name,)*][self as usize]
            }
            fn unit(self) -> $crate::storage::stats::Unit {
                [$($crate::storage::stats::Unit::$unit,)*][self as usize]
            }
            fn per(self) -> Option<$e> {
                let p = [$($e::$per,)*][self as usize];
                (p != self).then_some(p)
            }
        }
    };
}
pub(crate) use registry;

/// The forward's stats, in their order (the columns').
pub const ALL: &[S] = <S as Registry>::ALL;
/// The forward's stat count.
pub const N: usize = ALL.len();

registry! {
    /// Every statistic of a forward frame.
    S {
    // The frontier in, what it emitted and pushed, the frontier out.
    EntriesIn "entries_in" Count EntriesIn;
    LanesIn "lanes_in" Count LanesIn;
    OldIn "old_in" Count OldIn;
    Emissions "lanes_raw" Count Emissions;
    RepushEdges "repush_edges" Count RepushEdges;
    Pushes "pushes" Count Pushes;
    Touched "touched" Count Touched;
    Kept "lanes_kept" Count Kept;
    Grown "grown" Count Grown;
    EntriesOut "entries_out" Count EntriesOut;
    LanesOut "lanes_out" Count LanesOut;
    Visited "visited" Count Visited;
    // The pipeline's units, slices and routes; the edges recorded.
    Units "units" Count Units;
    Slices "slices" Count Slices;
    Routes "routes" Count Routes;
    Edges "edges" Count Edges;
    Bundles "bundles" Count Bundles;
    WaitedSegments "waited_segments" Count WaitedSegments;
    WaitedBundles "waited_bundles" Count WaitedBundles;
    // The kernels' calls (`compiled::print_kernel_stats`): the calls, the
    // rows they ran, their 16-lane slices, the (body, slice) evaluations
    // and those that took a lane, the lanes the bodies took (before level
    // -1; `lanes_raw` after it).
    KernelCalls "kernel_calls" Count KernelCalls;
    KernelRows "kernel_rows" Count KernelRows;
    KernelSlices "kernel_slices" Count KernelSlices;
    KernelBodies "kernel_bodies" Count KernelBodies;
    KernelBodiesTaken "kernel_bodies_taken" Count KernelBodiesTaken;
    KernelTakes "kernel_takes" Count KernelTakes;
    // The key space after the frame.
    KeyBits "key_bits" Count KeyBits;
    KeyCodes "key_codes" Count KeyCodes;
    NewCodes "new_codes" Count NewCodes;
    // The visited set's absorb: states added, packed blocks kept, decoded, made.
    AbsorbAdded "absorb_added" Count AbsorbAdded;
    AbsorbKept "absorb_kept" Count AbsorbKept;
    AbsorbDecoded "absorb_decoded" Count AbsorbDecoded;
    AbsorbMade "absorb_made" Count AbsorbMade;
    // The re-pushes' edge segments: read, their raw bytes, their records,
    // the records whose source (shape, key) is an old state's.
    RepushSegments "repush_segments" Count RepushSegments;
    RepushRaw "repush_raw_gb" Bytes RepushRaw;
    RepushRecords "repush_records" Count RepushRecords;
    RepushGrown "repush_grown" Count RepushGrown;
    // The files written.
    EdgeBytes "edge_gb" Bytes EdgeBytes;
    FrameBytes "frame_gb" Bytes FrameBytes;
    // Walls: the wave, the frame's end, the checkpoint, the frame.
    Wave "wave_ms" Wall Emissions;
    WaveIdle "wave_idle_pct" Count WaveIdle;
    Layer "layer_ms" Wall Kept;
    Ckpt "ckpt_ms" Wall Kept;
    Total "total_ms" Wall Kept;
    // The frame end's steps (plans/architecture.md "The frame's end").
    EndKeys "end_keys_ms" Wall NewCodes;
    EndSeal "end_seal_ms" Wall Touched;
    EndFilter "end_filter_ms" Wall Kept;
    EndAbsorb "end_absorb_ms" Wall Kept;
    EndLayer "end_layer_ms" Wall Grown;
    EndWins "end_wins_ms" Wall Kept;
    EndEdgeFile "end_edge_file_ms" Wall Edges;
    // The checkpoint's steps: the frame file written, read back.
    CkptFile "ckpt_file_ms" Wall LanesOut;
    CkptCheck "ckpt_check_ms" Wall LanesOut;
    // Worker time: busy, in region jobs, the seal's parts.
    Busy "busy_worker_ms" Work Busy;
    Regions "regions_worker_ms" Work Pushes;
    SealSort "seal_sort_worker_ms" Work Touched;
    SealCompare "seal_compare_worker_ms" Work Touched;
    SealTable "seal_table_worker_ms" Work Kept;
    // `--phases`: the wave's worker-seconds by phase. `PhUnit` (a kernel
    // unit's `engine.run`) holds setup, pack, kernel and emit; the rest
    // are disjoint.
    PhDecode "decode_s" Ticks LanesIn;
    PhSetup "setup_s" Ticks LanesIn;
    PhPack "pack_s" Ticks LanesIn;
    PhKernel "kernel_s" Ticks LanesIn;
    PhEmit "emit_s" Ticks Emissions;
    PhUnit "unit_s" Ticks LanesIn;
    PhUnitEnd "unit_end_s" Ticks Emissions;
    PhRepush "repush_s" Ticks RepushEdges;
    PhRegions "regions_s" Ticks Pushes;
    PhGather "gather_s" Ticks PhGather;
    PhEnd "frame_end_s" Ticks Kept;
    // Memory: the anonymous resident set at the frame's start, after the
    // wave and at its end; file-backed pages at the end; the peak (VmHWM).
    RssStart "rss_start_gb" Bytes RssStart;
    RssWave "rss_wave_gb" Bytes RssWave;
    RssEnd "rss_end_gb" Bytes RssEnd;
    RssFile "rss_file_gb" Bytes RssFile;
    Peak "peak_gb" Bytes Peak;
} }

/// A registry's statistics (`K` its count: a frame's, or a worker's, of
/// `S`; the arc phase's of `backward::A`); per timed stat its hardware
/// counts where a bench installed them (`bench::count_steps`).
#[derive(Clone, Copy)]
pub struct Stats<R: Registry = S, const K: usize = N> {
    v: [u64; K],
    hw: [[u64; 4]; K],
    /// `--phases`: the `Ticks` stats are timed.
    phases: bool,
    registry: std::marker::PhantomData<R>,
}

impl<R: Registry, const K: usize> Default for Stats<R, K> {
    fn default() -> Self {
        const { assert!(R::ALL.len() == K, "a registry's Stats holds its every stat") };
        Stats { v: [0; K], hw: [[0; 4]; K], phases: false, registry: std::marker::PhantomData }
    }
}

/// A lap of a wall timer (`Stats::lap`): its start, and the step counters'
/// counts there.
pub(crate) struct Lap {
    t: std::time::Instant,
    hw: Option<[u64; 4]>,
}

impl Lap {
    pub(crate) fn start() -> Lap {
        Lap { t: std::time::Instant::now(), hw: super::bench::step_counts() }
    }
}

impl<R: Registry, const K: usize> Stats<R, K> {
    /// Empty, the phases timed with `phases` (`--phases`).
    pub(crate) fn new(phases: bool) -> Self {
        Stats { phases, ..Default::default() }
    }
    pub fn get(&self, s: R) -> u64 {
        self.v[s.index()]
    }
    /// A time stat's hardware counts (user instructions and cycles, kernel
    /// cycles, page faults; zeros outside a bench).
    pub fn counts(&self, s: R) -> [u64; 4] {
        self.hw[s.index()]
    }
    /// A time as a `Duration`.
    pub fn time(&self, s: R) -> std::time::Duration {
        std::time::Duration::from_nanos(self.v[s.index()])
    }
    #[inline]
    pub(crate) fn add(&mut self, s: R, n: u64) {
        self.v[s.index()] += n;
    }
    pub(crate) fn set(&mut self, s: R, n: u64) {
        self.v[s.index()] = n;
    }
    pub(crate) fn add_time(&mut self, s: R, d: std::time::Duration) {
        self.add(s, d.as_nanos() as u64);
    }
    /// A size in GB (`metrics`' readings) as bytes.
    pub(crate) fn set_gb(&mut self, s: R, gb: f64) {
        self.set(s, (gb * 1e9) as u64);
    }
    /// Add the time since `lap` (and the step counters' counts) to `s`;
    /// the lap restarts.
    pub(crate) fn lap(&mut self, s: R, lap: &mut Lap) {
        let next = Lap::start();
        self.add_time(s, next.t - lap.t);
        if let (Some(a), Some(b)) = (lap.hw, next.hw) {
            for (k, c) in self.hw[s.index()].iter_mut().enumerate() {
                *c += b[k].wrapping_sub(a[k]);
            }
        }
        *lap = next;
    }
    /// A phase timer's start (`--phases`; off, 0, and `tick` is a no-op).
    #[inline]
    pub(crate) fn tick_start(&self) -> u64 {
        if self.phases {
            // SAFETY: rdtsc reads the time stamp counter; no memory access.
            unsafe { core::arch::x86_64::_rdtsc() }
        } else {
            0
        }
    }
    /// Add the ticks since `t0` (a `tick_start`) to phase `s`; returns now.
    #[inline]
    pub(crate) fn tick(&mut self, s: R, t0: u64) -> u64 {
        if t0 == 0 {
            return 0;
        }
        // SAFETY: as in `tick_start`.
        let t = unsafe { core::arch::x86_64::_rdtsc() };
        self.v[s.index()] += t - t0;
        t
    }
    /// `o`'s counts and times added (a worker's into the frame's; a `Max`
    /// stat the larger).
    pub fn merge(&mut self, o: &Self) {
        for (k, &s) in R::ALL.iter().enumerate() {
            if s.unit() == Unit::Max {
                self.v[k] = self.v[k].max(o.v[k]);
            } else {
                self.v[k] += o.v[k];
            }
            for c in 0..4 {
                self.hw[k][c] += o.hw[k][c];
            }
        }
    }
    /// Stat `s` as its unit shows it (`_ms` whole, `_gb` to the hundredth
    /// (three places under 1 GB), `_s` to the tenth).
    pub fn show(&self, s: R) -> String {
        let v = self.get(s);
        match s.unit() {
            Unit::Count | Unit::Max => v.to_string(),
            Unit::Bytes if v < 1_000_000_000 => format!("{:.3}", v as f64 / 1e9),
            Unit::Bytes => format!("{:.2}", v as f64 / 1e9),
            Unit::Wall | Unit::Work => format!("{:.0}", v as f64 / 1e6),
            Unit::Ticks => format!("{:.1}", v as f64 / tsc_hz()),
        }
    }
    /// The `[fwd]` (`[arc]`) line's fields: every stat that is not 0,
    /// `name value`.
    pub fn line(&self) -> String {
        R::ALL.iter().filter(|&&s| self.get(s) != 0).map(|&s| format!("{} {}", s.name(), self.show(s))).collect::<Vec<_>>().join(" ")
    }
    /// The `forward.tsv` row: every stat, `(name, shown)`.
    pub fn row(&self) -> Vec<(&'static str, String)> {
        R::ALL.iter().map(|&s| (s.name(), self.show(s))).collect()
    }
}

impl Stats {
    /// The wave's idle share, from its wall and the workers' busy time.
    pub(crate) fn set_idle(&mut self, workers: usize) {
        let budget = workers as f64 * self.get(S::Wave) as f64;
        let idle = if budget > 0.0 { (1.0 - self.get(S::Busy) as f64 / budget).max(0.0) } else { 0.0 };
        self.set(S::WaveIdle, (100.0 * idle).round() as u64);
    }
}

/// The TSC's ticks a second (measured once).
pub fn tsc_hz() -> f64 {
    static HZ: std::sync::OnceLock<f64> = std::sync::OnceLock::new();
    *HZ.get_or_init(|| {
        let t0 = std::time::Instant::now();
        // SAFETY: rdtsc reads the time stamp counter; no memory access.
        let c0 = unsafe { core::arch::x86_64::_rdtsc() };
        std::thread::sleep(std::time::Duration::from_millis(50));
        (unsafe { core::arch::x86_64::_rdtsc() } - c0) as f64 / t0.elapsed().as_secs_f64()
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Names are unique, and say their unit (the `forward.tsv` columns),
    /// in every registry.
    #[test]
    fn the_registries_name_their_units() {
        fn check<R: Registry>() {
            let mut names: Vec<&str> = R::ALL.iter().map(|s| s.name()).collect();
            names.sort_unstable();
            names.dedup();
            assert_eq!(names.len(), R::ALL.len(), "a name twice");
            for &s in R::ALL {
                let n = s.name();
                match s.unit() {
                    Unit::Wall | Unit::Work => assert!(n.ends_with("_ms"), "{n}"),
                    Unit::Bytes => assert!(n.ends_with("_gb"), "{n}"),
                    Unit::Ticks => assert!(n.ends_with("_s"), "{n}"),
                    Unit::Count | Unit::Max => assert!(!n.ends_with("_ms") && !n.ends_with("_gb") && !n.ends_with("_s"), "{n}"),
                }
            }
        }
        check::<S>();
        check::<crate::search::backward::A>();
    }

    /// Workers' stats add up; a wall's lap adds its time.
    #[test]
    fn stats_merge_and_lap() {
        let (mut a, mut b): (Stats, Stats) = (Stats::new(false), Stats::new(false));
        a.add(S::Emissions, 3);
        b.add(S::Emissions, 4);
        b.add_time(S::Busy, std::time::Duration::from_millis(2));
        a.merge(&b);
        assert_eq!((a.get(S::Emissions), a.show(S::Busy)), (7, "2".to_string()));
        assert_eq!(a.tick_start(), 0, "no phases: no ticks");
        let mut lap = Lap::start();
        a.lap(S::Wave, &mut lap);
        assert!(a.get(S::Wave) > 0);
        assert!(a.line().starts_with("lanes_raw 7 "), "{}", a.line());
    }
}
