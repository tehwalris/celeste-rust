// The exported run (`rewrite export-ui`, src/search/ui_export.rs): run.json
// plus one binary per (horizon, level) for frames, marks and marks-by-layer.
// Every binary is little-endian u32 at 4-byte alignment, read as
// Uint32Array views; nothing is parsed.

export interface FwdLine {
  f: number;
  in_blocks: number;
  in_lanes: number;
  raw: number;
  kept: number;
  out_blocks: number;
  out_lanes: number;
  visited: number;
  emit_ms: number;
  emit_idle: number;
  own_ms: number;
  own_idle: number;
  ckpt_ms: number;
  pos_ms: number;
  total_ms: number;
  rss_gb: number;
}

export interface BwdLine {
  f: number;
  targets: number;
  cand_cells: number;
  loaded: number;
  rerun: number;
  marked: number;
  load_thread_ms: number;
  par_ms: number;
  par_idle: number;
  total_ms: number;
}

export interface LevelRun {
  level: number;
  precision: string;
  fwd: FwdLine[];
  bwd: BwdLine[];
  first_win: number | null;
  marked: number | null;
  reruns: number | null;
  refuted: boolean;
  frames: number;
  frames_file: string;
  marks_file: string | null;
  frame_states: number[];
  frame_wins: number[];
  marks_have_dist: boolean;
  marks_by_dist: number[];
  mlayers_file: string | null;
  marks_by_layer: number[];
}

export interface HorizonRun {
  h: number;
  levels: LevelRun[];
  refuted_at: number | null;
}

export interface Box2 {
  x0: number;
  y0: number;
  w: number;
  h: number;
}

export interface Tiles {
  w: number;
  h: number;
  ids: number[];
  solid: boolean[];
}

export interface Run {
  format: number;
  room: [number, number];
  cell_box: Box2;
  tiles: Tiles;
  wall_s: number | null;
  prebuild_s: number | null;
  optimal: number | null;
  horizons: HorizonRun[];
}

/** A state with no player position (see ui_export::NO_POSITION). */
export const NO_POSITION = 0xffffffff;

/** One frame's sparse cell counts. */
export interface Sparse {
  idx: Uint32Array;
  count: Uint32Array;
}

/** A frames binary (`CUF1` frames or `CUL1` marks-by-layer). */
export class FramesBin {
  readonly nframes: number;
  private readonly words: Uint32Array;
  constructor(buf: ArrayBuffer, magic: string) {
    const m = String.fromCharCode(...new Uint8Array(buf, 0, 4));
    if (m !== magic) throw new Error(`bad magic ${m}, expected ${magic}`);
    this.words = new Uint32Array(buf);
    this.nframes = this.words[1];
  }
  private record(f: number, which: 0 | 2): Sparse {
    if (f < 0 || f >= this.nframes) return { idx: new Uint32Array(0), count: new Uint32Array(0) };
    const e = 2 + f * 4;
    const off = this.words[e + which] >>> 2;
    const n = this.words[e + which + 1];
    return {
      idx: this.words.subarray(off, off + n),
      count: this.words.subarray(off + n, off + 2 * n),
    };
  }
  /** The states per cell first reached at frame `f`. */
  cells(f: number): Sparse {
    return this.record(f, 0);
  }
  /** The win states per cell at frame `f`. */
  wins(f: number): Sparse {
    return this.record(f, 2);
  }
}

/** A marks binary (`CUM1`): (idx, dist, count) sorted by (dist, idx). */
export class MarksBin {
  readonly n: number;
  readonly idx: Uint32Array;
  readonly dist: Uint32Array;
  readonly count: Uint32Array;
  constructor(buf: ArrayBuffer) {
    const m = String.fromCharCode(...new Uint8Array(buf, 0, 4));
    if (m !== "CUM1") throw new Error(`bad magic ${m}`);
    const w = new Uint32Array(buf);
    this.n = w[1];
    this.idx = w.subarray(2, 2 + this.n);
    this.dist = w.subarray(2 + this.n, 2 + 2 * this.n);
    this.count = w.subarray(2 + 2 * this.n, 2 + 3 * this.n);
  }
}

const base = (import.meta.env.BASE_URL as string).replace(/\/?$/, "/");
export const dataUrl = (name: string) => `${base}data/${name}`;

const binCache = new Map<string, Promise<ArrayBuffer>>();
function fetchBin(name: string): Promise<ArrayBuffer> {
  let p = binCache.get(name);
  if (!p) {
    p = fetch(dataUrl(name)).then((r) => {
      if (!r.ok) throw new Error(`${name}: HTTP ${r.status}`);
      return r.arrayBuffer();
    });
    binCache.set(name, p);
  }
  return p;
}

const framesCache = new Map<string, Promise<FramesBin>>();
export function loadFrames(name: string, magic = "CUF1"): Promise<FramesBin> {
  let p = framesCache.get(name);
  if (!p) {
    p = fetchBin(name).then((b) => new FramesBin(b, magic));
    framesCache.set(name, p);
  }
  return p;
}
export const loadLayers = (name: string) => loadFrames(name, "CUL1");

const marksCache = new Map<string, Promise<MarksBin>>();
export function loadMarks(name: string): Promise<MarksBin> {
  let p = marksCache.get(name);
  if (!p) {
    p = fetchBin(name).then((b) => new MarksBin(b));
    marksCache.set(name, p);
  }
  return p;
}

export async function loadRun(): Promise<Run> {
  const r = await fetch(dataUrl("run.json"));
  if (!r.ok) throw new Error(`run.json: HTTP ${r.status}`);
  const run = (await r.json()) as Run;
  if (run.format !== 1) throw new Error(`run.json format ${run.format}, expected 1`);
  return run;
}

// ---------------------------------------------------------------------------
// The run as a sequence of chapters: what the search executed, in order. A
// forward chapter's steps are its frames, a backward chapter's steps are
// its iterations (frame H-1 down to 1). This is the timeline every view
// scrubs.

export type Phase = "fwd" | "bwd";

export interface Chapter {
  index: number;
  h: number;
  hIndex: number;
  level: number;
  levelRun: LevelRun;
  phase: Phase;
  /** Per step: the frame number, the duration in ms. */
  frames: number[];
  ms: number[];
  totalMs: number;
  /** Cumulative ms at the chapter's start over the whole run. */
  startMs: number;
  /** Cumulative step index at the chapter's start. */
  startStep: number;
}

export function chapters(run: Run): Chapter[] {
  const out: Chapter[] = [];
  let ms = 0;
  let step = 0;
  run.horizons.forEach((hr, hIndex) => {
    for (const lr of hr.levels) {
      const push = (phase: Phase, frames: number[], durations: number[]) => {
        if (frames.length === 0) return;
        const totalMs = durations.reduce((a, b) => a + b, 0);
        out.push({
          index: out.length,
          h: hr.h,
          hIndex,
          level: lr.level,
          levelRun: lr,
          phase,
          frames,
          ms: durations,
          totalMs,
          startMs: ms,
          startStep: step,
        });
        ms += totalMs;
        step += frames.length;
      };
      push(
        "fwd",
        lr.fwd.map((l) => l.f),
        lr.fwd.map((l) => l.total_ms),
      );
      push(
        "bwd",
        lr.bwd.map((l) => l.f),
        lr.bwd.map((l) => l.total_ms),
      );
    }
  });
  return out;
}

export const totalSteps = (chs: Chapter[]) =>
  chs.length ? chs[chs.length - 1].startStep + chs[chs.length - 1].frames.length : 0;

/** The chapter and step at a global step index. */
export function locate(chs: Chapter[], globalStep: number): { chapter: Chapter; step: number } {
  let lo = 0;
  let hi = chs.length - 1;
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1;
    if (chs[mid].startStep <= globalStep) lo = mid;
    else hi = mid - 1;
  }
  const c = chs[lo];
  return { chapter: c, step: Math.max(0, Math.min(c.frames.length - 1, globalStep - c.startStep)) };
}

export const levelName = (lr: LevelRun) =>
  lr.precision === "Exact" ? "exact" : `${lr.level} bit${lr.level === 1 ? "" : "s"}`;

export function fmtInt(n: number): string {
  return n.toLocaleString("en-US");
}
export function fmtCompact(n: number): string {
  if (n >= 1e9) return (n / 1e9).toFixed(n >= 1e10 ? 0 : 1) + "B";
  if (n >= 1e6) return (n / 1e6).toFixed(n >= 1e7 ? 0 : 1) + "M";
  if (n >= 1e3) return (n / 1e3).toFixed(n >= 1e4 ? 0 : 1) + "K";
  return String(n);
}
export function fmtMs(ms: number): string {
  if (ms >= 60_000) {
    const m = Math.floor(ms / 60_000);
    const s = Math.round((ms % 60_000) / 1000);
    return `${m}m ${s.toString().padStart(2, "0")}s`;
  }
  if (ms >= 1000) return (ms / 1000).toFixed(ms >= 10_000 ? 1 : 2) + " s";
  return `${Math.round(ms)} ms`;
}
