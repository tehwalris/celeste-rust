// The exported runs (`rewrite export-ui`, src/search/ui_export.rs): the
// data directory holds `runs.json` - the runs to offer, in order, the
// first one the default - and one subdirectory per run with its
// run.json plus one binary per (horizon, level) for frames, marks and
// marks-by-layer. Every binary is little-endian u32 at 4-byte alignment,
// read as Uint32Array views; nothing is parsed.

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

/** One entry of `runs.json`: `id` is the run's subdirectory. */
export interface RunInfo {
  id: string;
  label: string;
}

export interface Run {
  /** The run's id in `runs.json` (its subdirectory); set by `loadRun`. */
  id: string;
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
  /** The states per cell first reached at frame `f`. (Each entry's win
   *  record is not read: the UI does not mark wins.) */
  cells(f: number): Sparse {
    if (f < 0 || f >= this.nframes) return { idx: new Uint32Array(0), count: new Uint32Array(0) };
    const e = 2 + f * 4;
    const off = this.words[e] >>> 2;
    const n = this.words[e + 1];
    return {
      idx: this.words.subarray(off, off + n),
      count: this.words.subarray(off + n, off + 2 * n),
    };
  }
}

const base = (import.meta.env.BASE_URL as string).replace(/\/?$/, "/");
export const dataUrl = (path: string) => `${base}data/${path}`;

// Every cache is keyed by `runId/name`, so two runs' files never collide.
const binCache = new Map<string, Promise<ArrayBuffer>>();
function fetchBin(path: string): Promise<ArrayBuffer> {
  let p = binCache.get(path);
  if (!p) {
    p = fetch(dataUrl(path)).then((r) => {
      if (!r.ok) throw new Error(`${path}: HTTP ${r.status}`);
      return r.arrayBuffer();
    });
    binCache.set(path, p);
  }
  return p;
}

const framesCache = new Map<string, Promise<FramesBin>>();
export function loadFrames(runId: string, name: string, magic = "CUF1"): Promise<FramesBin> {
  const path = `${runId}/${name}`;
  let p = framesCache.get(path);
  if (!p) {
    p = fetchBin(path).then((b) => new FramesBin(b, magic));
    framesCache.set(path, p);
  }
  return p;
}
export const loadLayers = (runId: string, name: string) => loadFrames(runId, name, "CUL1");

/** The runs on offer, in order; the first is the default. */
export async function loadRuns(): Promise<RunInfo[]> {
  const r = await fetch(dataUrl("runs.json"));
  if (!r.ok) throw new Error(`runs.json: HTTP ${r.status}`);
  const { runs } = (await r.json()) as { runs: RunInfo[] };
  if (!Array.isArray(runs) || runs.length === 0) throw new Error("runs.json lists no runs");
  return runs;
}

export async function loadRun(id: string): Promise<Run> {
  const r = await fetch(dataUrl(`${id}/run.json`));
  if (!r.ok) throw new Error(`${id}/run.json: HTTP ${r.status}`);
  const run = (await r.json()) as Run;
  if (run.format !== 1) throw new Error(`${id}/run.json format ${run.format}, expected 1`);
  run.id = id;
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

// ---------------------------------------------------------------------------
// Rooms, in game order, with the altitude the game shows on entering them.

/** The cart's `level_index()`: `room.x%8 + room.y*8`. */
export const levelIndex = (x: number, y: number) => (x % 8) + y * 8;

/** The room's title as the original cart draws it on entry
 *  (`room_title.draw` in celeste.lua; celeste-minimal.lua keeps
 *  `level_index` but strips the drawing): room (3,1) is "old site", level
 *  index 30 is "summit", every other room `(1 + level_index) * 100 .. " m"`. */
export function roomTitle(x: number, y: number): string {
  if (x === 3 && y === 1) return "old site";
  const i = levelIndex(x, y);
  if (i === 30) return "summit";
  return `${(1 + i) * 100} m`;
}

/** A run's room from its runs.json entry (`Room (4,0)`, or the id
 *  `room40`), or null when neither names one. */
export function roomOf(info: RunInfo): [number, number] | null {
  const m = /\((\d+)\s*,\s*(\d+)\)/.exec(info.label) ?? /^room(\d)(\d)$/.exec(info.id);
  return m ? [Number(m[1]), Number(m[2])] : null;
}

/** `Room (4,0) · 500 m`: the label the room switch shows. */
export function roomLabel(info: RunInfo): string {
  const r = roomOf(info);
  return r ? `Room (${r[0]},${r[1]}) · ${roomTitle(r[0], r[1])}` : info.label;
}

/** The runs in game order (level index); unparseable labels keep their
 *  runs.json order after the rooms. */
export function runsInGameOrder(runs: RunInfo[]): RunInfo[] {
  const key = (r: RunInfo) => {
    const room = roomOf(r);
    return room ? levelIndex(room[0], room[1]) : 1e9;
  };
  return runs.map((r, i) => ({ r, i })).sort((a, b) => key(a.r) - key(b.r) || a.i - b.i).map((e) => e.r);
}

// ---------------------------------------------------------------------------
// Horizons: what the ladder concluded at each, and the order to offer them.

export type VerdictKind = "optimal" | "confirmed" | "refuted";
export interface Verdict {
  kind: VerdictKind;
  /** `optimal` / `confirmed` / `refuted at L9`. */
  short: string;
  /** What a picker shows: `optimal, every level wins` / `refuted at L9, no win by f75`. */
  label: string;
  /** `optimal: every level wins by frame 76` / `refuted at level 9: no win by frame 75`. */
  long: string;
}

/** A horizon's verdict. `optimal` is the run's answer (every level won
 *  there and it is the minimum); `confirmed` is a horizon every level won
 *  above the optimum (a count-down's ceiling); `refuted` stopped at a level
 *  with no win by the horizon, so by design nothing on screen wins. */
export function horizonVerdict(run: Run, hr: HorizonRun): Verdict {
  if (hr.refuted_at != null) {
    return { kind: "refuted", short: `refuted at L${hr.refuted_at}`, label: `refuted at L${hr.refuted_at}, no win by f${hr.h}`, long: `refuted at level ${hr.refuted_at}: no win by frame ${hr.h}` };
  }
  if (run.optimal === hr.h) return { kind: "optimal", short: "optimal", label: "optimal, every level wins", long: `optimal: every level wins by frame ${hr.h}` };
  return { kind: "confirmed", short: "confirmed", label: "confirmed, every level wins", long: `confirmed: every level wins by frame ${hr.h} (not the minimum)` };
}

/** The horizon a view opens on: the optimal one; else the lowest
 *  confirmed one; else the last one that ran. */
export function defaultHorizon(run: Run): number {
  const hs = run.horizons;
  const opt = hs.findIndex((h) => h.refuted_at == null && h.h === run.optimal);
  if (opt >= 0) return opt;
  let best = -1;
  hs.forEach((h, i) => {
    if (h.refuted_at == null && (best < 0 || h.h < hs[best].h)) best = i;
  });
  return best >= 0 ? best : hs.length - 1;
}

/** Horizon indices in the order to offer them: the default (optimal)
 *  first, then the rest by horizon, highest first. The run's own order is
 *  the order they RAN in - a count-down from a ceiling runs 76 then 75, a
 *  count-up 89 .. 99 - which is no order to choose from. */
export function horizonOrder(run: Run): number[] {
  const d = defaultHorizon(run);
  const rest = run.horizons.map((_, i) => i).filter((i) => i !== d);
  rest.sort((a, b) => run.horizons[b].h - run.horizons[a].h);
  return [d, ...rest];
}

export function fmtInt(n: number): string {
  return n.toLocaleString("en-US");
}
/** 1.2K / 34M / 2B: three significant figures at most, no trailing ".0". */
export function fmtCompact(n: number): string {
  const one = (v: number, unit: string) => (v >= 100 ? v.toFixed(0) : v >= 10 ? v.toFixed(1).replace(/\.0$/, "") : v.toFixed(2).replace(/\.?0+$/, "")) + unit;
  if (n >= 1e9) return one(n / 1e9, "B");
  if (n >= 1e6) return one(n / 1e6, "M");
  if (n >= 1e3) return one(n / 1e3, "K");
  return String(Math.round(n));
}
/** A duration for a timing label: `245 ms`, `2.3 s`, `1m 40s`, `2h 31m`. */
export function fmtMs(ms: number): string {
  if (ms >= 3_600_000) {
    const h = Math.floor(ms / 3_600_000);
    const m = Math.round((ms % 3_600_000) / 60_000);
    return `${h}h ${m.toString().padStart(2, "0")}m`;
  }
  if (ms >= 60_000) {
    const m = Math.floor(ms / 60_000);
    const s = Math.round((ms % 60_000) / 1000);
    return `${m}m ${s.toString().padStart(2, "0")}s`;
  }
  if (ms >= 1000) return (ms / 1000).toFixed(ms >= 10_000 ? 1 : 2) + " s";
  return `${Math.round(ms)} ms`;
}
/** A duration in words, for headline figures: `7 min 13 s`, `2 h 32 min`. */
export function fmtDuration(ms: number): string {
  if (ms >= 3_600_000) {
    const h = Math.floor(ms / 3_600_000);
    const m = Math.round((ms % 3_600_000) / 60_000);
    return `${h} h ${m} min`;
  }
  if (ms >= 60_000) {
    const m = Math.floor(ms / 60_000);
    const s = Math.round((ms % 60_000) / 1000);
    return `${m} min ${s} s`;
  }
  return fmtMs(ms);
}
