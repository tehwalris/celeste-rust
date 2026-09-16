// The search as space. Four axes, each bound to one control (README):
//
//   horizon  - the win frame H everything on screen is at; the horizon
//              picker at the top of the stage, optimal first and default,
//              every horizon labelled with its verdict.
//   time     - the position in H's ladder: the passes in the order they
//              ran (L0 forward, L0 backward, L1 forward, ...), a forward
//              sweeping frames 0..H and a backward sweeping iterations
//              H-1..1; the scrubber, the step / pass buttons, Play, and the
//              ladder panel's rows (jump to a level).
//   grain    - Room (the current pass, big) / Grid (every level's panel,
//              one playing at a time in ladder order) / Passes (a step is
//              a whole pass) / 3D (the room as columns or a stack of
//              layers, orbited; view3d.ts).
//   look     - Height map (the bands) / Full (accumulated under moving,
//              closed levels' bands) / Sweep (just what moves).
//
// The renderer composites per-cell count grids over the room's tiles
// (room.ts); this file decides which grids, in which ramp, at which alpha.
// Wins are drawn as a reticle on the cell the winning state left the room
// from, in every look.
//
// Layout: on a phone, the stage (horizon, pass title, the room, the cell
// readout) then the options, the ladder and the legend, with the transport
// fixed to the bottom. On a wide screen the page does not scroll: the
// stage fills the height left by the header and the transport under it,
// and the ladder, options and legend sit in a scrolling side column.

import type { FramesBin, HorizonRun, LevelRun, Run } from "./data";
import { NO_POSITION, defaultHorizon, fmtCompact, fmtInt, horizonOrder, horizonVerdict, levelName, loadFrames, loadLayers, type VerdictKind } from "./data";
import { levelCss, levelRamp, marksRamp, heightBand, bandColor, bandHalo, movingColor, HEIGHT_BANDS, rgbCss, levelColor, LEVELS, type RGB } from "./color";
import { addSparse, NEXT_ROOM_X, RoomRenderer, sparseMax, winKeyIcon, type HeatLayer, type Scene, type WinMark } from "./room";
import { button, chips, clear, el, icon, scrubber, select, show } from "./ui";
import { Instances, View3D } from "./view3d";
import type { View } from "./main";

type Phase = "fwd" | "bwd";
type Grain = "room" | "grid" | "passes" | "3d";
/** The 3D grain: columns (a cell's height is its count, on the step
 *  timeline) or stack (one layer of cubes per pass, on the pass timeline). */
type Mode3 = "columns" | "stack";
type Look = "full" | "sweep" | "last";
/** Uniform: every step takes the same time. Real: a step's share of the
 *  playback is its share of the run's logged time (the level-0 passes
 *  crawl, the high-bit passes flick by), normalised so a playthrough
 *  lasts as long as the uniform one at the same speed. */
type Pacing = "uniform" | "real";

/** One frame's wins of a level file, as recorded. */
interface WinEntry {
  f: number;
  idx: number;
  count: number;
}

interface LevelFiles {
  frames: FramesBin;
  layers: FramesBin | null;
  frameMax: number;
  cumMax: number;
  marksMax: number;
  wins: WinEntry[];
}

/** One pass of a horizon's ladder: a level's forward or backward, with
 *  the frames it sweeps (0..H forward, H-1..1 backward). */
interface Pass {
  index: number;
  lr: LevelRun;
  phase: Phase;
  frames: number[];
  /** Timeline step of the pass's first frame. */
  start: number;
}

interface Named {
  name: string;
  grid: Float32Array;
}
/** The colour scale of a scene's moving set: its ramp and the count
 *  that maps to the ramp's top. */
interface Scale {
  ramp: RGB[];
  max: number;
  what: string;
}
interface Built {
  scene: Scene;
  probe: Named[];
  off: number;
  scale?: Scale;
  /** Won states whose recorded cell is in the next room (an export from
   *  before the exit-placement fix): counted, not drawn. */
  offRoom: number;
}
const empty = (): Built => ({ scene: { layers: [], wins: [] }, probe: [], off: 0, offRoom: 0 });

const SPEEDS: { label: string; steps: number; passes: number }[] = [
  { label: "slow", steps: 15, passes: 1 },
  { label: "normal", steps: 60, passes: 2 },
  { label: "fast", steps: 250, passes: 4 },
];
const GRAINS: Grain[] = ["room", "grid", "passes", "3d"];
const LOOKS: Look[] = ["full", "sweep", "last"];
const GROUP: Record<VerdictKind, string> = {
  optimal: "optimal",
  confirmed: "confirmed above the optimum",
  refuted: "refuted: no level wins, by design",
};

export function spaceView(run: Run, onState: () => void): View {
  const renderer = new RoomRenderer(run.cell_box, run.tiles);
  const ncell = run.cell_box.w * run.cell_box.h;
  /** The horizon the view opens on: the optimal one (data.ts). */
  const defH = defaultHorizon(run);

  const st = {
    h: defH,
    step: 0,
    pass: 0,
    grain: "room" as Grain,
    mode3: "columns" as Mode3,
    look: "last" as Look,
    speed: 1,
    pacing: "uniform" as Pacing,
    /** Real pacing: the playhead in uniform-step units (a float). */
    pos: 0,
    playing: false,
  };
  /** Whether the timeline is walked a pass at a time (Passes, 3D stack)
   *  rather than a frame / iteration at a time. */
  const byPass = () => st.grain === "passes" || (st.grain === "3d" && st.mode3 === "stack");

  // ---- the timeline of a horizon --------------------------------------------
  const timelines = new Map<number, Pass[]>();
  function timeline(hIndex: number): Pass[] {
    let tl = timelines.get(hIndex);
    if (!tl) {
      const hr = run.horizons[hIndex];
      tl = [];
      let start = 0;
      for (const lr of hr.levels) {
        const last = Math.min(hr.h, Math.max(0, lr.frames - 1));
        const fwd = Array.from({ length: last + 1 }, (_, i) => i);
        tl.push({ index: tl.length, lr, phase: "fwd", frames: fwd, start });
        start += fwd.length;
        if (!lr.refuted && lr.mlayers_file && hr.h >= 2) {
          const bwd = Array.from({ length: hr.h - 1 }, (_, i) => hr.h - 1 - i);
          tl.push({ index: tl.length, lr, phase: "bwd", frames: bwd, start });
          start += bwd.length;
        }
      }
      timelines.set(hIndex, tl);
    }
    return tl;
  }
  const stepsOf = (tl: Pass[]) => (tl.length ? tl[tl.length - 1].start + tl[tl.length - 1].frames.length : 0);
  function locate(tl: Pass[], step: number): { pass: Pass; i: number } {
    let lo = 0;
    let hi = tl.length - 1;
    while (lo < hi) {
      const mid = (lo + hi + 1) >> 1;
      if (tl[mid].start <= step) lo = mid;
      else hi = mid - 1;
    }
    const pass = tl[lo];
    return { pass, i: Math.max(0, Math.min(pass.frames.length - 1, step - pass.start)) };
  }
  /** The last step of a horizon's timeline (the ladder's final state). */
  const lastStep = (hIndex: number) => Math.max(0, stepsOf(timeline(hIndex)) - 1);

  // ---- real-time pacing: the log's ms per step, normalised -------------------------
  // Level 0's forward frames are logged across horizons (each horizon's
  // level 0 holds the frames it was extended by), so its ms are one map.
  const l0ms = new Map<number, number>();
  for (const hr of run.horizons) for (const lr of hr.levels) if (lr.level === 0) for (const l of lr.fwd) l0ms.set(l.f, l.total_ms);
  function stepMs(p: Pass, f: number): number {
    if (p.phase === "fwd") {
      const ms = p.lr.level === 0 ? l0ms.get(f) : p.lr.fwd.find((l) => l.f === f)?.total_ms;
      return Math.max(1, ms ?? 1);
    }
    return Math.max(1, p.lr.bwd.find((l) => l.f === f)?.total_ms ?? 1);
  }
  /** Cumulative weights, one entry per step plus the end, scaled so the
   *  last entry equals the step count (uniform-step units). */
  function cumulative(weights: number[]): Float64Array {
    const n = weights.length;
    const cum = new Float64Array(n + 1);
    let total = 0;
    for (const w of weights) total += w;
    const k = total > 0 ? n / total : 1;
    for (let i = 0; i < n; i++) cum[i + 1] = cum[i] + weights[i] * k;
    return cum;
  }
  const stepCums = new Map<number, Float64Array>();
  function stepCum(hIndex: number): Float64Array {
    let c = stepCums.get(hIndex);
    if (!c) {
      const ws: number[] = [];
      for (const p of timeline(hIndex)) for (const f of p.frames) ws.push(stepMs(p, f));
      c = cumulative(ws);
      stepCums.set(hIndex, c);
    }
    return c;
  }
  const passCums = new Map<number, Float64Array>();
  function passCum(hIndex: number): Float64Array {
    let c = passCums.get(hIndex);
    if (!c) {
      c = cumulative(timeline(hIndex).map((p) => p.frames.reduce((a, f) => a + stepMs(p, f), 0)));
      passCums.set(hIndex, c);
    }
    return c;
  }
  /** The step whose span holds `pos`. */
  function indexAt(cum: Float64Array, pos: number): number {
    const n = cum.length - 1;
    let lo = 0;
    let hi = n - 1;
    while (lo < hi) {
      const mid = (lo + hi + 1) >> 1;
      if (cum[mid] <= pos) lo = mid;
      else hi = mid - 1;
    }
    return lo;
  }

  // ---- level files, cached ---------------------------------------------------
  /** (frames file, marks file): level 0's frames are one file for every
   *  horizon while its marks are per horizon, so the key carries both. */
  const filesKey = (lr: LevelRun) => `${lr.frames_file}|${lr.mlayers_file ?? ""}`;
  const fileCache = new Map<string, Promise<LevelFiles>>();
  const loaded = new Map<string, LevelFiles>();
  function levelFiles(lr: LevelRun): Promise<LevelFiles> {
    const key = filesKey(lr);
    let p = fileCache.get(key);
    if (!p) {
      p = (async () => {
        const [frames, layers] = await Promise.all([loadFrames(run.id, lr.frames_file), lr.mlayers_file ? loadLayers(run.id, lr.mlayers_file) : null]);
        let frameMax = 1;
        let marksMax = 1;
        const cum = new Float32Array(ncell);
        const wins: WinEntry[] = [];
        for (let f = 0; f < frames.nframes; f++) {
          const c = frames.cells(f);
          frameMax = Math.max(frameMax, sparseMax(c));
          addSparse(cum, c);
          const w = frames.wins(f);
          for (let i = 0; i < w.idx.length; i++) wins.push({ f, idx: w.idx[i], count: w.count[i] });
          if (layers) marksMax = Math.max(marksMax, sparseMax(layers.cells(f)));
        }
        let cumMax = 1;
        for (let i = 0; i < ncell; i++) if (cum[i] > cumMax) cumMax = cum[i];
        const out = { frames, layers, frameMax, cumMax, marksMax, wins };
        loaded.set(key, out);
        return out;
      })();
      // A failed fetch must not poison the cache: the next render retries.
      p.catch(() => fileCache.delete(key));
      fileCache.set(key, p);
    }
    return p;
  }
  const have = (lr: LevelRun) => loaded.get(filesKey(lr));
  const ensure = (lrs: LevelRun[]) => Promise.all(lrs.map(levelFiles));

  // ---- grids -----------------------------------------------------------------
  /** The whole-range sums (a level's reached set by H, a backward's whole
   *  marked set) are drawn under every frame of every later pass, so they
   *  are memoised per (file, H); the per-frame prefixes are not (a hundred
   *  grids per level would cost more than the adds they save). */
  function cumGrid(bin: FramesBin, lo: number, hi: number): { grid: Float32Array; off: number } {
    const grid = new Float32Array(ncell);
    let off = 0;
    for (let f = Math.max(0, lo); f <= Math.min(hi, bin.nframes - 1); f++) off += addSparse(grid, bin.cells(f));
    return { grid, off };
  }
  const cumCache = new WeakMap<FramesBin, Map<number, { grid: Float32Array; off: number }>>();
  /** `cumGrid(bin, 0, h)`, memoised: the sum to the horizon. */
  function cumTo(bin: FramesBin, h: number): { grid: Float32Array; off: number } {
    let m = cumCache.get(bin);
    if (!m) cumCache.set(bin, (m = new Map()));
    let out = m.get(h);
    if (!out) m.set(h, (out = cumGrid(bin, 0, h)));
    return out;
  }

  /** A level's wins at frames <= `upto`, one mark per cell. `upto` is
   *  capped by the callers at the horizon: level 0's frames file is shared
   *  by every horizon and holds wins past the earlier ones. A win recorded
   *  in the next room (x >= 128: an old export's placement at the next
   *  room's spawn) has no known exit cell and is only counted. */
  function winsUpTo(lr: LevelRun, upto: number): { wins: WinMark[]; offRoom: number } {
    const files = have(lr);
    const by = new Map<number, WinMark>();
    let offRoom = 0;
    if (!files) return { wins: [], offRoom };
    for (const e of files.wins) {
      if (e.f > upto || e.idx === NO_POSITION) continue;
      if (renderer.cellXY(e.idx)[0] >= NEXT_ROOM_X) {
        offRoom += e.count;
        continue;
      }
      const m = by.get(e.idx);
      if (m) {
        m.count += e.count;
        m.frame = Math.min(m.frame, e.f);
      } else by.set(e.idx, { idx: e.idx, count: e.count, frame: e.f });
    }
    return { wins: [...by.values()], offRoom };
  }
  /** The wins a pass shows at frame index `i` (null: the whole pass): a
   *  forward the wins found so far, a backward every win by H (it marks
   *  back from them). */
  function passWins(hr: HorizonRun, pass: Pass, i: number | null) {
    return winsUpTo(pass.lr, pass.phase === "fwd" && i != null ? Math.min(pass.frames[i], hr.h) : hr.h);
  }

  /** The closed levels of `hr` below `level`, as receding bands. */
  function bands(hr: HorizonRun, level: number, layers: HeatLayer[], probe: Named[]) {
    const closed = hr.levels.filter((l) => l.level < level && !l.refuted && l.mlayers_file);
    closed.forEach((l, k) => {
      const fl = have(l);
      if (!fl || !fl.layers) return;
      const g = cumTo(fl.layers, hr.h);
      layers.push({ grid: g.grid, max: fl.marksMax, ramp: levelRamp(l.level, "band"), alpha: k === closed.length - 1 ? 0.5 : 0.35 });
      probe.push({ name: `marked, level ${l.level}`, grid: g.grid });
    });
  }

  /** The base of the Full look: level 0's whole reachable space, dim. */
  function base(hr: HorizonRun, layers: HeatLayer[], probe: Named[]) {
    const l0 = hr.levels.find((l) => l.level === 0);
    const f0 = l0 && have(l0);
    if (!l0 || !f0) return;
    const g = cumTo(f0.frames, hr.h);
    layers.push({ grid: g.grid, max: f0.cumMax, ramp: levelRamp(0, "dim"), alpha: 0.95 });
    probe.push({ name: "reached, level 0", grid: g.grid });
  }

  /** One level at (phase, f): the moving set bright over its own
   *  accumulated set (Full), or the moving set alone (Sweep). */
  function levelAt(lr: LevelRun, phase: Phase, f: number, h: number, full: boolean, layers: HeatLayer[], probe: Named[]): { off: number; scale?: Scale } {
    const files = have(lr);
    if (!files) return { off: 0 };
    let off = 0;
    let scale: Scale | undefined;
    if (phase === "fwd") {
      if (full) {
        const g = cumGrid(files.frames, 0, f - 1);
        layers.push({ grid: g.grid, max: files.cumMax, ramp: levelRamp(lr.level, "dim"), alpha: 0.95 });
        probe.push({ name: `reached before f${f}`, grid: g.grid });
      }
      const g = new Float32Array(ncell);
      off += addSparse(g, files.frames.cells(f));
      layers.push({ grid: g, max: files.frameMax, ramp: levelRamp(lr.level, "bright"), alpha: 1 });
      probe.push({ name: `frontier at f${f}`, grid: g });
      scale = { ramp: levelRamp(lr.level, "bright"), max: files.frameMax, what: "frontier states per cell" };
    } else {
      if (full) {
        const g = cumTo(files.frames, h);
        layers.push({ grid: g.grid, max: files.cumMax, ramp: levelRamp(lr.level, "dim"), alpha: 0.95 });
        probe.push({ name: `reached, level ${lr.level}`, grid: g.grid });
      }
      if (files.layers) {
        if (full) {
          const sofar = cumGrid(files.layers, f + 1, h);
          layers.push({ grid: sofar.grid, max: files.marksMax, ramp: marksRamp("dim"), alpha: 0.9 });
          probe.push({ name: `marked so far`, grid: sofar.grid });
        }
        const g = new Float32Array(ncell);
        addSparse(g, files.layers.cells(f));
        layers.push({ grid: g, max: files.marksMax, ramp: marksRamp("bright"), alpha: 1 });
        probe.push({ name: `marked at f${f}`, grid: g });
        scale = { ramp: marksRamp("bright"), max: files.marksMax, what: "marked states per cell" };
      }
    }
    return { off, scale };
  }

  /** The height-map look: paint the passes of `tl` before `upto` in
   *  order, each set flat in its BAND colour over the previous ones, so a
   *  cell wears the colour of the finest band that still contains it. The
   *  two thinnest bands are dilated by one cell in a dimmer tone first,
   *  so a one-cell route still reads on a phone. */
  function paintPasses(hr: HorizonRun, tl: Pass[], upto: number, layers: HeatLayer[], probe: Named[]) {
    for (const p of tl.slice(0, upto)) {
      const files = have(p.lr);
      if (!files) continue;
      const g = p.phase === "fwd" ? cumTo(files.frames, hr.h) : files.layers ? cumTo(files.layers, hr.h) : null;
      if (!g) continue;
      paintBand(g.grid, heightBand(p.lr.level), layers);
      probe.push({ name: `${p.phase === "fwd" ? "reached" : "marked"}, level ${p.lr.level}`, grid: g.grid });
    }
  }
  function paintBand(grid: Float32Array, band: number, layers: HeatLayer[]) {
    if (band >= HEIGHT_BANDS.length - 2) {
      layers.push({ grid: dilate(grid), max: 1, ramp: [bandHalo(band)], alpha: 1, flat: true });
    }
    layers.push({ grid, max: 1, ramp: [bandColor(band)], alpha: 1, flat: true });
  }
  /** The 4-neighbourhood dilation of a set (as a grid). */
  function dilate(grid: Float32Array): Float32Array {
    const w = run.cell_box.w;
    const out = new Float32Array(ncell);
    for (let i = 0; i < ncell; i++) {
      if (grid[i] <= 0) continue;
      out[i] = 1;
      const x = i % w;
      if (x > 0) out[i - 1] = 1;
      if (x < w - 1) out[i + 1] = 1;
      if (i >= w) out[i - w] = 1;
      if (i + w < ncell) out[i + w] = 1;
    }
    return out;
  }

  /** The height-map look inside a pass: its accumulated set so far in its
   *  band colour, the moving set (frontier / marked now) in magenta. */
  function paintCurrent(lr: LevelRun, phase: Phase, f: number, h: number, layers: HeatLayer[], probe: Named[]) {
    const files = have(lr);
    if (!files) return 0;
    const band = heightBand(lr.level);
    if (phase === "fwd") {
      const g = cumGrid(files.frames, 0, f - 1);
      paintBand(g.grid, band, layers);
      probe.push({ name: `reached, level ${lr.level}`, grid: g.grid });
      const m = new Float32Array(ncell);
      const off = addSparse(m, files.frames.cells(f));
      layers.push({ grid: m, max: 1, ramp: [movingColor], alpha: 1, flat: true });
      probe.push({ name: `frontier at f${f}`, grid: m });
      return off;
    }
    if (!files.layers) return 0;
    const g = cumGrid(files.layers, f + 1, h);
    paintBand(g.grid, band, layers);
    probe.push({ name: `marked, level ${lr.level}`, grid: g.grid });
    const m = new Float32Array(ncell);
    addSparse(m, files.layers.cells(f));
    layers.push({ grid: m, max: 1, ramp: [movingColor], alpha: 1, flat: true });
    probe.push({ name: `marked at f${f}`, grid: m });
    return 0;
  }

  /** Room grain: the current pass at its time. */
  function roomScene(hr: HorizonRun, pass: Pass, i: number): Built {
    const layers: HeatLayer[] = [];
    const probe: Named[] = [];
    const w = passWins(hr, pass, i);
    if (st.look === "last") {
      paintPasses(hr, timeline(st.h), pass.index, layers, probe);
      const off = paintCurrent(pass.lr, pass.phase, pass.frames[i], hr.h, layers, probe);
      return { scene: { layers, wins: w.wins }, probe, off, offRoom: w.offRoom };
    }
    const full = st.look === "full";
    if (full && pass.lr.level !== 0) base(hr, layers, probe);
    if (full) bands(hr, pass.lr.level, layers, probe);
    const { off, scale } = levelAt(pass.lr, pass.phase, pass.frames[i], hr.h, full, layers, probe);
    return { scene: { layers, wins: w.wins }, probe, off, scale, offRoom: w.offRoom };
  }

  /** A level after its last pass: everything it reached, dim, with all
   *  its marks (or, in the height map, the passes painted through it). */
  function finalScene(lr: LevelRun, last: Pass, h: number): Built {
    const layers: HeatLayer[] = [];
    const probe: Named[] = [];
    const w = winsUpTo(lr, h);
    if (st.look === "last") {
      paintPasses(run.horizons[st.h], timeline(st.h), last.index + 1, layers, probe);
      return { scene: { layers, wins: w.wins }, probe, off: 0, offRoom: w.offRoom };
    }
    const files = have(lr);
    if (!files) return empty();
    if (st.look === "full") {
      const g = cumTo(files.frames, h);
      layers.push({ grid: g.grid, max: files.cumMax, ramp: levelRamp(lr.level, "dim"), alpha: 0.95 });
      probe.push({ name: `reached, level ${lr.level}`, grid: g.grid });
    }
    if (files.layers) {
      const g = cumTo(files.layers, h);
      layers.push({ grid: g.grid, max: files.marksMax, ramp: marksRamp("bright"), alpha: 1 });
      probe.push({ name: `marked, level ${lr.level}`, grid: g.grid });
    }
    return { scene: { layers, wins: w.wins }, probe, off: 0, offRoom: w.offRoom };
  }

  /** Grid grain: one level's panel at (phase, f). */
  function panelScene(lr: LevelRun, phase: Phase, f: number, h: number): Built {
    const layers: HeatLayer[] = [];
    const probe: Named[] = [];
    const w = winsUpTo(lr, phase === "fwd" ? Math.min(f, h) : h);
    if (st.look === "last") {
      // The passes before this level's (phase) pass, then its own sweep.
      const tl = timeline(st.h);
      const own = tl.find((p) => p.lr === lr && p.phase === phase) ?? tl.find((p) => p.lr === lr);
      const hr = run.horizons[st.h];
      if (own) {
        paintPasses(hr, tl, own.index, layers, probe);
        if (own.phase === phase) paintCurrent(lr, phase, f, h, layers, probe);
        else if (phase === "bwd") paintPasses(hr, tl, own.index + 1, layers, probe); // no backward ran: the reached set stays
      }
      return { scene: { layers, wins: w.wins }, probe, off: 0, offRoom: w.offRoom };
    }
    const full = st.look === "full";
    if (phase === "bwd" && (lr.refuted || !lr.mlayers_file)) {
      // No backward ran: the level's reached set, dimmed, and nothing else.
      const files = have(lr);
      if (files && full) {
        const g = cumTo(files.frames, h);
        layers.push({ grid: g.grid, max: files.cumMax, ramp: levelRamp(lr.level, "dim"), alpha: 0.95 });
      }
      return { scene: { layers, wins: w.wins }, probe, off: 0, offRoom: w.offRoom };
    }
    const { off } = levelAt(lr, phase, f, h, full, layers, probe);
    return { scene: { layers, wins: w.wins }, probe, off, offRoom: w.offRoom };
  }

  /** Passes grain: a whole pass. A forward is everything the level
   *  reached by H (bright); a backward its whole marked set (bright warm
   *  white) over the reached set dimmed. Closed levels recede underneath. */
  function passScene(hr: HorizonRun, pass: Pass): Built {
    const layers: HeatLayer[] = [];
    const probe: Named[] = [];
    const w = passWins(hr, pass, null);
    if (st.look === "last") {
      paintPasses(hr, timeline(st.h), pass.index + 1, layers, probe);
      return { scene: { layers, wins: w.wins }, probe, off: 0, offRoom: w.offRoom };
    }
    const full = st.look === "full";
    const files = have(pass.lr);
    if (!files) return empty();
    if (full && pass.lr.level !== 0) base(hr, layers, probe);
    if (full) bands(hr, pass.lr.level, layers, probe);
    const vis = cumTo(files.frames, hr.h);
    let scale: Scale | undefined;
    if (pass.phase === "fwd") {
      layers.push({ grid: vis.grid, max: files.cumMax, ramp: levelRamp(pass.lr.level, "bright"), alpha: 1 });
      probe.push({ name: `reached, level ${pass.lr.level}`, grid: vis.grid });
      scale = { ramp: levelRamp(pass.lr.level, "bright"), max: files.cumMax, what: "reached states per cell" };
    } else {
      if (full) {
        layers.push({ grid: vis.grid, max: files.cumMax, ramp: levelRamp(pass.lr.level, "dim"), alpha: 0.95 });
        probe.push({ name: `reached, level ${pass.lr.level}`, grid: vis.grid });
      }
      if (files.layers) {
        const g = cumTo(files.layers, hr.h);
        layers.push({ grid: g.grid, max: files.marksMax, ramp: marksRamp("bright"), alpha: 1 });
        probe.push({ name: `marked, level ${pass.lr.level}`, grid: g.grid });
        scale = { ramp: marksRamp("bright"), max: files.marksMax, what: "marked states per cell" };
      }
    }
    return { scene: { layers, wins: w.wins }, probe, off: vis.off, scale, offRoom: w.offRoom };
  }

  // ---- DOM: the stage --------------------------------------------------------------
  const aspect = run.cell_box.w / run.cell_box.h;
  const root = el("div", { class: "space", style: `--aspect:${aspect.toFixed(4)}` });

  const hPicker = select<number>(
    horizonOrder(run).map((i) => {
      const hr = run.horizons[i];
      const v = horizonVerdict(run, hr);
      return { value: i, label: `h${hr.h} · ${v.label}`, group: GROUP[v.kind] };
    }),
    st.h,
    (i) => goHorizon(i),
    { label: "horizon", class: "h-picker" },
  );
  const passTitle = el("div", { class: "pass-title" });
  const stageHead = el("div", { class: "stage-head" }, [hPicker.root, passTitle]);

  const canvas = el("canvas", { class: "room-canvas", "aria-label": "the room: one cell per player pixel, colour = states in the cell" });
  const overlay = el("canvas", { class: "room-overlay" });
  const roomWrap = el("div", { class: "room-wrap" }, [canvas, overlay]);
  const gridBox = el("div", { class: "grid-panels" });
  const view3dTools = el("div", { class: "view3d-tools" });
  const view3dWrap = el("div", { class: "view3d-wrap" }, [view3dTools]);
  const loadingPill = el("div", { class: "room-loading", role: "status" }, [el("span", { class: "spinner" }), "loading level files"]);
  const stage = el("div", { class: "stage", "data-loading": "false" }, [roomWrap, gridBox, view3dWrap, loadingPill]);

  const probeEl = el("div", { class: "probe", "aria-live": "polite" });
  const scaleEl = el("div", { class: "scale" });
  const winKey = el("div", { class: "win-key" });
  const stageFoot = el("div", { class: "stage-foot" }, [probeEl, scaleEl, winKey]);
  const stageCol = el("section", { class: "space-stage card" }, [stageHead, stage, stageFoot]);

  // ---- DOM: the transport (play, step, scrub) ------------------------------------------
  const status = el("div", { class: "status", "aria-live": "off" });
  const playBtn = button(icon("play"), () => togglePlay(), "primary icon play", "Play (space)");
  const backBtn = button(icon("stepBack"), () => nudge(-1), "icon", "Back one step (←)");
  const fwdBtn = button(icon("stepFwd"), () => nudge(1), "icon", "Forward one step (→)");
  const prevBtn = button(icon("prev"), () => jumpPass(-1), "icon", "Previous pass ([)");
  const nextBtn = button(icon("next"), () => jumpPass(1), "icon", "Next pass (])");
  const transport = el("div", { class: "transport" }, [el("div", { class: "buttons" }, [prevBtn, backBtn, playBtn, fwdBtn, nextBtn]), status]);
  const scrub = scrubber(
    (v) => {
      if (byPass()) st.pass = v;
      else st.step = v;
      stop();
      render();
    },
    (g) => {
      if (g) stop();
      else onState();
    },
  );
  const bar = el("div", { class: "transport-bar" }, [transport, scrub.root]);

  function goHorizon(i: number) {
    st.h = i;
    // A horizon opens on its answer, as the view does: the ladder's last step.
    st.step = lastStep(i);
    st.pass = timeline(i).length - 1;
    stop();
    hPicker.set(i);
    layout();
    onState();
  }

  // ---- DOM: the side column (ladder, options, legend) ---------------------------------------
  const ladderTitle = el("h2");
  const ladderVerdict = el("p", { class: "verdict-line" });
  const ladderList = el("div", { class: "ladder-list" });
  const ladderCard = el("section", { class: "card ladder-card" }, [
    ladderTitle,
    ladderVerdict,
    el("div", { class: "ladder-head", "aria-hidden": true }, [el("span", { text: "level" }), el("span", { text: "result" }), el("span", { text: "marked" })]),
    ladderList,
    el("p", { class: "note", text: "Each level searches only inside the marked set of the one before; tap a level to jump to its forward." }),
  ]);

  const grainChips = chips<Grain>(
    [
      { value: "room", label: "Room", title: "the current pass, big" },
      { value: "grid", label: "Grid", title: "every level's panel; the current pass's level animates" },
      { value: "passes", label: "Passes", title: "a step is a whole pass: the ladder narrowing pass by pass" },
      { value: "3d", label: "3D", title: "the room as columns or a stack of layers, orbited" },
    ],
    st.grain,
    (g) => {
      setGrain(g);
      onState();
    },
    { label: "view" },
  );
  const mode3Chips = chips<Mode3>(
    [
      { value: "columns", label: "Columns", title: "each cell a column as tall as its count (log); the step timeline" },
      { value: "stack", label: "Stack", title: "one layer of cubes per pass, stacking up as the passes go; the pass timeline" },
    ],
    st.mode3,
    (m) => {
      setMode3(m);
      onState();
    },
    { label: "3D" },
  );
  const lookChips = chips<Look>(
    [
      { value: "last", label: "Height map", title: "each cell in the colour of the finest band of levels whose set still contains it: dark for the flood, brightest for the exact route" },
      { value: "full", label: "Full", title: "the accumulated set under the moving set, closed levels' marks as bands" },
      { value: "sweep", label: "Sweep", title: "just what moves: the frontier, or the states this iteration marks" },
    ],
    st.look,
    (l) => {
      st.look = l;
      syncLegend();
      render();
      onState();
    },
    { label: "look" },
  );
  const speedChips = chips<number>(
    SPEEDS.map((s, i) => ({ value: i, label: s.label, title: `${s.steps} frames per second (${s.passes} pass${s.passes === 1 ? "" : "es"} per second when stepping by pass)` })),
    st.speed,
    (i) => {
      st.speed = i;
      onState();
    },
    { label: "speed" },
  );
  const pacingChips = chips<Pacing>(
    [
      { value: "uniform", label: "Uniform", title: "every frame / iteration takes the same time" },
      { value: "real", label: "Real time", title: "each step takes its share of the run's logged time (level 0 crawls, the high-bit levels flick by); same overall duration" },
    ],
    st.pacing,
    (p) => {
      st.pacing = p;
      st.pos = (byPass() ? passCum(st.h) : stepCum(st.h))[byPass() ? st.pass : st.step];
      onState();
    },
    { label: "pacing" },
  );
  const optionCard = el("section", { class: "card options" }, [grainChips.root, lookChips.root, mode3Chips.root, speedChips.root, pacingChips.root]);

  // ---- DOM: the legend --------------------------------------------------------------
  const key = (color: string, label: string, cls = "") => el("span", { class: "key" }, [el("i", { class: cls, style: color ? `background:${color}` : undefined }), label]);
  const winLegend = () => el("span", { class: "key" }, [winKeyIcon(16), "win: where a winning state left the room"]);
  const legendFull = el("div", { class: "legend" }, [
    key(rgbCss(levelRamp(0, "bright")[55]), "moving: the frontier"),
    key(rgbCss(levelRamp(0, "dim")[40]), "accumulated: reached before"),
    key(rgbCss(marksRamp("bright")[55]), "marked now"),
    key(rgbCss(marksRamp("dim")[40]), "marked so far"),
    key(rgbCss(levelRamp(4, "band")[45]), "a closed level's marks"),
    winLegend(),
  ]);
  const legendSweep = el("div", { class: "legend" }, [key(rgbCss(levelRamp(0, "bright")[55]), "the frontier at this frame"), key(rgbCss(marksRamp("bright")[55]), "marked at this iteration"), winLegend()]);
  const levelStrip = el("div", { class: "level-strip" });
  for (let i = 0; i < LEVELS; i++) levelStrip.append(el("i", { style: `background:${levelCss(i)}`, title: i === 16 ? "exact" : `level ${i}: ${i} bit${i === 1 ? "" : "s"}` }));
  const levelBlock = el("div", {}, [
    el("div", { class: "legend-title", text: "hue = the ladder level" }),
    levelStrip,
    el("div", { class: "strip-labels" }, [el("span", { text: "level 0 (coarsest)" }), el("span", { text: "1 … 15 bits" }), el("span", { text: "exact" })]),
  ]);
  const bandStrip = el("div", { class: "band-strip" });
  HEIGHT_BANDS.forEach((b, i) => bandStrip.append(el("span", { class: "band" }, [el("i", { style: `background:${rgbCss(bandColor(i))}` }), el("span", { text: b.label })])));
  bandStrip.append(el("span", { class: "band" }, [el("i", { style: `background:${rgbCss(movingColor)}` }), el("span", { text: "moving" })]));
  const bandBlock = el("div", {}, [
    el("div", { class: "legend-title", text: "colour = the finest band of levels that still contains the cell" }),
    bandStrip,
    el("div", { class: "legend", style: "margin-top:8px" }, [winLegend()]),
  ]);
  const block3d = el("div", {}, [
    el("div", { class: "legend-title", text: "3D" }),
    el("p", { class: "note", text: "One finger (or the mouse) orbits: sideways turns, up and down tilts from top-down to edge-on; two fingers (or shift-drag) pan, pinch or scroll zooms; double-tap resets. Columns: a cell's height is its count (log), the moving set as the bright cap. Stack: one layer of cubes per pass, a forward's reached set in the level's colour, a backward's marked set in warm white." }),
  ]);
  const tilesLegend = el("div", { class: "legend" }, [key("#3a3a37", "wall"), key("#784638", "spikes"), key("#826e32", "spring"), key("#467846", "fruit")]);
  const help = el("details", { class: "help" }, [
    el("summary", { text: "How to read this" }),
    el("p", { class: "note", text: "One cell is one player pixel. Brightness is the number of states in the cell, on a log scale against the level's largest cell over the run, so a frame's brightness is comparable to the next one's." }),
    el("p", { class: "note", text: "A horizon H asks: is there a win by frame H? The ladder answers it level by level, coarse to exact: each level's forward sweeps the frames out to H, then its backward marks the states that can still win by H, and the next level searches only inside those marks. A refuted horizon stops at the first level with no win, so nothing on screen wins there - that is the answer, not missing data." }),
    el("p", { class: "note", text: "The backward is drawn by the frame each marked state was first reached at (this tree stores no per-mark distance), swept from the horizon back to frame 1." }),
    el("p", { class: "note", text: "Height map: the levels collapsed into seven bands, each cell in the colour of the finest band whose set still contains it (forward and backward alike). The broad coarse bands are dark; lightness climbs with the band, so the exact route is the brightest thing on screen (the two thinnest bands carry a one-cell halo)." }),
    el("p", { class: "note", text: "A win marker sits on the cell the winning state left the room from; the readout under the room names its count and frame." }),
    el("div", { class: "keys" }, [
      el("kbd", { text: "space" }), el("span", { text: "play / pause" }),
      el("kbd", { text: "← →" }), el("span", { text: "one step (shift: ten)" }),
      el("kbd", { text: "[ ]" }), el("span", { text: "previous / next pass" }),
      el("kbd", { text: "home end" }), el("span", { text: "start / end of the horizon" }),
      el("kbd", { text: "1 2 3" }), el("span", { text: "speed" }),
    ]),
  ]);
  const legendCard = el("section", { class: "card legend-card" }, [legendFull, legendSweep, bandBlock, levelBlock, block3d, el("div", { class: "legend-title", text: "the room" }), tilesLegend, help]);
  function syncLegend() {
    const is3d = st.grain === "3d";
    show(legendFull, st.look === "full" && !is3d);
    show(legendSweep, st.look === "sweep" && !is3d);
    show(bandBlock, st.look === "last" && !is3d);
    show(levelBlock, st.look !== "last" || is3d);
    show(block3d, is3d);
  }

  const sideCol = el("aside", { class: "space-side" }, [optionCard, ladderCard, legendCard]);
  root.append(stageCol, bar, sideCol);

  // ---- the ladder panel ------------------------------------------------------------
  let ladderFor = -1;
  const ladderRows = new Map<number, { row: HTMLElement; ph: HTMLElement }>();
  function buildLadder() {
    if (ladderFor === st.h) return;
    ladderFor = st.h;
    const hr = run.horizons[st.h];
    const v = horizonVerdict(run, hr);
    ladderTitle.textContent = `The ladder at h${hr.h}`;
    ladderVerdict.textContent = v.long;
    ladderVerdict.dataset.kind = v.kind;
    hPicker.root.dataset.kind = v.kind;
    ladderList.replaceChildren();
    ladderRows.clear();
    for (let level = 0; level < LEVELS; level++) {
      const lr = hr.levels.find((l) => l.level === level);
      const name = level === 16 ? "exact" : `${level} bit${level === 1 ? "" : "s"}`;
      const result = !lr ? "not run" : lr.refuted ? `no win by f${hr.h}` : lr.first_win != null ? `win f${lr.first_win}` : "–";
      const ph = el("span", { class: "ph" });
      const row = el(
        "button",
        {
          type: "button",
          class: `ladder-row${!lr ? " not-run" : lr.refuted ? " refuted" : ""}`,
          disabled: !lr,
          title: !lr ? `level ${level} did not run at h${hr.h}: the ladder stopped below it` : `jump to level ${level}'s forward at h${hr.h}`,
        },
        [
          el("i", { class: "dot", style: `background:${levelCss(level)}` }),
          el("span", { class: "lv" }, [el("b", { text: `L${level}` }), ` ${name}`]),
          el("span", { class: "res", text: result }),
          el("span", { class: "mk", text: lr?.marked != null ? fmtCompact(lr.marked) : "" }),
          ph,
        ],
      );
      if (lr) row.addEventListener("click", () => jumpLevel(lr));
      ladderList.append(row);
      ladderRows.set(level, { row, ph });
    }
  }
  function syncLadder(pass: Pass) {
    for (const [level, r] of ladderRows) {
      const on = level === pass.lr.level;
      r.row.classList.toggle("on", on);
      r.row.setAttribute("aria-current", on ? "true" : "false");
      r.ph.textContent = on ? (pass.phase === "fwd" ? "fwd" : "bwd") : "";
      r.ph.dataset.phase = pass.phase;
    }
  }
  function jumpLevel(lr: LevelRun) {
    const tl = timeline(st.h);
    const own = tl.find((p) => p.lr === lr);
    if (!own) return;
    if (byPass()) st.pass = own.index;
    else st.step = own.start;
    stop();
    render();
    onState();
  }

  // ---- grid panels -----------------------------------------------------------------
  interface Panel {
    lr: LevelRun | null;
    level: number;
    root: HTMLElement;
    state: HTMLElement;
    canvas: HTMLCanvasElement;
    overlay: HTMLCanvasElement;
  }
  let panels: Panel[] = [];
  let panelsFor = -1;
  function buildPanels() {
    const hr = run.horizons[st.h];
    if (panelsFor === st.h) return;
    panelsFor = st.h;
    clear(gridBox);
    panels = [];
    for (let level = 0; level < LEVELS; level++) {
      const lr = hr.levels.find((l) => l.level === level) ?? null;
      const c = el("canvas", { "aria-label": `level ${level}` });
      const o = el("canvas", { class: "room-overlay" });
      const name = level === 16 ? "exact" : `${level} bit${level === 1 ? "" : "s"}`;
      // The panel's state (playing / done / not yet) sits in its own
      // corner so the label does not wrap on a phone.
      const state = el("div", { class: "state" });
      const lab = el("div", { class: "lab" }, [
        el("i", { style: `background:${levelCss(level)}` }),
        lr ? `${name}${lr.refuted ? " · refuted" : lr.first_win != null ? ` · win f${lr.first_win}` : ""}` : `${name} · not run`,
      ]);
      const p = el("div", { class: `panel${lr ? "" : " not-run"}`, role: lr ? "button" : undefined, tabindex: lr ? 0 : undefined, title: lr ? `open level ${level} in Room` : undefined }, [c, o, lab, state]);
      if (lr) {
        const open = () => {
          // Tap a panel: open that level in Room where the sequence has
          // it - mid-sweep if it is playing, at the end of its last pass
          // if it is done, at the start of its first pass if not yet.
          const tl = timeline(st.h);
          const { pass } = locate(tl, st.step);
          const own = tl.filter((x) => x.lr === lr);
          const last = own[own.length - 1];
          if (!last) return;
          if (last.index < pass.index) st.step = last.start + last.frames.length - 1;
          else if (!own.some((x) => x.index === pass.index)) st.step = own[0].start;
          grainChips.set("room");
          setGrain("room");
          onState();
        };
        p.addEventListener("click", open);
        p.addEventListener("keydown", (ev) => {
          if (ev.key === "Enter" || ev.key === " ") {
            ev.preventDefault();
            open();
          }
        });
      }
      gridBox.append(p);
      panels.push({ lr, level, root: p, state, canvas: c, overlay: o });
    }
    fitGrid();
  }

  /** The wide layout: the stage has a fixed height to fill. */
  const wide = window.matchMedia("(min-width: 1024px) and (min-height: 620px)");
  /** On a wide screen the grid's panels are sized to fit the stage without
   *  scrolling: the column count that makes a panel biggest. On a phone the
   *  grid flows (CSS auto-fill) and the page scrolls. */
  function fitGrid() {
    if (st.grain !== "grid" || !wide.matches) {
      gridBox.style.removeProperty("grid-template-columns");
      return;
    }
    const W = stage.clientWidth;
    const H = stage.clientHeight;
    const gap = 8;
    let best = 0;
    let cols = 4;
    for (let c = 1; c <= LEVELS; c++) {
      const r = Math.ceil(LEVELS / c);
      const w = Math.min((W - gap * (c - 1)) / c, ((H - gap * (r - 1)) / r) * aspect);
      if (w > best) {
        best = w;
        cols = c;
      }
    }
    gridBox.style.gridTemplateColumns = `repeat(${cols}, ${Math.max(60, Math.floor(best))}px)`;
  }

  // ---- layout + render -------------------------------------------------------------
  function setGrain(g: Grain) {
    const was = byPass();
    st.grain = g;
    convertPlayhead(was);
    stop();
    layout();
    if (!wide.matches) stageCol.scrollIntoView({ behavior: "smooth", block: "start" });
  }
  function setMode3(m: Mode3) {
    const was = byPass();
    st.mode3 = m;
    convertPlayhead(was);
    stop();
    layout();
  }
  /** The playhead keeps its place when the timeline's unit changes. */
  function convertPlayhead(wasByPass: boolean) {
    const tl = timeline(st.h);
    if (byPass() && !wasByPass) st.pass = locate(tl, st.step).pass.index;
    if (!byPass() && wasByPass) st.step = tl[Math.min(st.pass, tl.length - 1)]?.start ?? 0;
  }

  function layout() {
    const grid = st.grain === "grid";
    const is3d = st.grain === "3d";
    show(roomWrap, !grid && !is3d);
    show(gridBox, grid);
    show(view3dWrap, is3d);
    show(lookChips.root, !is3d);
    show(mode3Chips.root, is3d);
    stage.dataset.grain = st.grain;
    syncLegend();
    buildLadder();
    if (grid) buildPanels();
    if (is3d) view3();
    paintBackdrop();
    render();
  }

  function paintBackdrop() {
    const tl = timeline(st.h);
    const total = stepsOf(tl);
    const perPass = byPass();
    scrub.backdrop((ctx, w, h) => {
      ctx.font = "600 10px " + getComputedStyle(document.body).fontFamily;
      ctx.textBaseline = "middle";
      ctx.textAlign = "left";
      // The same slot mapping as the knob: `total` (or `tl.length`) slots.
      for (const p of tl) {
        const x0 = perPass ? (p.index / tl.length) * w : (p.start / total) * w;
        const x1 = perPass ? ((p.index + 1) / tl.length) * w : ((p.start + p.frames.length) / total) * w;
        const bw = Math.max(1, x1 - x0 - 0.5);
        ctx.fillStyle = rgbCss(levelColor(p.lr.level), p.phase === "fwd" ? 0.7 : 0.3);
        ctx.fillRect(x0, 0, bw, h);
        // A backward's band carries a warm hairline along its top: the
        // marks it is producing.
        if (p.phase === "bwd") {
          ctx.fillStyle = "rgba(255,245,225,0.55)";
          ctx.fillRect(x0, 0, bw, 2);
        }
        const label = p.lr.level === 16 ? "ex" : `L${p.lr.level}`;
        if (bw > 22) {
          ctx.fillStyle = p.phase === "fwd" ? "rgba(0,0,0,0.8)" : "rgba(255,255,255,0.75)";
          ctx.fillText(label, x0 + 4, h / 2 + 0.5);
        }
      }
    });
  }

  const setLoading = (on: boolean) => {
    stage.dataset.loading = on ? "true" : "false";
  };

  let renderToken = 0;
  function render() {
    const token = ++renderToken;
    // A scrub / jump moved the step under the float playhead: follow it.
    if (!st.playing) st.pos = (byPass() ? passCum(st.h) : stepCum(st.h))[Math.max(0, byPass() ? st.pass : st.step)] ?? 0;
    const hr = run.horizons[st.h];
    const tl = timeline(st.h);
    const need = hr.levels;
    const ready = need.every((l) => have(l));
    if (!ready) {
      setLoading(true);
      ensure(need)
        .then(() => {
          if (token === renderToken) render();
        })
        .catch((e: Error) => {
          if (token !== renderToken) return;
          setLoading(false);
          passTitle.replaceChildren(el("div", {}, [el("b", { text: "could not load this horizon's files" })]), el("div", { class: "verdict", text: e.message }));
        });
      return;
    }
    setLoading(false);
    if (tl.length === 0) return;

    if (st.grain === "3d") {
      render3d(hr, tl);
      return;
    }
    if (st.grain === "passes") {
      st.pass = Math.max(0, Math.min(tl.length - 1, st.pass));
      const pass = tl[st.pass];
      scrub.set(st.pass, tl.length - 1, `${pass.phase === "fwd" ? "forward" : "backward"} L${pass.lr.level}`);
      const built = passScene(hr, pass);
      shown(built);
      renderer.render(canvas, built.scene, overlay);
      describePass(hr, pass, null, built.off);
      return;
    }
    const total = stepsOf(tl);
    st.step = Math.max(0, Math.min(total - 1, st.step));
    const { pass, i } = locate(tl, st.step);
    scrub.set(st.step, total - 1, `${pass.phase === "fwd" ? "f" : "← f"}${pass.frames[i]} · L${pass.lr.level}`);
    if (st.grain === "room") {
      const built = roomScene(hr, pass, i);
      shown(built);
      renderer.render(canvas, built.scene, overlay);
      describePass(hr, pass, i, built.off);
    } else {
      // The grid walks the same timeline as the room: only the current
      // pass's level animates; levels whose passes are done sit at their
      // final state, levels not yet reached are blank.
      const f = pass.frames[i];
      let offRoom = 0;
      for (const p of panels) {
        let built = empty();
        let state: "animating" | "done" | "not yet" | "not run" = "not run";
        if (p.lr) {
          const own = tl.filter((x) => x.lr === p.lr);
          const last = own[own.length - 1];
          if (own.some((x) => x.index === pass.index)) {
            built = panelScene(p.lr, pass.phase, f, hr.h);
            state = "animating";
          } else if (last && last.index < pass.index) {
            built = finalScene(p.lr, last, hr.h);
            state = "done";
          } else state = "not yet";
        }
        offRoom = Math.max(offRoom, built.offRoom);
        renderer.render(p.canvas, built.scene, p.overlay);
        p.root.classList.toggle("not-run", state === "not run" || state === "not yet");
        p.root.classList.toggle("animating", state === "animating");
        p.state.textContent = state === "animating" ? `${pass.phase === "fwd" ? "forward" : "backward"} f${f}` : state === "done" ? "done" : state === "not yet" ? "not yet" : "";
      }
      shown({ ...empty(), offRoom });
      describePass(hr, pass, i, 0);
    }
  }

  // ---- the readout under the stage: cell probe, colour scale, win key ---------------------
  let lastBuilt: Built = empty();
  let probeIdx = -1;
  /** What the stage shows now: the probe's source, the scale, the win key. */
  function shown(built: Built) {
    lastBuilt = built;
    showScale(st.grain === "grid" || st.grain === "3d" ? undefined : built.scale);
    const off = built.offRoom > 0 ? `${fmtInt(built.offRoom)} won state${built.offRoom === 1 ? "" : "s"} recorded in the next room by an old export: exit cell unknown, not drawn` : "";
    winKey.replaceChildren();
    if (built.scene.wins.length || off) {
      winKey.append(winKeyIcon(16));
      winKey.append(
        el("span", {
          text: built.scene.wins.length ? `win${built.scene.wins.length === 1 ? "" : "s"}: ${built.scene.wins.map((w) => `${fmtInt(w.count)} at (${renderer.cellXY(w.idx).join(", ")}) from f${w.frame}`).join(" · ")}` : "no win drawn",
        }),
      );
      if (off) winKey.append(el("span", { class: "warn", text: off }));
    }
    if (probeIdx >= 0) probeAt(probeIdx);
    else probeHint();
  }

  /** The colour scale: the moving set's ramp, 1 .. max. */
  function showScale(s: Scale | undefined) {
    if (!s || st.look === "last") {
      scaleEl.replaceChildren();
      return;
    }
    const stops = [0, 12, 24, 36, 48, 63].map((i) => rgbCss(s.ramp[i])).join(", ");
    scaleEl.replaceChildren(el("span", { class: "muted", text: s.what }), el("span", { text: "1" }), el("i", { style: `background:linear-gradient(to right, ${stops})` }), el("span", { text: fmtCompact(s.max) }));
  }

  function probeHint() {
    const touch = window.matchMedia("(hover: none)").matches;
    probeEl.replaceChildren(
      el("span", {
        class: "muted",
        text: st.grain === "3d" ? "drag to orbit · pinch or scroll to zoom" : st.grain === "grid" ? "tap a panel to open that level" : `${touch ? "press and drag on" : "hover"} the room to read a cell`,
      }),
    );
  }
  function probeAt(idx: number) {
    const [x, y] = renderer.cellXY(idx);
    const rows = lastBuilt.probe.filter((p) => p.grid[idx] > 0).map((p) => `${fmtInt(p.grid[idx])} ${p.name}`);
    const win = lastBuilt.scene.wins.find((w) => w.idx === idx);
    if (win) rows.unshift(`${fmtInt(win.count)} won from here (first at f${win.frame})`);
    probeEl.replaceChildren(el("b", { text: `cell (${x}, ${y})` }), el("span", { text: rows.length ? rows.join(" · ") : "no states" }));
  }

  // ---- stage copy: the pass title and the transport's status --------------------------------
  function describePass(hr: HorizonRun, pass: Pass, i: number | null, off: number) {
    const lr = pass.lr;
    const tl = timeline(st.h);
    const states = lr.frame_states.slice(0, hr.h + 1).reduce((a, b) => a + b, 0);
    const verdict = lr.refuted ? `no win by f${hr.h}: refuted` : lr.first_win != null ? `first win f${lr.first_win}${lr.marked != null ? ` · ${fmtCompact(lr.marked)} marked` : ""}` : "";
    const pill = el("span", { class: "ph", "data-phase": pass.phase, style: pass.phase === "fwd" ? `background:${levelCss(lr.level)}` : undefined, text: pass.phase === "fwd" ? "fwd" : "bwd" });
    passTitle.replaceChildren(
      el("div", { class: "t1" }, [pill, el("b", { text: `Level ${lr.level} · ${levelName(lr)}` }), ` · ${pass.phase === "fwd" ? "forward" : "backward"}`]),
      el("div", { class: `t2${lr.refuted ? " refuted" : ""}`, text: verdict }),
    );
    syncLadder(pass);

    // The status names the position: the frame and its counts.
    let l1: string;
    let l2: string;
    if (i == null) {
      l1 = `whole pass ${pass.index + 1} of ${tl.length}`;
      l2 = pass.phase === "fwd" ? `${fmtCompact(states)} reached by f${hr.h}` : `${fmtCompact(lr.marked ?? 0)} marked of ${fmtCompact(states)} (${((100 * (lr.marked ?? 0)) / Math.max(1, states)).toFixed(1)}%)`;
    } else if (pass.phase === "fwd") {
      const f = pass.frames[i];
      const before = lr.frame_states.slice(0, f).reduce((a, b) => a + b, 0);
      l1 = `frame ${f} of ${hr.h}`;
      l2 = `${fmtCompact(lr.frame_states[f] ?? 0)} new · ${fmtCompact(before)} before${off ? ` · ${fmtCompact(off)} off-grid` : ""}`;
    } else {
      const f = pass.frames[i];
      const soFar = lr.marks_by_layer.slice(f, hr.h + 1).reduce((a, b) => a + b, 0);
      l1 = `back to frame ${f}`;
      l2 = `${fmtCompact(lr.marks_by_layer[f] ?? 0)} marked at f${f} · ${fmtCompact(soFar)} so far`;
    }
    if (st.grain === "grid" && i != null) l2 = `level ${lr.level} plays; earlier levels final`;
    status.replaceChildren(el("div", { class: "l1" }, [l1, el("span", { class: "of", text: ` · pass ${pass.index + 1}/${tl.length}` })]), el("div", { class: "l2", text: l2 }));
  }

  // ---- the 3D grain -------------------------------------------------------------------
  // World units are cells. Columns: a cell's height is log(count) against
  // the level's max over the run (as the 2D brightness), capped at HMAX.
  // Stack: each pass is a layer LAYER tall; the whole stack of a horizon
  // is built once and pass p draws its first prefix[p + 1] instances.
  const HMAX = 36;
  const LAYER = 1;
  const FOOT = 0.88;
  const PAD = (1 - FOOT) / 2;
  let v3: View3D | null = null;
  const inst3 = new Instances(1 << 15);
  /** What the dynamic buffer holds: "columns" (rebuilt per render) or a horizon's stack. */
  let uploaded3 = "";
  function view3(): View3D | null {
    if (v3) return v3;
    try {
      v3 = new View3D({ width: run.cell_box.w, height: run.cell_box.h });
    } catch (e) {
      view3dWrap.replaceChildren(el("div", { class: "err", text: `The 3D view needs WebGL2: ${(e as Error).message}` }));
      return null;
    }
    view3dWrap.prepend(v3.canvas);
    const stat = staticScene();
    v3.setStatic(stat.data, stat.count);
    const v = v3;
    view3dTools.append(button("top", () => v.topDown(), "small", "look straight down"), button("reset", () => v.resetView(), "small", "reset the camera"));
    return v3;
  }
  /** The floor and the room's tiles, once. */
  function staticScene(): Instances {
    const b = run.cell_box;
    const t = run.tiles;
    const s = new Instances(t.w * t.h + 2);
    s.push(0, 0, -1, 1, b.w, b.h, 0.075, 0.075, 0.07);
    const solid = [58 / 255, 58 / 255, 55 / 255];
    const far = [40 / 255, 40 / 255, 38 / 255];
    const spike = [120 / 255, 70 / 255, 60 / 255];
    const spring = [130 / 255, 110 / 255, 50 / 255];
    const fruit = [70 / 255, 120 / 255, 70 / 255];
    for (let ty = 0; ty < t.h; ty++) {
      for (let tx = 0; tx < t.w; tx++) {
        const i = tx + ty * t.w;
        const id = t.ids[i];
        const x0 = Math.max(0, tx * 8 - b.x0);
        const x1 = Math.min(b.w, tx * 8 + 8 - b.x0);
        const z0 = Math.max(0, ty * 8 - b.y0);
        const z1 = Math.min(b.h, ty * 8 + 8 - b.y0);
        if (x1 <= x0 || z1 <= z0) continue;
        let c: number[] | null = null;
        let h = 0;
        if (t.solid[i]) {
          c = tx < 16 ? solid : far;
          h = 2.5;
        } else if ([17, 27, 43, 59].includes(id)) {
          c = spike;
          h = 0.7;
        } else if (id === 18) {
          c = spring;
          h = 0.5;
        } else if (id === 26) {
          c = fruit;
          h = 1.2;
        }
        if (!c) continue;
        s.push(x0 + 0.1, z0 + 0.1, 0, h, x1 - x0 - 0.2, z1 - z0 - 0.2, c[0], c[1], c[2]);
      }
    }
    return s;
  }
  const marksCumMaxCache = new WeakMap<LevelFiles, number>();
  function marksCumMax(files: LevelFiles, h: number): number {
    let m = marksCumMaxCache.get(files);
    if (m == null) {
      m = 1;
      if (files.layers) {
        const g = cumTo(files.layers, h).grid;
        for (let i = 0; i < ncell; i++) if (g[i] > m) m = g[i];
      }
      marksCumMaxCache.set(files, m);
    }
    return m;
  }
  /** Columns at (pass, i): forward = the accumulated count as a dim
   *  column with the frontier's share as a bright cap; backward = the
   *  reached set as a slab, the marks as warm-white columns (so far dim,
   *  this iteration bright). Returns the off-grid count. */
  function columnsScene(hr: HorizonRun, pass: Pass, i: number): number {
    const files = have(pass.lr);
    inst3.reset();
    if (!files) return 0;
    const w = run.cell_box.w;
    const f = pass.frames[i];
    const col = (c: RGB): [number, number, number] => [c[0] / 255, c[1] / 255, c[2] / 255];
    const put = (c: number, y0: number, h: number, rgb: [number, number, number]) => {
      const x = c % w;
      inst3.push(x + PAD, (c - x) / w + PAD, y0, h, FOOT, FOOT, rgb[0], rgb[1], rgb[2]);
    };
    let base: Float32Array;
    let top: Float32Array;
    let lmax: number;
    let dim: [number, number, number];
    let bright: [number, number, number];
    let off = 0;
    if (pass.phase === "fwd") {
      base = cumGrid(files.frames, 0, f - 1).grid;
      top = new Float32Array(ncell);
      off = addSparse(top, files.frames.cells(f));
      lmax = Math.log1p(Math.max(1, files.cumMax));
      dim = col(levelRamp(pass.lr.level, "dim")[38]);
      bright = col(levelRamp(pass.lr.level, "bright")[54]);
    } else {
      // The reached set, as a slab under the marks.
      const reached = cumTo(files.frames, hr.h).grid;
      const slab = col(levelRamp(pass.lr.level, "band")[30]);
      for (let c = 0; c < ncell; c++) if (reached[c] > 0) put(c, 0, 0.45, slab);
      if (!files.layers) return 0;
      base = cumGrid(files.layers, f + 1, hr.h).grid;
      top = new Float32Array(ncell);
      addSparse(top, files.layers.cells(f));
      lmax = Math.log1p(Math.max(1, marksCumMax(files, hr.h)));
      dim = col(marksRamp("dim")[40]);
      bright = col(marksRamp("bright")[56]);
    }
    const y0 = pass.phase === "fwd" ? 0 : 0.45;
    for (let c = 0; c < ncell; c++) {
      const v = base[c];
      const m = top[c];
      if (v <= 0 && m <= 0) continue;
      const hb = v > 0 ? (Math.log1p(v) / lmax) * HMAX : 0;
      const ht = (Math.log1p(v + m) / lmax) * HMAX;
      if (v > 0) put(c, y0, Math.max(0.3, hb), dim);
      if (m > 0) put(c, y0 + hb, Math.max(0.4, ht - hb), bright);
    }
    return off;
  }
  const stacks = new Map<number, { inst: Instances; prefix: number[] }>();
  /** A horizon's whole stack: pass q's set as a layer of cubes at height q. */
  function stackOf(hr: HorizonRun, tl: Pass[]): { inst: Instances; prefix: number[] } {
    let s = stacks.get(st.h);
    if (s) return s;
    const inst = new Instances(1 << 16);
    const prefix = [0];
    const w = run.cell_box.w;
    const warm = marksRamp("bright")[52];
    for (const p of tl) {
      const files = have(p.lr);
      const g = !files ? null : p.phase === "fwd" ? cumTo(files.frames, hr.h).grid : files.layers ? cumTo(files.layers, hr.h).grid : null;
      if (g) {
        const lc = levelColor(p.lr.level);
        // A backward's layer: warm white with a trace of the level's hue.
        const c: RGB = p.phase === "fwd" ? lc : [0.8 * warm[0] + 0.2 * lc[0], 0.8 * warm[1] + 0.2 * lc[1], 0.8 * warm[2] + 0.2 * lc[2]];
        const y0 = p.index * LAYER;
        for (let i = 0; i < ncell; i++) {
          if (g[i] <= 0) continue;
          const x = i % w;
          inst.push(x + PAD, (i - x) / w + PAD, y0, LAYER * FOOT, FOOT, FOOT, c[0] / 255, c[1] / 255, c[2] / 255);
        }
      }
      prefix.push(inst.count);
    }
    s = { inst, prefix };
    stacks.set(st.h, s);
    return s;
  }
  function render3d(hr: HorizonRun, tl: Pass[]) {
    const v = view3();
    if (!v) return;
    shown(empty());
    if (st.mode3 === "stack") {
      st.pass = Math.max(0, Math.min(tl.length - 1, st.pass));
      const pass = tl[st.pass];
      scrub.set(st.pass, tl.length - 1, `${pass.phase === "fwd" ? "forward" : "backward"} L${pass.lr.level}`);
      const s = stackOf(hr, tl);
      const key = `stack:${st.h}`;
      if (uploaded3 !== key) {
        v.setDynamic(s.inst.data, s.inst.count);
        uploaded3 = key;
      }
      v.setDynamicLimit(s.prefix[st.pass + 1]);
      v.setDimBelow(st.pass * LAYER, 0.62);
      describePass(hr, pass, null, 0);
      return;
    }
    const total = stepsOf(tl);
    st.step = Math.max(0, Math.min(total - 1, st.step));
    const { pass, i } = locate(tl, st.step);
    scrub.set(st.step, total - 1, `${pass.phase === "fwd" ? "f" : "← f"}${pass.frames[i]} · L${pass.lr.level}`);
    const off = columnsScene(hr, pass, i);
    v.setDynamic(inst3.data, inst3.count);
    v.setDimBelow(-1e9, 1);
    uploaded3 = "columns";
    describePass(hr, pass, i, off);
  }

  // ---- probe (press / drag on the room, hover with a mouse) ------------------------------
  let probeTimer = 0;
  function probeEvent(ev: PointerEvent) {
    const r = canvas.getBoundingClientRect();
    const lx = Math.floor(((ev.clientX - r.left) / r.width) * run.cell_box.w);
    const ly = Math.floor(((ev.clientY - r.top) / r.height) * run.cell_box.h);
    if (lx < 0 || ly < 0 || lx >= run.cell_box.w || ly >= run.cell_box.h) return;
    clearTimeout(probeTimer);
    probeIdx = lx + ly * run.cell_box.w;
    probeAt(probeIdx);
  }
  canvas.addEventListener("pointerdown", (ev) => {
    canvas.setPointerCapture(ev.pointerId);
    probeEvent(ev);
  });
  canvas.addEventListener("pointermove", (ev) => {
    if (canvas.hasPointerCapture(ev.pointerId) || ev.pointerType === "mouse") probeEvent(ev);
  });
  const hideProbe = () => {
    clearTimeout(probeTimer);
    probeTimer = window.setTimeout(() => {
      probeIdx = -1;
      probeHint();
    }, 2500);
  };
  canvas.addEventListener("pointerup", hideProbe);
  canvas.addEventListener("pointercancel", hideProbe);
  canvas.addEventListener("pointerleave", (ev) => {
    if (ev.pointerType === "mouse") hideProbe();
  });

  // ---- playback ----------------------------------------------------------------------
  let raf = 0;
  let last = 0;
  let acc = 0;
  function tick(t: number) {
    if (!st.playing) return;
    // Detached (another tab, or another run replaced this view): pause
    // rather than animate into a canvas nobody sees.
    if (!root.isConnected) {
      stop();
      return;
    }
    const sp = SPEEDS[st.speed];
    const units = last ? ((t - last) / 1000) * (byPass() ? sp.passes : sp.steps) : 0;
    last = t;
    let n = 0;
    if (st.pacing === "uniform") {
      acc += units;
      n = Math.floor(acc);
      acc -= n;
    } else {
      // Advance the float playhead by uniform-step units; the step it
      // lands in is set by the log's weights.
      const passes = byPass();
      const cum = passes ? passCum(st.h) : stepCum(st.h);
      const cur = passes ? st.pass : st.step;
      st.pos = Math.max(st.pos, cum[cur]) + units;
      n = Math.min(cum.length - 2, indexAt(cum, st.pos)) - cur;
    }
    if (n > 0) {
      const tl = timeline(st.h);
      const end = byPass() ? tl.length - 1 : stepsOf(tl) - 1;
      const cur = byPass() ? st.pass : st.step;
      // Play stops at the end of the horizon: the horizon changes only by
      // the picker, so what is on screen is always the horizon it names.
      const next = Math.min(end, cur + n);
      if (byPass()) st.pass = next;
      else st.step = next;
      if (next >= end) {
        st.playing = false;
        syncPlay();
        render();
        onState();
      } else render();
    }
    if (st.playing) raf = requestAnimationFrame(tick);
  }
  function syncPlay() {
    playBtn.replaceChildren(icon(st.playing ? "pause" : "play"));
    playBtn.title = st.playing ? "Pause (space)" : "Play (space)";
    playBtn.setAttribute("aria-label", playBtn.title);
    if (st.playing) {
      last = 0;
      acc = 0;
      cancelAnimationFrame(raf);
      raf = requestAnimationFrame(tick);
    }
  }
  function stop() {
    if (!st.playing) return;
    st.playing = false;
    syncPlay();
  }
  function togglePlay() {
    if (!st.playing) {
      const tl = timeline(st.h);
      const finished = byPass() ? st.pass >= tl.length - 1 : st.step >= stepsOf(tl) - 1;
      if (finished) {
        st.step = 0;
        st.pass = 0;
        render();
      }
    }
    st.playing = !st.playing;
    syncPlay();
    if (!st.playing) onState();
  }
  function jumpPass(d: number) {
    const tl = timeline(st.h);
    if (byPass()) st.pass = Math.max(0, Math.min(tl.length - 1, st.pass + d));
    else {
      const { pass } = locate(tl, st.step);
      st.step = tl[Math.max(0, Math.min(tl.length - 1, pass.index + d))].start;
    }
    stop();
    render();
    onState();
  }
  function nudge(d: number) {
    if (byPass()) st.pass += d;
    else st.step += d;
    stop();
    render();
    onState();
  }
  document.addEventListener("keydown", (ev) => {
    if (!root.isConnected || ev.metaKey || ev.ctrlKey || ev.altKey) return;
    const tag = (ev.target as HTMLElement).tagName;
    if (tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT") return;
    switch (ev.key) {
      case " ":
        ev.preventDefault();
        togglePlay();
        break;
      case "ArrowRight":
      case "ArrowLeft":
        ev.preventDefault();
        nudge((ev.key === "ArrowRight" ? 1 : -1) * (ev.shiftKey ? 10 : 1));
        break;
      case "[":
        jumpPass(-1);
        break;
      case "]":
        jumpPass(1);
        break;
      case "Home":
        ev.preventDefault();
        if (byPass()) st.pass = 0;
        else st.step = 0;
        stop();
        render();
        onState();
        break;
      case "End":
        ev.preventDefault();
        if (byPass()) st.pass = timeline(st.h).length - 1;
        else st.step = lastStep(st.h);
        stop();
        render();
        onState();
        break;
      case "1":
      case "2":
      case "3":
        st.speed = Number(ev.key) - 1;
        speedChips.set(st.speed);
        onState();
        break;
    }
  });

  // ---- state <-> URL -----------------------------------------------------------------
  function params(): string {
    const p = new URLSearchParams();
    if (st.h !== defH) p.set("h", String(run.horizons[st.h].h));
    if (byPass()) p.set("p", String(st.pass));
    else if (st.step !== lastStep(st.h)) p.set("t", String(st.step));
    if (st.grain !== "room") p.set("g", st.grain);
    if (st.grain === "3d" && st.mode3 !== "columns") p.set("m", st.mode3);
    if (st.look !== "last") p.set("l", st.look);
    if (st.speed !== 1) p.set("s", String(st.speed));
    if (st.pacing !== "uniform") p.set("pc", st.pacing);
    return p.toString();
  }
  function apply(p: URLSearchParams) {
    stop();
    const hv = p.has("h") ? Number(p.get("h")) : NaN;
    const hi = run.horizons.findIndex((x) => x.h === hv);
    st.h = hi >= 0 ? hi : defH;
    const g = p.get("g") as Grain | null;
    st.grain = g && GRAINS.includes(g) ? g : "room";
    st.mode3 = p.get("m") === "stack" ? "stack" : "columns";
    const l = p.get("l") as Look | null;
    st.look = l && LOOKS.includes(l) ? l : "last";
    const s = p.has("s") ? Number(p.get("s")) : 1;
    st.speed = Number.isInteger(s) && s >= 0 && s < SPEEDS.length ? s : 1;
    st.pacing = p.get("pc") === "real" ? "real" : "uniform";
    const tl = timeline(st.h);
    if (byPass()) {
      const pv = p.has("p") ? Number(p.get("p")) : tl.length - 1;
      st.pass = Number.isFinite(pv) ? Math.max(0, Math.min(tl.length - 1, pv)) : tl.length - 1;
      st.step = tl[st.pass]?.start ?? 0;
    } else {
      const tv = p.has("t") ? Number(p.get("t")) : lastStep(st.h);
      st.step = Number.isFinite(tv) ? Math.max(0, Math.min(lastStep(st.h), tv)) : lastStep(st.h);
      st.pass = locate(tl, st.step).pass.index;
    }
    hPicker.set(st.h);
    grainChips.set(st.grain);
    mode3Chips.set(st.mode3);
    lookChips.set(st.look);
    speedChips.set(st.speed);
    pacingChips.set(st.pacing);
    layout();
  }

  new ResizeObserver(() => {
    fitGrid();
    render();
  }).observe(stage);
  // Open on the answer: the optimal horizon's ladder at its last step -
  // the exact level's marked route over everything that was searched.
  // Play from there starts the sweep over from the top.
  apply(new URLSearchParams());
  return { root, params, apply };
}
