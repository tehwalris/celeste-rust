// The search as space. Four axes, each bound to one control (README):
//
//   horizon  - the win frame H everything on screen is at (default: the
//              winning one); the horizon row in the transport bar.
//   time     - the position in H's ladder: the passes in the order they
//              ran (L0 forward, L0 backward, L1 forward, ...), a forward
//              sweeping frames 0..H and a backward sweeping iterations
//              H-1..1; the scrubber, the step / pass buttons, Play.
//   grain    - Room (the current pass, big) / Grid (every level's panel,
//              one playing at a time in ladder order) / Passes (a step is
//              a whole pass) / 3D (the room as columns or a stack of
//              layers, orbited; view3d.ts).
//   look     - Full (accumulated under moving, closed levels' bands, wins)
//              / Sweep only (just what moves) / Height map (the bands).
//
// The renderer composites per-cell count grids over the room's tiles
// (room.ts); this file decides which grids, in which ramp, at which alpha.
//
// Layout: the stage (room / grid / 3D) with the transport under it -
// fixed to the bottom of a phone, in the flow on a wide screen where the
// options and the legend sit in a sidebar. Status lives in exactly one
// place each: the caption on the room names the PASS (which level, which
// phase, its verdict); the transport's status names the POSITION (frame,
// counts); the scrubber's bubble names the step while it is held.

import type { FramesBin, HorizonRun, LevelRun, Run } from "./data";
import { fmtCompact, fmtInt, levelName, loadFrames, loadLayers } from "./data";
import { levelCss, levelRamp, marksRamp, heightBand, bandColor, bandHalo, movingColor, HEIGHT_BANDS, winColor, rgbCss, levelColor, LEVELS, type RGB } from "./color";
import { addSparse, RoomRenderer, sparseMax, type HeatLayer, type Ring, type Scene } from "./room";
import { button, chips, clear, el, icon, scrubber, show } from "./ui";
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

interface LevelFiles {
  frames: FramesBin;
  layers: FramesBin | null;
  frameMax: number;
  cumMax: number;
  marksMax: number;
  winRings: Ring[];
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
}

const SPEEDS: { label: string; steps: number; passes: number }[] = [
  { label: "slow", steps: 15, passes: 1 },
  { label: "normal", steps: 60, passes: 2 },
  { label: "fast", steps: 250, passes: 4 },
];
const GRAINS: Grain[] = ["room", "grid", "passes", "3d"];
const LOOKS: Look[] = ["full", "sweep", "last"];

export function spaceView(run: Run, onState: () => void): View {
  const renderer = new RoomRenderer(run.cell_box, run.tiles);
  const ncell = run.cell_box.w * run.cell_box.h;
  /** The winning horizon: the last one (the ladder confirmed it). */
  const winH = run.horizons.length - 1;

  const st = {
    h: winH,
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
        const winRings: Ring[] = [];
        const seen = new Set<number>();
        for (let f = 0; f < frames.nframes; f++) {
          const c = frames.cells(f);
          frameMax = Math.max(frameMax, sparseMax(c));
          addSparse(cum, c);
          const w = frames.wins(f);
          for (let i = 0; i < w.idx.length; i++) {
            if (!seen.has(w.idx[i])) {
              seen.add(w.idx[i]);
              winRings.push({ idx: w.idx[i], color: winColor });
            }
          }
          if (layers) marksMax = Math.max(marksMax, sparseMax(layers.cells(f)));
        }
        let cumMax = 1;
        for (let i = 0; i < ncell; i++) if (cum[i] > cumMax) cumMax = cum[i];
        const out = { frames, layers, frameMax, cumMax, marksMax, winRings };
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
  function cumGrid(bin: FramesBin, lo: number, hi: number): { grid: Float32Array; off: number } {
    const grid = new Float32Array(ncell);
    let off = 0;
    for (let f = Math.max(0, lo); f <= Math.min(hi, bin.nframes - 1); f++) off += addSparse(grid, bin.cells(f));
    return { grid, off };
  }
  function ringsUpTo(files: LevelFiles, f: number): Ring[] {
    const out: Ring[] = [];
    const seen = new Set<number>();
    for (let i = 0; i <= Math.min(f, files.frames.nframes - 1); i++) {
      const w = files.frames.wins(i);
      for (let k = 0; k < w.idx.length; k++) {
        if (!seen.has(w.idx[k])) {
          seen.add(w.idx[k]);
          out.push({ idx: w.idx[k], color: winColor });
        }
      }
    }
    return out;
  }

  /** The closed levels of `hr` below `level`, as receding bands. */
  function bands(hr: HorizonRun, level: number, layers: HeatLayer[], probe: Named[]) {
    const closed = hr.levels.filter((l) => l.level < level && !l.refuted && l.mlayers_file);
    closed.forEach((l, k) => {
      const fl = have(l);
      if (!fl || !fl.layers) return;
      const g = cumGrid(fl.layers, 0, hr.h);
      layers.push({ grid: g.grid, max: fl.marksMax, ramp: levelRamp(l.level, "band"), alpha: k === closed.length - 1 ? 0.5 : 0.35 });
      probe.push({ name: `marked, level ${l.level}`, grid: g.grid });
    });
  }

  /** The base of the Full look: level 0's whole reachable space, dim. */
  function base(hr: HorizonRun, layers: HeatLayer[], probe: Named[]) {
    const l0 = hr.levels.find((l) => l.level === 0);
    const f0 = l0 && have(l0);
    if (!l0 || !f0) return;
    const g = cumGrid(f0.frames, 0, hr.h);
    layers.push({ grid: g.grid, max: f0.cumMax, ramp: levelRamp(0, "dim"), alpha: 0.95 });
    probe.push({ name: "reached, level 0", grid: g.grid });
  }

  /** One level at (phase, f): the moving set bright over its own
   *  accumulated set (Full), or the moving set alone (Sweep). */
  function levelAt(lr: LevelRun, phase: Phase, f: number, h: number, full: boolean, layers: HeatLayer[], probe: Named[]): { rings: Ring[]; off: number; scale?: Scale } {
    const files = have(lr);
    if (!files) return { rings: [], off: 0 };
    let off = 0;
    let rings: Ring[] = [];
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
      if (full) rings = ringsUpTo(files, f);
    } else {
      if (full) {
        const g = cumGrid(files.frames, 0, h);
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
      if (full) rings = files.winRings;
    }
    return { rings, off, scale };
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
      const g = p.phase === "fwd" ? cumGrid(files.frames, 0, hr.h) : files.layers ? cumGrid(files.layers, 0, hr.h) : null;
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
    if (st.look === "last") {
      paintPasses(hr, timeline(st.h), pass.index, layers, probe);
      const off = paintCurrent(pass.lr, pass.phase, pass.frames[i], hr.h, layers, probe);
      return { scene: { layers, rings: [] }, probe, off };
    }
    const full = st.look === "full";
    if (full && pass.lr.level !== 0) base(hr, layers, probe);
    if (full) bands(hr, pass.lr.level, layers, probe);
    const { rings, off, scale } = levelAt(pass.lr, pass.phase, pass.frames[i], hr.h, full, layers, probe);
    return { scene: { layers, rings }, probe, off, scale };
  }

  /** A level after its last pass: everything it reached, dim, with all
   *  its marks (or, in the height map, the passes painted through it). */
  function finalScene(lr: LevelRun, last: Pass, h: number): Built {
    const layers: HeatLayer[] = [];
    const probe: Named[] = [];
    if (st.look === "last") {
      paintPasses(run.horizons[st.h], timeline(st.h), last.index + 1, layers, probe);
      return { scene: { layers, rings: [] }, probe, off: 0 };
    }
    const files = have(lr);
    if (!files) return { scene: { layers, rings: [] }, probe, off: 0 };
    if (st.look === "full") {
      const g = cumGrid(files.frames, 0, h);
      layers.push({ grid: g.grid, max: files.cumMax, ramp: levelRamp(lr.level, "dim"), alpha: 0.95 });
      probe.push({ name: `reached, level ${lr.level}`, grid: g.grid });
    }
    if (files.layers) {
      const g = cumGrid(files.layers, 0, h);
      layers.push({ grid: g.grid, max: files.marksMax, ramp: marksRamp("bright"), alpha: 1 });
      probe.push({ name: `marked, level ${lr.level}`, grid: g.grid });
    }
    return { scene: { layers, rings: st.look === "full" ? files.winRings : [] }, probe, off: 0 };
  }

  /** Grid grain: one level's panel at (phase, f). */
  function panelScene(lr: LevelRun, phase: Phase, f: number, h: number): Built {
    const layers: HeatLayer[] = [];
    const probe: Named[] = [];
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
      return { scene: { layers, rings: [] }, probe, off: 0 };
    }
    const full = st.look === "full";
    if (phase === "bwd" && (lr.refuted || !lr.mlayers_file)) {
      // No backward ran: the level's reached set, dimmed, and nothing else.
      const files = have(lr);
      if (files && full) {
        const g = cumGrid(files.frames, 0, h);
        layers.push({ grid: g.grid, max: files.cumMax, ramp: levelRamp(lr.level, "dim"), alpha: 0.95 });
      }
      return { scene: { layers, rings: [] }, probe, off: 0 };
    }
    const { rings, off } = levelAt(lr, phase, f, h, full, layers, probe);
    return { scene: { layers, rings }, probe, off };
  }

  /** Passes grain: a whole pass. A forward is everything the level
   *  reached by H (bright); a backward its whole marked set (bright warm
   *  white) over the reached set dimmed. Closed levels recede underneath. */
  function passScene(hr: HorizonRun, pass: Pass): Built {
    const layers: HeatLayer[] = [];
    const probe: Named[] = [];
    if (st.look === "last") {
      paintPasses(hr, timeline(st.h), pass.index + 1, layers, probe);
      return { scene: { layers, rings: [] }, probe, off: 0 };
    }
    const full = st.look === "full";
    const files = have(pass.lr);
    if (!files) return { scene: { layers, rings: [] }, probe, off: 0 };
    if (full && pass.lr.level !== 0) base(hr, layers, probe);
    if (full) bands(hr, pass.lr.level, layers, probe);
    const vis = cumGrid(files.frames, 0, hr.h);
    let rings: Ring[] = [];
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
        const g = cumGrid(files.layers, 0, hr.h);
        layers.push({ grid: g.grid, max: files.marksMax, ramp: marksRamp("bright"), alpha: 1 });
        probe.push({ name: `marked, level ${pass.lr.level}`, grid: g.grid });
        scale = { ramp: marksRamp("bright"), max: files.marksMax, what: "marked states per cell" };
      }
    }
    if (full) rings = files.winRings;
    return { scene: { layers, rings }, probe, off: vis.off, scale };
  }

  // ---- DOM: the stage ----------------------------------------------------------
  const aspect = run.cell_box.w / run.cell_box.h;
  const root = el("div", { class: "space", style: `--aspect:${aspect.toFixed(4)}` });
  const canvas = el("canvas", { class: "room-canvas", "aria-label": "the room: one cell per player pixel, brightness = states in the cell" });
  const overlay = el("canvas", { class: "room-overlay" });
  const caption = el("div", { class: "room-caption" });
  const probeEl = el("div", { class: "room-probe", "aria-live": "polite" });
  const scaleEl = el("div", { class: "room-scale" });
  const loadingPill = () => el("div", { class: "room-loading" }, [el("span", { class: "spinner" }), "loading level files"]);
  const roomWrap = el("div", { class: "room-wrap" }, [canvas, overlay, caption, probeEl, scaleEl, loadingPill()]);
  const roomCard = el("div", { class: "card stage", "data-loading": "false" }, [roomWrap]);
  const gridBox = el("div", { class: "grid-panels" });
  const gridCard = el("div", { class: "card stage", "data-loading": "false" }, [gridBox, loadingPill()]);
  const caption3 = el("div", { class: "room-caption" });
  const view3dTools = el("div", { class: "view3d-tools" });
  const view3dWrap = el("div", { class: "view3d-wrap" }, [caption3, view3dTools, loadingPill()]);
  const view3dCard = el("div", { class: "card stage", "data-loading": "false" }, [
    view3dWrap,
    el("p", { class: "note", text: "One finger orbits (sideways turns, up and down tilts from top-down to edge-on); two fingers pan and pinch; double-tap resets. Columns: a cell's height is its count (log), the moving set as the bright cap. Stack: one layer of cubes per pass, a forward's reached set in the level's colour, a backward's marked set in warm white." }),
  ]);
  gridCard.style.position = "relative";

  // ---- DOM: the transport (play, scrub, horizon) ------------------------------------
  const status = el("div", { class: "status", "aria-live": "off" });
  const playBtn = button(icon("play"), () => togglePlay(), "primary icon play", "Play (space)");
  const backBtn = button(icon("stepBack"), () => nudge(-1), "icon", "Back one step (←)");
  const fwdBtn = button(icon("stepFwd"), () => nudge(1), "icon", "Forward one step (→)");
  const prevBtn = button(icon("prev"), () => jumpPass(-1), "icon", "Previous pass ([)");
  const nextBtn = button(icon("next"), () => jumpPass(1), "icon", "Next pass (])");
  const transport = el("div", { class: "transport" }, [playBtn, backBtn, fwdBtn, prevBtn, nextBtn, status]);
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
  const hRow = el("div", { class: "h-row" });
  const bar = el("div", { class: "transport-bar" }, [el("div", { class: "transport-inner" }, [transport, scrub.root, hRow])]);

  const hChips = chips<number>(
    run.horizons.map((x, i) => ({
      value: i,
      label: `h${x.h}`,
      title: i === winH ? `horizon ${x.h}: the winning horizon (every level wins)` : x.refuted_at != null ? `horizon ${x.h}: refuted at level ${x.refuted_at} (no win by frame ${x.h})` : `horizon ${x.h}`,
    })),
    st.h,
    (i) => goHorizon(i),
    { label: "horizon", scroll: true },
  );
  hChips.root.querySelectorAll(".chip")[winH]?.classList.add("win");
  const toWin = button(icon("target"), () => goHorizon(winH), "icon small", "Jump to the winning horizon");
  hRow.append(hChips.root, toWin);

  function goHorizon(i: number, step = 0) {
    st.h = i;
    st.step = step;
    st.pass = byPass() ? Math.max(0, Math.min(timeline(i).length - 1, step)) : 0;
    stop();
    hChips.set(i);
    show(toWin, i !== winH);
    layout();
    onState();
  }

  // ---- DOM: the options (grain, look, speed) --------------------------------------
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
    { label: "grain" },
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
      { value: "full", label: "Full", title: "the accumulated set under the moving set, closed levels' marks as bands, wins as rings" },
      { value: "sweep", label: "Sweep", title: "just what moves: the frontier, or the states this iteration marks" },
      { value: "last", label: "Height map", title: "each cell in the colour of the finest band of levels whose set still contains it: dark for the flood, brightest for the exact route" },
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
  const optionCard = el("div", { class: "card stack" }, [grainChips.root, lookChips.root, mode3Chips.root, speedChips.root, pacingChips.root]);

  // ---- DOM: the legend --------------------------------------------------------------
  const key = (color: string, label: string, cls = "") => el("span", { class: "key" }, [el("i", { class: cls, style: color ? `background:${color}` : undefined }), label]);
  const legendFull = el("div", { class: "legend" }, [
    key(rgbCss(levelRamp(0, "bright")[55]), "moving: the frontier"),
    key(rgbCss(levelRamp(0, "dim")[40]), "accumulated: reached before"),
    key(rgbCss(marksRamp("bright")[55]), "marked now"),
    key(rgbCss(marksRamp("dim")[40]), "marked so far"),
    key(rgbCss(levelRamp(4, "band")[45]), "a closed level's marks"),
    key("", "win", "ring"),
  ]);
  const legendSweep = el("div", { class: "legend" }, [key(rgbCss(levelRamp(0, "bright")[55]), "the frontier at this frame"), key(rgbCss(marksRamp("bright")[55]), "marked at this iteration")]);
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
  const bandBlock = el("div", {}, [el("div", { class: "legend-title", text: "the finest band that still contains the cell" }), bandStrip]);
  const tilesLegend = el("div", { class: "legend" }, [key("#3a3a37", "wall"), key("#784638", "spikes"), key("#826e32", "spring"), key("#467846", "fruit")]);
  const help = el("details", { class: "help" }, [
    el("summary", { text: "How to read this" }),
    el("p", { class: "note", text: "One cell is one player pixel. Brightness is the number of states in the cell, on a log scale against the level's largest cell over the run, so a frame's brightness is comparable to the next one's." }),
    el("p", { class: "note", text: "A forward sweeps the frames out to the horizon: the frontier is what was first reached at that frame, the accumulated set is everything before it. A backward sweeps its iterations from the horizon back to frame 1: the marks are the states that can still win by the horizon, drawn by the frame they were first reached at (this tree stores no per-mark distance). Closed levels' marks recede to dark bands; the next level searches only inside them." }),
    el("p", { class: "note", text: "Height map: the levels collapsed into seven bands, each cell in the colour of the finest band whose set still contains it (forward and backward alike). The broad coarse bands are dark and desaturated; lightness and saturation climb with the band, so the exact route is the brightest thing on screen (the two thinnest bands carry a one-cell halo). Scrubbing passes paints the bands over in order." }),
    el("p", { class: "note", text: "Press or drag on the room to read a cell's counts." }),
    el("div", { class: "keys" }, [
      el("kbd", { text: "space" }), el("span", { text: "play / pause" }),
      el("kbd", { text: "← →" }), el("span", { text: "one step (shift: ten)" }),
      el("kbd", { text: "[ ]" }), el("span", { text: "previous / next pass" }),
      el("kbd", { text: "home end" }), el("span", { text: "start / end of the horizon" }),
      el("kbd", { text: "1 2 3" }), el("span", { text: "speed" }),
    ]),
  ]);
  const legendCard = el("div", { class: "card" }, [legendFull, legendSweep, levelBlock, bandBlock, el("div", { class: "legend-title", text: "the room" }), tilesLegend, help]);
  function syncLegend() {
    show(legendFull, st.look === "full" && st.grain !== "3d");
    show(legendSweep, st.look === "sweep" && st.grain !== "3d");
    show(bandBlock, st.look === "last" && st.grain !== "3d");
    show(levelBlock, st.look !== "last" || st.grain === "3d");
  }

  const mainCol = el("div", { class: "space-main" }, [roomCard, gridCard, view3dCard, bar]);
  const sideCol = el("aside", { class: "space-side" }, [optionCard, legendCard]);
  root.append(mainCol, sideCol);

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
  let panelsKey = "";
  function buildPanels() {
    const hr = run.horizons[st.h];
    const key = `${st.h}`;
    if (key === panelsKey) return;
    panelsKey = key;
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
        lr ? `${name}${lr.refuted ? " · refuted" : lr.first_win != null ? ` · win f${lr.first_win}` : ""}` : `${name} · not run at h${hr.h}`,
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
  }

  // ---- layout + render -------------------------------------------------------------
  function setGrain(g: Grain) {
    const was = byPass();
    st.grain = g;
    convertPlayhead(was);
    stop();
    layout();
    if (window.innerWidth < 960) window.scrollTo({ top: 0, behavior: "smooth" });
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
    show(roomCard, !grid && !is3d);
    show(gridCard, grid);
    show(view3dCard, is3d);
    show(lookChips.root, !is3d);
    show(mode3Chips.root, is3d);
    syncLegend();
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
    for (const c of [roomCard, gridCard, view3dCard]) c.dataset.loading = on ? "true" : "false";
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
          caption.replaceChildren(el("div", {}, [el("b", { text: "could not load this horizon's files" })]), el("div", { class: "verdict", text: e.message }));
        });
      return;
    }
    setLoading(false);
    if (tl.length === 0) return;
    // Prefetch the next horizon so Play runs into it without a pause.
    if (st.h + 1 < run.horizons.length) ensure(run.horizons[st.h + 1].levels).catch(() => {});

    if (st.grain === "3d") {
      render3d(hr, tl);
      return;
    }
    if (st.grain === "passes") {
      st.pass = Math.max(0, Math.min(tl.length - 1, st.pass));
      const pass = tl[st.pass];
      scrub.set(st.pass, tl.length - 1, `${pass.phase === "fwd" ? "forward" : "backward"} L${pass.lr.level}`);
      const built = passScene(hr, pass);
      lastProbe = built.probe;
      renderer.render(canvas, built.scene, overlay);
      showScale(built.scale);
      describePass(hr, pass, null, built.off);
      return;
    }
    const total = stepsOf(tl);
    st.step = Math.max(0, Math.min(total - 1, st.step));
    const { pass, i } = locate(tl, st.step);
    scrub.set(st.step, total - 1, `${pass.phase === "fwd" ? "f" : "← f"}${pass.frames[i]} · L${pass.lr.level}`);
    if (st.grain === "room") {
      const built = roomScene(hr, pass, i);
      lastProbe = built.probe;
      renderer.render(canvas, built.scene, overlay);
      showScale(built.scale);
      describePass(hr, pass, i, built.off);
    } else {
      // The grid walks the same timeline as the room: only the current
      // pass's level animates; levels whose passes are done sit at their
      // final state, levels not yet reached are blank.
      const f = pass.frames[i];
      for (const p of panels) {
        let built: Built = { scene: { layers: [], rings: [] }, probe: [], off: 0 };
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
        renderer.render(p.canvas, built.scene, p.overlay);
        p.root.classList.toggle("not-run", state === "not run" || state === "not yet");
        p.root.classList.toggle("animating", state === "animating");
        p.state.textContent = state === "animating" ? `${pass.phase === "fwd" ? "forward" : "backward"} f${f}` : state === "done" ? "done" : state === "not yet" ? "not yet" : "";
      }
      describePass(hr, pass, i, 0);
    }
  }

  /** The colour scale under the room: the moving set's ramp, 1 .. max. */
  function showScale(s: Scale | undefined) {
    if (!s || st.look === "last") {
      scaleEl.replaceChildren();
      return;
    }
    const stops = [0, 12, 24, 36, 48, 63].map((i) => rgbCss(s.ramp[i])).join(", ");
    scaleEl.replaceChildren(el("span", { text: "1" }), el("i", { style: `background:linear-gradient(to right, ${stops})` }), el("span", { text: fmtCompact(s.max) }), el("span", { class: "muted", text: s.what }));
  }

  function describePass(hr: HorizonRun, pass: Pass, i: number | null, off: number) {
    const lr = pass.lr;
    const phase = pass.phase === "fwd" ? "forward" : "backward";
    const tl = timeline(st.h);
    const states = lr.frame_states.slice(0, hr.h + 1).reduce((a, b) => a + b, 0);
    const verdict = lr.refuted ? `no win by f${hr.h} → refuted` : lr.first_win != null ? `first win f${lr.first_win}${lr.marked != null ? ` · ${fmtCompact(lr.marked)} marked` : ""}` : "";
    // The caption names the pass: level, phase, verdict.
    for (const c of [caption, caption3]) {
      c.replaceChildren(el("div", {}, [el("b", { text: `h${hr.h} · level ${lr.level} (${levelName(lr)}) · ${phase}` })]));
      if (verdict) c.append(el("div", { class: "verdict", text: verdict }));
    }

    // The status names the position: the frame and its counts.
    const pill = el("span", { class: "ph", style: `background:${pass.phase === "fwd" ? levelCss(lr.level) : "#fff5e1"}`, text: pass.phase === "fwd" ? "fwd" : "bwd" });
    let l1: string;
    let l2: string;
    if (i == null) {
      l1 = `L${lr.level} · whole pass`;
      l2 =
        pass.phase === "fwd"
          ? `pass ${pass.index + 1} of ${tl.length} · ${fmtCompact(states)} reached by f${hr.h}`
          : `pass ${pass.index + 1} of ${tl.length} · ${fmtCompact(lr.marked ?? 0)} marked of ${fmtCompact(states)} (${((100 * (lr.marked ?? 0)) / Math.max(1, states)).toFixed(1)}%)`;
    } else if (pass.phase === "fwd") {
      const f = pass.frames[i];
      const before = lr.frame_states.slice(0, f).reduce((a, b) => a + b, 0);
      l1 = `L${lr.level} · frame ${f} of ${hr.h}`;
      l2 = `pass ${pass.index + 1} of ${tl.length} · ${fmtCompact(lr.frame_states[f] ?? 0)} in the frontier · ${fmtCompact(before)} before${off ? ` · ${fmtCompact(off)} off-grid` : ""}`;
    } else {
      const f = pass.frames[i];
      const soFar = lr.marks_by_layer.slice(f, hr.h + 1).reduce((a, b) => a + b, 0);
      l1 = `L${lr.level} · back to f${f}`;
      l2 = `pass ${pass.index + 1} of ${tl.length} · ${fmtCompact(lr.marks_by_layer[f] ?? 0)} marked at f${f} · ${fmtCompact(soFar)} so far`;
    }
    status.replaceChildren(el("div", { class: "l1" }, [pill, l1]), el("div", { class: "l2", text: l2 }));
    if (st.grain === "grid") status.lastElementChild!.textContent = `${l2.split(" · ")[0]} · level ${lr.level} plays, earlier levels final, later ones not yet`;
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
        const g = cumGrid(files.layers, 0, h).grid;
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
      const reached = cumGrid(files.frames, 0, hr.h).grid;
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
      const g = !files ? null : p.phase === "fwd" ? cumGrid(files.frames, 0, hr.h).grid : files.layers ? cumGrid(files.layers, 0, hr.h).grid : null;
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

  // ---- probe (press / drag on the room) ----------------------------------------------
  let lastProbe: Named[] = [];
  let probeTimer = 0;
  function probeAt(ev: PointerEvent) {
    const r = canvas.getBoundingClientRect();
    const lx = Math.floor(((ev.clientX - r.left) / r.width) * run.cell_box.w);
    const ly = Math.floor(((ev.clientY - r.top) / r.height) * run.cell_box.h);
    if (lx < 0 || ly < 0 || lx >= run.cell_box.w || ly >= run.cell_box.h) return;
    const idx = lx + ly * run.cell_box.w;
    const [x, y] = renderer.cellXY(idx);
    const rows = lastProbe.filter((p) => p.grid[idx] > 0).map((p) => `${fmtInt(p.grid[idx])} ${p.name}`);
    probeEl.replaceChildren(el("div", {}, [el("b", { text: `cell (${x}, ${y})` })]), ...rows.map((t) => el("div", { text: t })));
    if (rows.length === 0) probeEl.append(el("div", { class: "muted", text: "no states" }));
  }
  canvas.addEventListener("pointerdown", (ev) => {
    canvas.setPointerCapture(ev.pointerId);
    clearTimeout(probeTimer);
    probeAt(ev);
  });
  canvas.addEventListener("pointermove", (ev) => {
    if (canvas.hasPointerCapture(ev.pointerId) || ev.pointerType === "mouse") probeAt(ev);
  });
  const hideProbe = () => {
    clearTimeout(probeTimer);
    probeTimer = window.setTimeout(() => probeEl.replaceChildren(), 1200);
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
      const atEnd = byPass() ? st.pass + n >= tl.length - 1 : st.step + n >= stepsOf(tl) - 1;
      if (atEnd) {
        // Run on into the next horizon; stop after the last one.
        if (st.h + 1 < run.horizons.length) {
          st.h += 1;
          st.step = 0;
          st.pass = 0;
          st.pos = 0;
          hChips.set(st.h);
          show(toWin, st.h !== winH);
          layout();
        } else {
          if (byPass()) st.pass = tl.length - 1;
          else st.step = stepsOf(tl) - 1;
          st.playing = false;
          syncPlay();
          render();
          onState();
        }
      } else {
        if (byPass()) st.pass += n;
        else st.step += n;
        render();
      }
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
      if (finished && st.h === run.horizons.length - 1) {
        st.step = 0;
        st.pass = 0;
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
    if (st.h !== winH) p.set("h", String(run.horizons[st.h].h));
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
    st.h = hi >= 0 ? hi : winH;
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
    hChips.set(st.h);
    show(toWin, st.h !== winH);
    grainChips.set(st.grain);
    mode3Chips.set(st.mode3);
    lookChips.set(st.look);
    speedChips.set(st.speed);
    pacingChips.set(st.pacing);
    layout();
  }

  new ResizeObserver(() => render()).observe(roomWrap);
  // Open on the answer: the winning horizon's ladder at its last step -
  // the exact level's marked route over everything that was searched.
  // Play from there starts the sweep over from the top.
  apply(new URLSearchParams());
  return { root, params, apply };
}
