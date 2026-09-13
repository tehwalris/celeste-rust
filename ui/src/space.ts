// The search as space. Four axes, each bound to one control (README):
//
//   horizon  - the win frame H everything on screen is at (default: the
//              winning one); the h-strip in the bottom bar, `→ win`.
//   time     - the position in H's ladder: the passes in the order they
//              ran (L0 forward, L0 backward, L1 forward, ...), a forward
//              sweeping frames 0..H and a backward sweeping iterations
//              H-1..1; the scrubber, ‹ ›, Play.
//   grain    - Room (the current pass, big) / Grid (every level's panel at
//              the same phase and time) / Passes (a step is a whole pass).
//   look     - Full (accumulated under moving, closed levels' bands, wins)
//              / Sweep only (just what moves).
//
// The renderer composites per-cell count grids over the room's tiles
// (room.ts); this file decides which grids, in which ramp, at which alpha.

import type { FramesBin, HorizonRun, LevelRun, Run } from "./data";
import { fmtCompact, fmtInt, levelName, loadFrames, loadLayers } from "./data";
import { levelCss, levelRamp, marksRamp, heightBand, bandColor, bandHalo, movingColor, HEIGHT_BANDS, winColor, rgbCss, levelColor, LEVELS } from "./color";
import { addSparse, RoomRenderer, sparseMax, type HeatLayer, type Ring, type Scene } from "./room";
import { button, chips, clear, el, scrubber } from "./ui";

type Phase = "fwd" | "bwd";
type Grain = "room" | "grid" | "passes";
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
interface Built {
  scene: Scene;
  probe: Named[];
  off: number;
}

const SPEEDS: { label: string; steps: number; passes: number }[] = [
  { label: "slow", steps: 15, passes: 1 },
  { label: "normal", steps: 60, passes: 2 },
  { label: "fast", steps: 250, passes: 4 },
];

export function spaceView(run: Run): HTMLElement {
  const renderer = new RoomRenderer(run.cell_box, run.tiles);
  const ncell = run.cell_box.w * run.cell_box.h;
  /** The winning horizon: the last one (the ladder confirmed it). */
  const winH = run.horizons.length - 1;

  const st = {
    h: winH,
    step: 0,
    pass: 0,
    grain: "room" as Grain,
    look: "full" as Look,
    speed: 1,
    pacing: "uniform" as Pacing,
    /** Real pacing: the playhead in uniform-step units (a float). */
    pos: 0,
    playing: false,
  };

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
        const [frames, layers] = await Promise.all([loadFrames(lr.frames_file), lr.mlayers_file ? loadLayers(lr.mlayers_file) : null]);
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
      probe.push({ name: `marked L${l.level}`, grid: g.grid });
    });
  }

  /** The base of the Full look: level 0's whole reachable space, dim. */
  function base(hr: HorizonRun, layers: HeatLayer[], probe: Named[]) {
    const l0 = hr.levels.find((l) => l.level === 0);
    const f0 = l0 && have(l0);
    if (!l0 || !f0) return;
    const g = cumGrid(f0.frames, 0, hr.h);
    layers.push({ grid: g.grid, max: f0.cumMax, ramp: levelRamp(0, "dim"), alpha: 0.95 });
    probe.push({ name: "visited L0", grid: g.grid });
  }

  /** One level at (phase, f): the moving set bright over its own
   *  accumulated set (Full), or the moving set alone (Sweep). */
  function levelAt(lr: LevelRun, phase: Phase, f: number, h: number, full: boolean, layers: HeatLayer[], probe: Named[]): { rings: Ring[]; off: number } {
    const files = have(lr);
    if (!files) return { rings: [], off: 0 };
    let off = 0;
    let rings: Ring[] = [];
    if (phase === "fwd") {
      if (full) {
        const g = cumGrid(files.frames, 0, f - 1);
        layers.push({ grid: g.grid, max: files.cumMax, ramp: levelRamp(lr.level, "dim"), alpha: 0.95 });
        probe.push({ name: `visited L${lr.level}`, grid: g.grid });
      }
      const g = new Float32Array(ncell);
      off += addSparse(g, files.frames.cells(f));
      layers.push({ grid: g, max: files.frameMax, ramp: levelRamp(lr.level, "bright"), alpha: 1 });
      probe.push({ name: `frontier f${f}`, grid: g });
      if (full) rings = ringsUpTo(files, f);
    } else {
      if (full) {
        const g = cumGrid(files.frames, 0, h);
        layers.push({ grid: g.grid, max: files.cumMax, ramp: levelRamp(lr.level, "dim"), alpha: 0.95 });
        probe.push({ name: `visited L${lr.level}`, grid: g.grid });
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
      }
      if (full) rings = files.winRings;
    }
    return { rings, off };
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
      probe.push({ name: `${p.phase === "fwd" ? "reached" : "marked"} L${p.lr.level}`, grid: g.grid });
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
      probe.push({ name: `reached L${lr.level}`, grid: g.grid });
      const m = new Float32Array(ncell);
      const off = addSparse(m, files.frames.cells(f));
      layers.push({ grid: m, max: 1, ramp: [movingColor], alpha: 1, flat: true });
      probe.push({ name: `frontier f${f}`, grid: m });
      return off;
    }
    if (!files.layers) return 0;
    const g = cumGrid(files.layers, f + 1, h);
    paintBand(g.grid, band, layers);
    probe.push({ name: `marked L${lr.level}`, grid: g.grid });
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
    const { rings, off } = levelAt(pass.lr, pass.phase, pass.frames[i], hr.h, full, layers, probe);
    return { scene: { layers, rings }, probe, off };
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
    if (pass.phase === "fwd") {
      layers.push({ grid: vis.grid, max: files.cumMax, ramp: levelRamp(pass.lr.level, "bright"), alpha: 1 });
      probe.push({ name: `visited L${pass.lr.level}`, grid: vis.grid });
    } else {
      if (full) {
        layers.push({ grid: vis.grid, max: files.cumMax, ramp: levelRamp(pass.lr.level, "dim"), alpha: 0.95 });
        probe.push({ name: `visited L${pass.lr.level}`, grid: vis.grid });
      }
      if (files.layers) {
        const g = cumGrid(files.layers, 0, hr.h);
        layers.push({ grid: g.grid, max: files.marksMax, ramp: marksRamp("bright"), alpha: 1 });
        probe.push({ name: `marked L${pass.lr.level}`, grid: g.grid });
      }
    }
    if (full) rings = files.winRings;
    return { scene: { layers, rings }, probe, off: vis.off };
  }

  // ---- DOM: the stage ----------------------------------------------------------
  const root = el("div", { class: "space" });
  const canvas = el("canvas", { class: "room-canvas", "aria-label": "the room" });
  const overlay = el("canvas", { class: "room-overlay" });
  const caption = el("div", { class: "room-caption" });
  const probeEl = el("div", { class: "room-probe" });
  const roomWrap = el("div", { class: "room-wrap" }, [canvas, overlay, caption, probeEl]);
  const roomCard = el("div", { class: "card stage" }, [roomWrap]);
  const gridBox = el("div", { class: "grid-panels" });
  const gridCard = el("div", { class: "card stage" }, [gridBox]);

  // ---- DOM: the bottom bar (horizon, time) ---------------------------------------
  const status = el("div", { class: "status" });
  const playBtn = button("Play", () => togglePlay(), "primary");
  const prevBtn = button("‹", () => jumpPass(-1), "small");
  const nextBtn = button("›", () => jumpPass(1), "small");
  const transport = el("div", { class: "transport" }, [playBtn, prevBtn, nextBtn, status]);
  const scrub = scrubber(
    (v) => {
      if (st.grain === "passes") st.pass = v;
      else st.step = v;
      stop();
      render();
    },
    (g) => {
      if (g) stop();
    },
  );
  const passLabel = el("div", { class: "note pass-label" });
  const hStrip = el("div", { class: "h-strip" });
  const bar = el("div", { class: "bottom-bar" }, [el("div", { class: "bottom-bar-inner" }, [transport, scrub.root, passLabel, hStrip])]);

  function buildHStrip() {
    clear(hStrip);
    hStrip.append(
      chips<number>(
        run.horizons.map((x, i) => ({ value: i, label: `h${x.h}${i === winH ? " ✓" : ""}`, title: i === winH ? "the winning horizon" : run.horizons[i].refuted_at != null ? `refuted at level ${run.horizons[i].refuted_at}` : undefined })),
        new Set([st.h]),
        (sel) => goHorizon([...sel][0]),
        { label: "horizon" },
      ),
    );
    if (st.h !== winH) hStrip.append(button("→ win", () => goHorizon(winH), "small"));
    // Keep the chosen horizon in view in the scrolling strip (after it
    // has a layout; scrollIntoView would also move the page).
    const on = hStrip.querySelector(".chip.on") as HTMLElement | null;
    if (on) requestAnimationFrame(() => (hStrip.scrollLeft = on.offsetLeft - hStrip.clientWidth / 2 + on.offsetWidth / 2));
  }

  function goHorizon(i: number) {
    st.h = i;
    st.step = 0;
    st.pass = 0;
    stop();
    buildHStrip();
    layout();
  }

  // ---- DOM: the options (grain, look, speed) --------------------------------------
  const grainRow = chips<Grain>(
    [
      { value: "room", label: "Room" },
      { value: "grid", label: "Grid" },
      { value: "passes", label: "Passes" },
    ],
    new Set([st.grain]),
    (sel) => setGrain([...sel][0]),
    { label: "grain" },
  );
  const lookRow = chips<Look>(
    [
      { value: "full", label: "Full", title: "accumulated under moving, closed levels' bands, wins" },
      { value: "sweep", label: "Sweep only", title: "just what moves: the frontier, or the states this iteration marks" },
      { value: "last", label: "Height map", title: "each cell in the colour of the finest band of levels whose set still contains it: dark for the flood, brightest for the exact route (forward and backward alike)" },
    ],
    new Set([st.look]),
    (sel) => {
      st.look = [...sel][0];
      render();
    },
    { label: "look" },
  );
  const speedRow = chips<number>(
    SPEEDS.map((s, i) => ({ value: i, label: s.label })),
    new Set([st.speed]),
    (sel) => {
      st.speed = [...sel][0];
    },
    { label: "speed" },
  );
  const pacingRow = chips<Pacing>(
    [
      { value: "uniform", label: "uniform", title: "every frame / iteration takes the same time" },
      { value: "real", label: "real time", title: "each step takes its share of the run's logged time; same overall duration" },
    ],
    new Set([st.pacing]),
    (sel) => {
      st.pacing = [...sel][0];
      st.pos = (st.grain === "passes" ? passCum(st.h) : stepCum(st.h))[st.grain === "passes" ? st.pass : st.step];
    },
    { label: "pacing" },
  );
  const optionCard = el("div", { class: "card" }, [grainRow, el("div", { style: "height:8px" }), lookRow, el("div", { style: "height:8px" }), el("div", { class: "chips" }, [speedRow, pacingRow])]);

  const legend = el("div", { class: "legend" }, [
    el("span", { class: "key" }, [el("i", { style: `background:${rgbCss(levelRamp(0, "bright")[55])}` }), "moving (frontier)"]),
    el("span", { class: "key" }, [el("i", { style: `background:${rgbCss(levelRamp(0, "dim")[40])}` }), "accumulated (visited)"]),
    el("span", { class: "key" }, [el("i", { style: `background:${rgbCss(marksRamp("bright")[55])}` }), "marked now"]),
    el("span", { class: "key" }, [el("i", { style: `background:${rgbCss(marksRamp("dim")[40])}` }), "marked so far"]),
    el("span", { class: "key" }, [el("i", { style: `background:${rgbCss(levelRamp(4, "band")[45])}` }), "closed level"]),
    el("span", { class: "key" }, [el("i", { class: "ring" }), "win"]),
    el("span", { class: "key" }, [el("i", { style: "background:#3a3a37" }), "wall"]),
    el("span", { class: "key" }, [el("i", { style: "background:#784638" }), "spikes"]),
  ]);
  const strip = el("div", { class: "level-strip" });
  for (let i = 0; i < LEVELS; i++) strip.append(el("i", { style: `background:${levelCss(i)}`, title: `level ${i}` }));
  const stripLabels = el("div", { class: "level-strip-labels" }, [el("span", { text: "level 0 (coarsest)" }), el("span", { text: "1 … 15 bits" }), el("span", { text: "exact" })]);
  const lastStrip = el("div", { class: "band-strip" });
  HEIGHT_BANDS.forEach((b, i) => lastStrip.append(el("span", { class: "band" }, [el("i", { style: `background:${rgbCss(bandColor(i))}` }), el("small", { text: b.label })])));
  lastStrip.append(el("span", { class: "band" }, [el("i", { style: `background:${rgbCss(movingColor)}` }), el("small", { text: "moving" })]));
  const legendCard = el("div", { class: "card" }, [
    legend,
    strip,
    stripLabels,
    el("p", { class: "note", text: "Height map: the levels collapsed into seven bands, each cell in the colour of the finest band whose set still contains it (forward and backward alike). The broad coarse bands are dark and desaturated; lightness and saturation climb with the band, so the exact route is the brightest thing on screen (the two thinnest bands carry a one-cell halo). Scrubbing passes paints the bands over in order." }),
    lastStrip,
    el("p", { class: "note", text: "One cell is one player pixel. Brightness is the number of states in the cell (log). A forward sweeps the frames to the horizon: the frontier is what was first reached at that frame, the accumulated set everything before. A backward sweeps its iterations from the horizon back to frame 1: the marks are the states that can still win by the horizon, drawn by the frame they were first reached at (this tree stores no per-mark distance). Closed levels' marks recede to dark bands; the next level searches only inside them." }),
  ]);

  root.append(roomCard, gridCard, optionCard, legendCard, bar);

  // ---- grid panels -----------------------------------------------------------------
  interface Panel {
    lr: LevelRun | null;
    level: number;
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
      const lab = el("div", { class: "lab" }, [
        el("i", { style: `background:${levelCss(level)}` }),
        lr ? `${name}${lr.refuted ? " · refuted" : lr.first_win != null ? ` · win f${lr.first_win}` : ""}` : `${name} · not run at h${hr.h}`,
      ]);
      const p = el("div", { class: `panel${lr ? "" : " not-run"}` }, [c, o, lab]);
      if (lr) {
        p.addEventListener("click", () => {
          // Tap a panel: open that level's pass at the same phase and time.
          const tl = timeline(st.h);
          const { pass, i } = locate(tl, st.step);
          const target = tl.find((x) => x.lr === lr && x.phase === pass.phase) ?? tl.find((x) => x.lr === lr);
          if (!target) return;
          const f = pass.frames[i];
          const j = target.frames.indexOf(f);
          st.step = target.start + Math.max(0, j);
          setGrain("room");
          grainRow.querySelectorAll(".chip").forEach((b, k) => b.classList.toggle("on", k === 0));
        });
      }
      gridBox.append(p);
      panels.push({ lr, level, canvas: c, overlay: o });
    }
  }

  // ---- layout + render -------------------------------------------------------------
  function setGrain(g: Grain) {
    const tl = timeline(st.h);
    if (g === "passes" && st.grain !== "passes") st.pass = locate(tl, st.step).pass.index;
    if (g !== "passes" && st.grain === "passes") st.step = tl[Math.min(st.pass, tl.length - 1)]?.start ?? 0;
    st.grain = g;
    stop();
    layout();
    window.scrollTo({ top: 0, behavior: "smooth" });
  }

  function layout() {
    const grid = st.grain === "grid";
    roomCard.hidden = grid;
    gridCard.hidden = !grid;
    if (grid) buildPanels();
    paintBackdrop();
    render();
  }

  function paintBackdrop() {
    const tl = timeline(st.h);
    const total = stepsOf(tl);
    const byPass = st.grain === "passes";
    scrub.backdrop((ctx, w, h) => {
      for (const p of tl) {
        const x0 = byPass ? (p.index / tl.length) * w : (p.start / total) * w;
        const x1 = byPass ? ((p.index + 1) / tl.length) * w : ((p.start + p.frames.length) / total) * w;
        ctx.fillStyle = rgbCss(levelColor(p.lr.level), p.phase === "fwd" ? 0.7 : 0.32);
        ctx.fillRect(x0, 0, Math.max(1, x1 - x0 - 0.5), h);
      }
    });
  }

  let renderToken = 0;
  function render() {
    const token = ++renderToken;
    // A scrub / jump moved the step under the float playhead: follow it.
    if (!st.playing) st.pos = (st.grain === "passes" ? passCum(st.h) : stepCum(st.h))[Math.max(0, st.grain === "passes" ? st.pass : st.step)] ?? 0;
    const hr = run.horizons[st.h];
    const tl = timeline(st.h);
    const need = hr.levels;
    const ready = need.every((l) => have(l));
    const stage = st.grain === "grid" ? gridBox : roomWrap;
    if (!ready) {
      stage.style.opacity = "0.55";
      ensure(need).then(() => {
        if (token === renderToken) render();
      });
      return;
    }
    stage.style.opacity = "1";
    if (tl.length === 0) return;
    // Prefetch the next horizon so Play runs into it without a pause.
    if (st.h + 1 < run.horizons.length) ensure(run.horizons[st.h + 1].levels);

    if (st.grain === "passes") {
      st.pass = Math.max(0, Math.min(tl.length - 1, st.pass));
      scrub.set(st.pass, tl.length - 1);
      const pass = tl[st.pass];
      const built = passScene(hr, pass);
      lastProbe = built.probe;
      renderer.render(canvas, built.scene, overlay);
      describePass(hr, pass, null, built.off);
      return;
    }
    const total = stepsOf(tl);
    st.step = Math.max(0, Math.min(total - 1, st.step));
    scrub.set(st.step, total - 1);
    const { pass, i } = locate(tl, st.step);
    if (st.grain === "room") {
      const built = roomScene(hr, pass, i);
      lastProbe = built.probe;
      renderer.render(canvas, built.scene, overlay);
      describePass(hr, pass, i, built.off);
    } else {
      const f = pass.frames[i];
      for (const p of panels) {
        const built = p.lr ? panelScene(p.lr, pass.phase, f, hr.h) : { scene: { layers: [], rings: [] } };
        renderer.render(p.canvas, built.scene, p.overlay);
      }
      describePass(hr, pass, i, 0);
    }
  }

  function describePass(hr: HorizonRun, pass: Pass, i: number | null, off: number) {
    const lr = pass.lr;
    const phase = pass.phase === "fwd" ? "forward" : "backward";
    const tl = timeline(st.h);
    const head = `h${hr.h} · L${lr.level} (${levelName(lr)}) · ${phase}`;
    let detail: string;
    let sub: string;
    if (i == null) {
      const states = lr.frame_states.slice(0, hr.h + 1).reduce((a, b) => a + b, 0);
      detail =
        pass.phase === "fwd"
          ? `${fmtCompact(states)} states reached by f${hr.h}${lr.refuted ? " · no win" : lr.first_win != null ? ` · first win f${lr.first_win}` : ""}`
          : `${fmtCompact(lr.marked ?? 0)} marked of ${fmtCompact(states)} (${((100 * (lr.marked ?? 0)) / Math.max(1, states)).toFixed(1)}%)`;
      sub = `pass ${pass.index + 1}/${tl.length}`;
    } else if (pass.phase === "fwd") {
      const f = pass.frames[i];
      const visited = lr.frame_states.slice(0, f).reduce((a, b) => a + b, 0);
      detail = `f${f} · ${fmtCompact(lr.frame_states[f] ?? 0)} in the frontier`;
      sub = `accumulated ${fmtCompact(visited)}${off ? ` · ${fmtCompact(off)} off-grid` : ""}`;
    } else {
      const f = pass.frames[i];
      const line = lr.bwd.find((b) => b.f === f);
      const soFar = lr.marks_by_layer.slice(f, hr.h + 1).reduce((a, b) => a + b, 0);
      detail = `f${f} · ${fmtCompact(lr.marks_by_layer[f] ?? 0)} marked, ${fmtCompact(soFar)} so far`;
      sub = line ? `${fmtInt(line.targets)} targets · ${fmtInt(line.rerun)} re-run · ${line.total_ms} ms` : "";
    }
    caption.replaceChildren(el("div", {}, [el("b", { text: head })]), el("div", { text: detail }));
    // The bar's status is the numbers; the room's caption already names the pass.
    status.replaceChildren(
      el("b", { text: st.grain === "grid" ? `h${hr.h} · ${phase}${i == null ? "" : ` f${pass.frames[i]}`}` : `pass ${pass.index + 1}/${tl.length} · ${phase}` }),
      el("br"),
      el("small", { text: st.grain === "grid" ? "every level at this phase and frame" : i == null ? detail : `${detail} · ${sub}` }),
    );
    const verdict = lr.refuted ? "no win → refuted" : lr.first_win != null ? `first win f${lr.first_win}${lr.marked != null ? `, ${fmtCompact(lr.marked)} marked` : ""}` : "";
    passLabel.textContent = `pass ${pass.index + 1}/${tl.length}: ${phase} of level ${lr.level} at h${hr.h}${verdict ? " · " + verdict : ""}`;
  }

  // ---- probe (press / drag on the room) ----------------------------------------------
  let lastProbe: Named[] = [];
  function probeAt(ev: PointerEvent) {
    const r = canvas.getBoundingClientRect();
    const lx = Math.floor(((ev.clientX - r.left) / r.width) * run.cell_box.w);
    const ly = Math.floor(((ev.clientY - r.top) / r.height) * run.cell_box.h);
    if (lx < 0 || ly < 0 || lx >= run.cell_box.w || ly >= run.cell_box.h) return;
    const idx = lx + ly * run.cell_box.w;
    const [x, y] = renderer.cellXY(idx);
    const rows = lastProbe.filter((p) => p.grid[idx] > 0).map((p) => `${p.name}: ${fmtInt(p.grid[idx])}`);
    probeEl.replaceChildren(el("div", {}, [el("b", { text: `(${x}, ${y})` })]), ...rows.map((t) => el("div", { text: t })));
    if (rows.length === 0) probeEl.append(el("div", { class: "muted", text: "no states" }));
  }
  canvas.addEventListener("pointerdown", (ev) => {
    canvas.setPointerCapture(ev.pointerId);
    probeAt(ev);
  });
  canvas.addEventListener("pointermove", (ev) => {
    if (canvas.hasPointerCapture(ev.pointerId)) probeAt(ev);
  });
  canvas.addEventListener("pointerup", () => setTimeout(() => probeEl.replaceChildren(), 1500));

  // ---- playback ----------------------------------------------------------------------
  let raf = 0;
  let last = 0;
  let acc = 0;
  function tick(t: number) {
    if (!st.playing) return;
    const sp = SPEEDS[st.speed];
    const units = last ? ((t - last) / 1000) * (st.grain === "passes" ? sp.passes : sp.steps) : 0;
    last = t;
    let n = 0;
    if (st.pacing === "uniform") {
      acc += units;
      n = Math.floor(acc);
      acc -= n;
    } else {
      // Advance the float playhead by uniform-step units; the step it
      // lands in is set by the log's weights.
      const passes = st.grain === "passes";
      const cum = passes ? passCum(st.h) : stepCum(st.h);
      const cur = passes ? st.pass : st.step;
      st.pos = Math.max(st.pos, cum[cur]) + units;
      n = Math.min(cum.length - 2, indexAt(cum, st.pos)) - cur;
    }
    if (n > 0) {
      const tl = timeline(st.h);
      const atEnd = st.grain === "passes" ? st.pass + n >= tl.length - 1 : st.step + n >= stepsOf(tl) - 1;
      if (atEnd) {
        // Run on into the next horizon; stop after the last one.
        if (st.h + 1 < run.horizons.length) {
          st.h += 1;
          st.step = 0;
          st.pass = 0;
          st.pos = 0;
          buildHStrip();
          layout();
        } else {
          if (st.grain === "passes") st.pass = tl.length - 1;
          else st.step = stepsOf(tl) - 1;
          st.playing = false;
          syncPlay();
          render();
        }
      } else {
        if (st.grain === "passes") st.pass += n;
        else st.step += n;
        render();
      }
    }
    if (st.playing) raf = requestAnimationFrame(tick);
  }
  function syncPlay() {
    playBtn.textContent = st.playing ? "Pause" : "Play";
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
      const finished = st.grain === "passes" ? st.pass >= tl.length - 1 : st.step >= stepsOf(tl) - 1;
      if (finished && st.h === run.horizons.length - 1) {
        st.step = 0;
        st.pass = 0;
      }
    }
    st.playing = !st.playing;
    syncPlay();
  }
  function jumpPass(d: number) {
    const tl = timeline(st.h);
    if (st.grain === "passes") st.pass = Math.max(0, Math.min(tl.length - 1, st.pass + d));
    else {
      const { pass } = locate(tl, st.step);
      st.step = tl[Math.max(0, Math.min(tl.length - 1, pass.index + d))].start;
    }
    stop();
    render();
  }
  document.addEventListener("keydown", (ev) => {
    if (!root.isConnected) return;
    const tag = (ev.target as HTMLElement).tagName;
    if (tag === "INPUT" || tag === "TEXTAREA") return;
    if (ev.key === " ") {
      ev.preventDefault();
      togglePlay();
    } else if (ev.key === "ArrowRight" || ev.key === "ArrowLeft") {
      const d = (ev.key === "ArrowRight" ? 1 : -1) * (ev.shiftKey ? 10 : 1);
      if (st.grain === "passes") st.pass += d;
      else st.step += d;
      stop();
      render();
    }
  });

  new ResizeObserver(() => render()).observe(roomWrap);
  // Open where there is something to see: level 0's forward at f45.
  st.step = 45;
  buildHStrip();
  layout();
  return root;
}
