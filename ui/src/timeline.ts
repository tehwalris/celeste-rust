// Where the time goes: the run as a waterfall. X is time (the log's
// per-frame and per-iteration totals laid end to end after the kernel
// prebuild; the log has no wall clock, so this is the search's own
// accounting). Rows: horizon, level, phase, then every frame / iteration
// as a column split into its parts (emit / own / checkpoint / pos-graph
// for a forward frame, parallel vs serial for a backward iteration).
// Zoom by SELECTING a span: drag across the overview strip or across the
// waterfall itself, or tap a horizon / level / phase block; "back" pops
// the previous span, "whole run" resets. Pinch and wheel still zoom.

import type { Chapter, Run } from "./data";
import { fmtInt, fmtMs, levelName } from "./data";
import { bwdSplit, fwdSplit, levelCss, phaseColor, prebuildColor, inkMuted, gridline, inkSecondary } from "./color";
import { button, el, fitCanvas } from "./ui";
import type { View } from "./main";

interface Block {
  row: number; // 0 horizon, 1 level, 2 phase, 3 step
  t0: number;
  t1: number;
  label: string;
  color: string;
  chapter?: Chapter;
  step?: number;
  hIndex?: number;
  level?: number;
  parts?: { name: string; ms: number; color: string }[];
}

export function timelineView(run: Run, chs: Chapter[]): View {
  const prebuildMs = (run.prebuild_s ?? 0) * 1000;
  const total = prebuildMs + chs.reduce((a, c) => a + c.totalMs, 0);
  const blocks: Block[] = [];
  if (prebuildMs > 0) blocks.push({ row: 2, t0: 0, t1: prebuildMs, label: "kernel prebuild", color: prebuildColor, parts: [{ name: "prebuild (17 rungs)", ms: prebuildMs, color: prebuildColor }] });

  // Horizon and level blocks span their chapters.
  const byH = new Map<number, { t0: number; t1: number }>();
  const byHL = new Map<string, { t0: number; t1: number; c: Chapter }>();
  for (const c of chs) {
    const t0 = prebuildMs + c.startMs;
    const t1 = t0 + c.totalMs;
    const h = byH.get(c.hIndex);
    if (!h) byH.set(c.hIndex, { t0, t1 });
    else h.t1 = t1;
    const k = `${c.hIndex}:${c.level}`;
    const hl = byHL.get(k);
    if (!hl) byHL.set(k, { t0, t1, c });
    else hl.t1 = t1;
    // Phase block.
    blocks.push({ row: 2, t0, t1, label: c.phase === "fwd" ? "forward" : "backward", color: phaseColor[c.phase], chapter: c });
    // Steps.
    let t = t0;
    c.frames.forEach((f, i) => {
      const ms = c.ms[i];
      const parts: Block["parts"] = [];
      if (c.phase === "fwd") {
        const l = c.levelRun.fwd[i];
        parts.push({ name: "emit (kernel)", ms: l.emit_ms, color: fwdSplit.emit }, { name: "own (filter + door + append)", ms: l.own_ms, color: fwdSplit.own }, { name: "checkpoint", ms: l.ckpt_ms, color: fwdSplit.ckpt }, { name: "pos-graph", ms: l.pos_ms, color: fwdSplit.pos });
        const rest = ms - parts.reduce((a, p) => a + p.ms, 0);
        if (rest > 0) parts.push({ name: "other", ms: rest, color: "#4b5563" });
      } else {
        const l = c.levelRun.bwd[i];
        parts.push({ name: "parallel (re-run)", ms: l.par_ms, color: bwdSplit.par });
        const rest = ms - l.par_ms;
        if (rest > 0) parts.push({ name: "serial (load, mark)", ms: rest, color: bwdSplit.load });
      }
      blocks.push({ row: 3, t0: t, t1: t + ms, label: `f${f}`, color: phaseColor[c.phase], chapter: c, step: i, parts });
      t += ms;
    });
  }
  for (const [hIndex, r] of byH) blocks.push({ row: 0, t0: r.t0, t1: r.t1, label: `h${run.horizons[hIndex].h}`, color: "#2f2f2d", hIndex });
  for (const [, r] of byHL) blocks.push({ row: 1, t0: r.t0, t1: r.t1, label: r.c.level === 16 ? "exact" : `L${r.c.level}`, color: levelCss(r.c.level, 0.85), hIndex: r.c.hIndex, level: r.c.level, chapter: r.c });

  // ---- view state ------------------------------------------------------------
  let v0 = 0;
  let v1 = total;
  let selected: Block | null = null;
  const canvas = el("canvas", { "aria-label": "timing waterfall" });
  const overview = el("canvas", { class: "wf-overview", "aria-label": "whole run; drag to select a span" });
  const backBtn = button("← back", () => popZoom(), "small");
  const allBtn = button("whole run", () => setView(0, total, true), "small");
  const crumb = el("span", { class: "wf-crumb" });
  const toolbar = el("div", { class: "wf-toolbar" }, [backBtn, allBtn, crumb]);
  const wrap = el("div", { class: "waterfall" }, [overview, toolbar, canvas]);
  const detail = el("div", { class: "wf-detail" });
  const bars = el("div", { class: "wf-bars" });
  const legend = el("div", { class: "legend" }, [
    ...Object.entries({ "emit (kernel)": fwdSplit.emit, "own": fwdSplit.own, "checkpoint": fwdSplit.ckpt, "pos-graph": fwdSplit.pos, "backward parallel": bwdSplit.par, "backward serial": bwdSplit.load }).map(([k, c]) => el("span", { class: "key" }, [el("i", { style: `background:${c}` }), k])),
  ]);
  const accounted = chs.reduce((a, c) => a + c.totalMs, 0);
  const note = el("p", { class: "note", text: `The log's per-frame and per-iteration totals account for ${fmtMs(accounted)} of the ${run.wall_s != null ? fmtMs(run.wall_s * 1000) : "?"} wall clock (plus ${fmtMs(prebuildMs)} kernel prebuild); the rest is between the lines - loading marks, building filters, writing the marked set. Drag across the strip or the waterfall to zoom to a span; tap a horizon / level / phase block to zoom to it; "back" returns.` });
  const root = el("div", { class: "card" }, [el("h2", { text: "Where the time goes" }), wrap, detail, bars, legend, note]);

  const ROWS = [
    { y: 0, h: 24, label: "horizon" },
    { y: 26, h: 24, label: "level" },
    { y: 52, h: 24, label: "phase" },
    { y: 78, h: 0, label: "frames / iterations" },
  ];
  const LEFT = 0;

  const draw = (ctx: CanvasRenderingContext2D, w: number, h: number) => {
    ctx.clearRect(0, 0, w, h);
    ctx.fillStyle = "#151514";
    ctx.fillRect(0, 0, w, h);
    ROWS[3].h = h - 78 - 18;
    const sx = (t: number) => LEFT + ((t - v0) / (v1 - v0)) * (w - LEFT);
    ctx.font = "11px system-ui, sans-serif";
    ctx.textBaseline = "middle";
    // Time ticks.
    const span = v1 - v0;
    const step = niceStep(span / Math.max(2, w / 90));
    ctx.fillStyle = inkMuted;
    ctx.strokeStyle = gridline;
    ctx.textAlign = "left";
    for (let t = Math.ceil(v0 / step) * step; t <= v1; t += step) {
      const x = Math.round(sx(t)) + 0.5;
      ctx.beginPath();
      ctx.moveTo(x, 0);
      ctx.lineTo(x, h - 18);
      ctx.stroke();
      ctx.fillText(fmtMs(t), x + 3, h - 9);
    }
    for (const b of blocks) {
      const x0 = sx(b.t0);
      const x1 = sx(b.t1);
      if (x1 < 0 || x0 > w) continue;
      const r = ROWS[b.row];
      const bw = Math.max(b.row === 3 ? 0.5 : 1, x1 - x0 - (x1 - x0 > 3 ? 1 : 0));
      if (b.row === 3 && b.parts) {
        // Stacked split, bottom-up, in the row's height.
        let y = r.y + r.h;
        const ms = b.t1 - b.t0;
        for (const p of b.parts) {
          const ph = (p.ms / ms) * r.h;
          ctx.fillStyle = p.color;
          ctx.fillRect(x0, y - ph, bw, Math.max(0.5, ph - (ph > 3 ? 1 : 0)));
          y -= ph;
        }
      } else {
        ctx.fillStyle = b.color;
        ctx.fillRect(x0, r.y, bw, r.h);
      }
      if (b === selected) {
        ctx.strokeStyle = "#fff";
        ctx.lineWidth = 2;
        ctx.strokeRect(x0 + 1, r.y + 1, Math.max(2, x1 - x0 - 2), r.h - 2);
      }
      if (b.row < 3 && x1 - x0 > 26) {
        ctx.fillStyle = b.row === 1 ? "#0d0d0d" : inkSecondary;
        ctx.textAlign = "left";
        const tx = Math.max(x0, 0) + 4;
        const label = b.label;
        if (ctx.measureText(label).width + 8 < x1 - Math.max(x0, 0)) ctx.fillText(label, tx, r.y + r.h / 2);
      }
    }
    // A span being selected.
    if (sel) {
      const a = Math.min(sel.x0, sel.x1);
      const b = Math.max(sel.x0, sel.x1);
      ctx.fillStyle = "rgba(255,255,255,0.14)";
      ctx.fillRect(a, 0, b - a, h - 18);
      ctx.strokeStyle = "rgba(255,255,255,0.7)";
      ctx.lineWidth = 1;
      ctx.strokeRect(a + 0.5, 0.5, b - a - 1, h - 19);
    }
    // Row labels, on a dark backdrop so they stay legible over the blocks.
    ctx.textAlign = "right";
    for (const r of ROWS) {
      if (r.h <= 0) continue;
      const tw = ctx.measureText(r.label).width;
      ctx.fillStyle = "rgba(13,13,13,0.75)";
      ctx.fillRect(w - tw - 10, r.y + 2, tw + 8, 14);
      ctx.fillStyle = inkMuted;
      ctx.fillText(r.label, w - 6, r.y + 9);
    }
  };
  const redrawMain = fitCanvas(canvas, draw);

  // The overview: the whole run, horizons on top and phases below, with
  // the current span as a bright window.
  const drawOverview = (ctx: CanvasRenderingContext2D, w: number, h: number) => {
    ctx.clearRect(0, 0, w, h);
    ctx.fillStyle = "#151514";
    ctx.fillRect(0, 0, w, h);
    const ox = (t: number) => (t / total) * w;
    for (const b of blocks) {
      if (b.row !== 2) continue;
      ctx.fillStyle = b.color;
      ctx.fillRect(ox(b.t0), h * 0.5, Math.max(0.5, ox(b.t1) - ox(b.t0)), h * 0.5);
    }
    ctx.font = "10px system-ui, sans-serif";
    ctx.textBaseline = "middle";
    ctx.textAlign = "left";
    for (const b of blocks) {
      if (b.row !== 0) continue;
      const x0 = ox(b.t0);
      const x1 = ox(b.t1);
      ctx.fillStyle = "#2f2f2d";
      ctx.fillRect(x0, 0, Math.max(0.5, x1 - x0 - 1), h * 0.5 - 1);
      if (x1 - x0 > 24) {
        ctx.fillStyle = inkSecondary;
        ctx.fillText(b.label, x0 + 3, h * 0.25);
      }
    }
    // The window.
    const a = ox(v0);
    const b = ox(v1);
    ctx.fillStyle = "rgba(0,0,0,0.55)";
    ctx.fillRect(0, 0, a, h);
    ctx.fillRect(b, 0, w - b, h);
    ctx.strokeStyle = "#fff";
    ctx.lineWidth = 1.5;
    ctx.strokeRect(a + 0.75, 0.75, Math.max(2, b - a - 1.5), h - 1.5);
    if (osel) {
      const p = Math.min(osel.x0, osel.x1);
      const q = Math.max(osel.x0, osel.x1);
      ctx.fillStyle = "rgba(255,255,255,0.25)";
      ctx.fillRect(p, 0, q - p, h);
    }
  };
  const redrawOverview = fitCanvas(overview, drawOverview);
  const redraw = () => {
    redrawMain();
    redrawOverview();
    crumb.textContent = v0 === 0 && v1 === total ? "whole run" : `${fmtMs(v0)} – ${fmtMs(v1)} (${fmtMs(v1 - v0)})`;
    backBtn.disabled = zoomStack.length === 0;
  };

  function niceStep(raw: number): number {
    const mag = Math.pow(10, Math.floor(Math.log10(raw)));
    const n = raw / mag;
    return (n < 1.5 ? 1 : n < 3.5 ? 2 : n < 7.5 ? 5 : 10) * mag;
  }

  // ---- interaction: select a span to zoom ------------------------------------------
  const zoomStack: [number, number][] = [];
  let sel: { x0: number; x1: number } | null = null; // on the main canvas, px
  let osel: { x0: number; x1: number } | null = null; // on the overview, px
  const clamp = (a: number, b: number): [number, number] => {
    let span = Math.max(50, Math.min(total, b - a));
    let lo = Math.max(0, Math.min(total - span, a));
    return [lo, lo + span];
  };
  function setView(a: number, b: number, push: boolean) {
    const [lo, hi] = clamp(a, b);
    if (lo === v0 && hi === v1) return;
    if (push) zoomStack.push([v0, v1]);
    v0 = lo;
    v1 = hi;
    redraw();
  }
  function popZoom() {
    const prev = zoomStack.pop();
    if (!prev) return;
    [v0, v1] = prev;
    redraw();
  }
  const xToT = (x: number) => {
    const r = canvas.getBoundingClientRect();
    return v0 + ((x - r.left - LEFT) / (r.width - LEFT)) * (v1 - v0);
  };

  // Main canvas: one finger drags out a span (zoom on release), a tap
  // picks a block (and zooms to it if it is a horizon / level / phase);
  // two fingers pinch.
  const pointers = new Map<number, { x: number; y: number }>();
  let pinch: { d: number; center: number; v0: number; v1: number } | null = null;
  let press: { x: number; y: number; moved: boolean } | null = null;
  canvas.addEventListener("pointerdown", (ev) => {
    canvas.setPointerCapture(ev.pointerId);
    pointers.set(ev.pointerId, { x: ev.clientX, y: ev.clientY });
    if (pointers.size === 1) press = { x: ev.clientX, y: ev.clientY, moved: false };
    else if (pointers.size === 2) {
      const [a, b] = [...pointers.values()];
      pinch = { d: Math.abs(a.x - b.x), center: xToT((a.x + b.x) / 2), v0, v1 };
      press = null;
      sel = null;
    }
    ev.preventDefault();
  });
  canvas.addEventListener("pointermove", (ev) => {
    if (!pointers.has(ev.pointerId)) return;
    pointers.set(ev.pointerId, { x: ev.clientX, y: ev.clientY });
    const r = canvas.getBoundingClientRect();
    if (pinch && pointers.size === 2) {
      const [a, b] = [...pointers.values()];
      const d = Math.max(10, Math.abs(a.x - b.x));
      const span = (pinch.v1 - pinch.v0) * (pinch.d / d);
      const cx = ((a.x + b.x) / 2 - r.left - LEFT) / (r.width - LEFT);
      [v0, v1] = clamp(pinch.center - cx * span, pinch.center - cx * span + span);
      redraw();
    } else if (press) {
      if (Math.abs(ev.clientX - press.x) > 6) press.moved = true;
      if (press.moved) {
        sel = { x0: press.x - r.left, x1: ev.clientX - r.left };
        redrawMain();
      }
    }
  });
  const up = (ev: PointerEvent) => {
    if (!pointers.has(ev.pointerId)) return;
    pointers.delete(ev.pointerId);
    if (canvas.hasPointerCapture(ev.pointerId)) canvas.releasePointerCapture(ev.pointerId);
    if (press && pointers.size === 0) {
      if (press.moved && sel) {
        const r = canvas.getBoundingClientRect();
        const a = xToT(Math.min(sel.x0, sel.x1) + r.left);
        const b = xToT(Math.max(sel.x0, sel.x1) + r.left);
        sel = null;
        setView(a, b, true);
      } else if (!press.moved) select(ev.clientX, ev.clientY);
    }
    sel = null;
    if (pointers.size < 2) pinch = null;
    if (pointers.size === 0) press = null;
    redraw();
  };
  canvas.addEventListener("pointerup", up);
  canvas.addEventListener("pointercancel", up);
  canvas.addEventListener(
    "wheel",
    (ev) => {
      ev.preventDefault();
      const r = canvas.getBoundingClientRect();
      const cx = (ev.clientX - r.left - LEFT) / (r.width - LEFT);
      const t = v0 + cx * (v1 - v0);
      const span = (v1 - v0) * Math.exp(ev.deltaY * 0.002);
      [v0, v1] = clamp(t - cx * span, t - cx * span + span);
      redraw();
    },
    { passive: false },
  );
  canvas.addEventListener("dblclick", () => setView(0, total, true));

  // Overview strip: drag selects a span of the whole run; a tap centres
  // the current window there.
  let opress: { x: number; moved: boolean } | null = null;
  const oxToT = (x: number) => {
    const r = overview.getBoundingClientRect();
    return Math.max(0, Math.min(total, ((x - r.left) / r.width) * total));
  };
  overview.addEventListener("pointerdown", (ev) => {
    overview.setPointerCapture(ev.pointerId);
    opress = { x: ev.clientX, moved: false };
    ev.preventDefault();
  });
  overview.addEventListener("pointermove", (ev) => {
    if (!opress) return;
    if (Math.abs(ev.clientX - opress.x) > 4) opress.moved = true;
    if (opress.moved) {
      const r = overview.getBoundingClientRect();
      osel = { x0: opress.x - r.left, x1: ev.clientX - r.left };
      redrawOverview();
    }
  });
  const oup = (ev: PointerEvent) => {
    if (!opress) return;
    if (overview.hasPointerCapture(ev.pointerId)) overview.releasePointerCapture(ev.pointerId);
    if (opress.moved) {
      const a = oxToT(Math.min(opress.x, ev.clientX));
      const b = oxToT(Math.max(opress.x, ev.clientX));
      osel = null;
      setView(a, b, true);
    } else {
      const span = v1 - v0;
      const c = oxToT(ev.clientX);
      setView(c - span / 2, c + span / 2, false);
    }
    osel = null;
    opress = null;
    redraw();
  };
  overview.addEventListener("pointerup", oup);
  overview.addEventListener("pointercancel", oup);

  function select(clientX: number, clientY: number) {
    const r = canvas.getBoundingClientRect();
    const y = clientY - r.top;
    const t = xToT(clientX);
    const row = ROWS.findIndex((rw) => y >= rw.y && y < rw.y + rw.h);
    if (row < 0) return;
    const hit = blocks.find((b) => b.row === row && t >= b.t0 && t < b.t1) ?? null;
    selected = hit;
    describe(hit);
    // A horizon / level / phase block is a span: zoom to it.
    if (hit && hit.row < 3) setView(hit.t0, hit.t1, true);
    else redraw();
  }

  function describe(b: Block | null) {
    detail.replaceChildren();
    bars.replaceChildren();
    if (!b) return;
    const ms = b.t1 - b.t0;
    const head = el("div", {});
    if (b.row === 0 && b.hIndex != null) {
      const hr = run.horizons[b.hIndex];
      head.append(el("b", { text: `horizon ${hr.h}` }), ` · ${fmtMs(ms)} · ${hr.levels.length} levels · ${hr.refuted_at != null ? `refuted at level ${hr.refuted_at}` : "confirmed"}`);
      const parts = hr.levels.map((l) => {
        const c = chs.filter((x) => x.hIndex === b.hIndex && x.level === l.level);
        return { name: `L${l.level}`, ms: c.reduce((a, x) => a + x.totalMs, 0), color: levelCss(l.level) };
      });
      showBars(parts, ms);
      detail.append(head, el("div", { text: parts.map((p) => `${p.name} ${fmtMs(p.ms)}`).join(" · ") }));
    } else if (b.row === 1 && b.chapter) {
      const lr = b.chapter.levelRun;
      const fwd = chs.find((x) => x.hIndex === b.hIndex && x.level === b.level && x.phase === "fwd");
      const bwd = chs.find((x) => x.hIndex === b.hIndex && x.level === b.level && x.phase === "bwd");
      head.append(el("b", { text: `h${b.chapter.h} level ${lr.level} (${levelName(lr)})` }), ` · ${fmtMs(ms)} · ${lr.refuted ? "no win → refuted" : `first win f${lr.first_win}, ${fmtInt(lr.marked ?? 0)} marked, ${fmtInt(lr.reruns ?? 0)} re-runs`}`);
      const parts = [
        { name: "forward", ms: fwd?.totalMs ?? 0, color: phaseColor.fwd },
        { name: "backward", ms: bwd?.totalMs ?? 0, color: phaseColor.bwd },
      ];
      showBars(parts, ms);
      detail.append(head, el("div", { text: `forward ${fwd?.frames.length ?? 0} frames ${fmtMs(fwd?.totalMs ?? 0)} · backward ${bwd?.frames.length ?? 0} iterations ${fmtMs(bwd?.totalMs ?? 0)}` }));
    } else if (b.row === 2 && b.chapter) {
      const c = b.chapter;
      head.append(el("b", { text: `h${c.h} level ${c.level} ${b.label}` }), ` · ${fmtMs(ms)} · ${c.frames.length} ${c.phase === "fwd" ? "frames" : "iterations"}`);
      const parts =
        c.phase === "fwd"
          ? [
              { name: "emit", ms: c.levelRun.fwd.reduce((a, l) => a + l.emit_ms, 0), color: fwdSplit.emit },
              { name: "own", ms: c.levelRun.fwd.reduce((a, l) => a + l.own_ms, 0), color: fwdSplit.own },
              { name: "checkpoint", ms: c.levelRun.fwd.reduce((a, l) => a + l.ckpt_ms, 0), color: fwdSplit.ckpt },
              { name: "pos-graph", ms: c.levelRun.fwd.reduce((a, l) => a + l.pos_ms, 0), color: fwdSplit.pos },
            ]
          : [
              { name: "parallel", ms: c.levelRun.bwd.reduce((a, l) => a + l.par_ms, 0), color: bwdSplit.par },
              { name: "serial", ms: c.levelRun.bwd.reduce((a, l) => a + (l.total_ms - l.par_ms), 0), color: bwdSplit.load },
            ];
      showBars(parts, ms);
      detail.append(head, el("div", { text: parts.map((p) => `${p.name} ${fmtMs(p.ms)}`).join(" · ") }));
    } else if (b.row === 2 && !b.chapter) {
      head.append(el("b", { text: b.label }), ` · ${fmtMs(ms)}`);
      detail.append(head);
    } else if (b.row === 3 && b.chapter && b.step != null) {
      const c = b.chapter;
      const f = c.frames[b.step];
      if (c.phase === "fwd") {
        const l = c.levelRun.fwd[b.step];
        head.append(el("b", { text: `h${c.h} level ${c.level} forward frame ${f}` }), ` · ${fmtMs(ms)}`);
        detail.append(
          head,
          el("div", { text: `${fmtInt(l.in_lanes)} in (${l.in_blocks} blocks) · ${fmtInt(l.raw)} raw · ${fmtInt(l.kept)} kept · visited ${fmtInt(l.visited)} · rss ${l.rss_gb.toFixed(2)} GB` }),
          el("div", { text: `emit ${l.emit_ms} ms (idle ${l.emit_idle}%) · own ${l.own_ms} ms (idle ${l.own_idle}%) · ckpt ${l.ckpt_ms} ms · pos ${l.pos_ms} ms` }),
        );
      } else {
        const l = c.levelRun.bwd[b.step];
        head.append(el("b", { text: `h${c.h} level ${c.level} backward iteration f${f}` }), ` · ${fmtMs(ms)}`);
        detail.append(
          head,
          el("div", { text: `${fmtInt(l.targets)} targets · ${fmtInt(l.cand_cells)} candidate cells · ${fmtInt(l.loaded)} loaded · ${fmtInt(l.rerun)} re-run · ${fmtInt(l.marked)} marked` }),
          el("div", { text: `load ${l.load_thread_ms} thread-ms · parallel ${l.par_ms} ms (idle ${l.par_idle}%)` }),
        );
      }
      if (b.parts) showBars(b.parts, ms);
    }
  }
  function showBars(parts: { name: string; ms: number; color: string }[], ms: number) {
    bars.replaceChildren(...parts.filter((p) => p.ms > 0).map((p) => el("i", { style: `width:${(100 * p.ms) / Math.max(1, ms)}%;background:${p.color}`, title: `${p.name}: ${fmtMs(p.ms)}` })));
  }

  // Open on the slowest horizon.
  let slowest: Block | null = null;
  for (const b of blocks) if (b.row === 0 && (!slowest || b.t1 - b.t0 > slowest.t1 - slowest.t0)) slowest = b;
  selected = slowest;
  describe(slowest);
  return { root };
}
