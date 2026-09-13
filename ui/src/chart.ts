// A line chart on canvas: hairline grid, 2px lines, a crosshair that
// snaps to the nearest x and a readout under the plot listing every series
// (values lead, names follow). Touch: press and drag along the x axis.

import { fitCanvas, el } from "./ui";
import { gridline, baseline, inkMuted, inkSecondary } from "./color";
import { fmtCompact } from "./data";

export interface Series {
  name: string;
  color: string;
  /** Sorted by x. */
  points: [number, number][];
  width?: number;
}

export interface LineChartOpts {
  series: Series[];
  yLog?: boolean;
  xLabel?: string;
  yLabel?: string;
  xFormat?: (x: number) => string;
  yFormat?: (y: number) => string;
  /** Vertical marker lines (e.g. the horizon). */
  markers?: { x: number; label: string }[];
  tall?: boolean;
}

function niceTicks(lo: number, hi: number, n = 5): number[] {
  if (!(hi > lo)) return [lo];
  const raw = (hi - lo) / n;
  const mag = Math.pow(10, Math.floor(Math.log10(raw)));
  const norm = raw / mag;
  const step = (norm < 1.5 ? 1 : norm < 3.5 ? 2 : norm < 7.5 ? 5 : 10) * mag;
  const out: number[] = [];
  for (let v = Math.ceil(lo / step) * step; v <= hi + 1e-9; v += step) out.push(v);
  return out;
}

interface ReadoutRow {
  name: string;
  value: string;
  color?: string;
}

function readout(): { root: HTMLElement; show(rows: ReadoutRow[], title: string): void; hide(): void } {
  const root = el("div", { class: "readout" });
  const title = el("div", { class: "readout-title", text: "press the plot to read values" });
  const list = el("div", { class: "readout-rows" });
  root.append(title, list);
  return {
    root,
    show(rows, t) {
      title.textContent = t;
      list.replaceChildren(
        ...rows.map((r) =>
          el("span", { class: "readout-row" }, [
            r.color ? el("i", { style: `background:${r.color}` }) : null,
            el("b", { text: r.value }),
            el("span", { text: r.name }),
          ]),
        ),
      );
    },
    hide() {
      title.textContent = "press the plot to read values";
      list.replaceChildren();
    },
  };
}

export function lineChart(opts: LineChartOpts): HTMLElement {
  const canvas = el("canvas");
  const wrap = el("div", { class: `chart${opts.tall ? " tall" : ""}` }, [canvas]);
  // The readout sits UNDER the plot (a floating tooltip covers most of a
  // phone-width chart); it lists every series at the crosshair's x.
  const tt = readout();
  wrap.append(tt.root);
  const yLog = !!opts.yLog;
  const yF = opts.yFormat ?? ((y: number) => fmtCompact(Math.round(y)));
  const xF = opts.xFormat ?? ((x: number) => String(x));

  let xmin = Infinity;
  let xmax = -Infinity;
  let ymin = Infinity;
  let ymax = -Infinity;
  for (const s of opts.series) {
    for (const [x, y] of s.points) {
      if (yLog && y <= 0) continue;
      xmin = Math.min(xmin, x);
      xmax = Math.max(xmax, x);
      ymin = Math.min(ymin, y);
      ymax = Math.max(ymax, y);
    }
  }
  if (!isFinite(xmin)) {
    xmin = 0;
    xmax = 1;
    ymin = 0;
    ymax = 1;
  }
  if (yLog) {
    ymin = Math.pow(10, Math.floor(Math.log10(Math.max(1, ymin))));
    ymax = Math.pow(10, Math.ceil(Math.log10(Math.max(10, ymax))));
  } else {
    ymin = 0;
    ymax = ymax <= 0 ? 1 : ymax * 1.05;
  }
  const m = { l: 44, r: 12, t: 10, b: 26 };
  let hoverX: number | null = null;
  let size = { w: 0, h: 0 };

  const sx = (x: number) => m.l + ((x - xmin) / Math.max(1e-9, xmax - xmin)) * (size.w - m.l - m.r);
  const sy = (y: number) => {
    const t = yLog ? (Math.log10(Math.max(ymin, y)) - Math.log10(ymin)) / (Math.log10(ymax) - Math.log10(ymin)) : (y - ymin) / (ymax - ymin);
    return size.h - m.b - t * (size.h - m.t - m.b);
  };

  const draw = (ctx: CanvasRenderingContext2D, w: number, h: number) => {
    size = { w, h };
    ctx.clearRect(0, 0, w, h);
    ctx.font = "11px system-ui, sans-serif";
    // Grid + y ticks.
    const yt = yLog ? Array.from({ length: Math.log10(ymax) - Math.log10(ymin) + 1 }, (_, i) => ymin * Math.pow(10, i)) : niceTicks(ymin, ymax, 4);
    ctx.strokeStyle = gridline;
    ctx.lineWidth = 1;
    ctx.fillStyle = inkMuted;
    ctx.textAlign = "right";
    ctx.textBaseline = "middle";
    for (const y of yt) {
      const py = Math.round(sy(y)) + 0.5;
      ctx.beginPath();
      ctx.moveTo(m.l, py);
      ctx.lineTo(w - m.r, py);
      ctx.stroke();
      ctx.fillText(yF(y), m.l - 5, py);
    }
    // x ticks.
    ctx.textAlign = "center";
    ctx.textBaseline = "top";
    for (const x of niceTicks(xmin, xmax, Math.max(2, Math.floor((w - m.l - m.r) / 60)))) {
      const px = Math.round(sx(x)) + 0.5;
      ctx.fillText(xF(x), px, h - m.b + 6);
    }
    ctx.strokeStyle = baseline;
    ctx.beginPath();
    ctx.moveTo(m.l, Math.round(sy(ymin)) + 0.5);
    ctx.lineTo(w - m.r, Math.round(sy(ymin)) + 0.5);
    ctx.stroke();
    if (opts.xLabel) {
      ctx.fillStyle = inkMuted;
      ctx.textAlign = "right";
      ctx.fillText(opts.xLabel, w - m.r, h - 12);
    }
    if (opts.yLabel) {
      ctx.textAlign = "left";
      ctx.textBaseline = "top";
      ctx.fillStyle = inkSecondary;
      ctx.fillText(opts.yLabel, m.l + 4, m.t - 6);
    }
    // Markers.
    for (const mk of opts.markers ?? []) {
      const px = Math.round(sx(mk.x)) + 0.5;
      ctx.strokeStyle = "rgba(255,255,255,0.35)";
      ctx.beginPath();
      ctx.moveTo(px, m.t);
      ctx.lineTo(px, h - m.b);
      ctx.stroke();
      ctx.fillStyle = inkMuted;
      ctx.textAlign = "left";
      ctx.textBaseline = "top";
      ctx.fillText(mk.label, px + 3, m.t);
    }
    // Lines.
    ctx.lineJoin = "round";
    ctx.lineCap = "round";
    for (const s of opts.series) {
      ctx.strokeStyle = s.color;
      ctx.lineWidth = s.width ?? 2;
      ctx.beginPath();
      let pen = false;
      for (const [x, y] of s.points) {
        if (yLog && y <= 0) {
          pen = false;
          continue;
        }
        const px = sx(x);
        const py = sy(y);
        if (!pen) {
          ctx.moveTo(px, py);
          pen = true;
        } else ctx.lineTo(px, py);
      }
      ctx.stroke();
    }
    // Crosshair.
    if (hoverX != null) {
      const px = Math.round(sx(hoverX)) + 0.5;
      ctx.strokeStyle = "rgba(255,255,255,0.5)";
      ctx.lineWidth = 1;
      ctx.beginPath();
      ctx.moveTo(px, m.t);
      ctx.lineTo(px, h - m.b);
      ctx.stroke();
      for (const s of opts.series) {
        const p = s.points.find((q) => q[0] === hoverX);
        if (!p || (yLog && p[1] <= 0)) continue;
        ctx.beginPath();
        ctx.arc(sx(p[0]), sy(p[1]), 4, 0, Math.PI * 2);
        ctx.fillStyle = s.color;
        ctx.fill();
        ctx.strokeStyle = "#1a1a19";
        ctx.lineWidth = 2;
        ctx.stroke();
      }
    }
  };
  const redraw = fitCanvas(canvas, draw);

  const xs = Array.from(new Set(opts.series.flatMap((s) => s.points.map((p) => p[0])))).sort((a, b) => a - b);
  const snap = (clientX: number): number | null => {
    if (xs.length === 0) return null;
    const r = canvas.getBoundingClientRect();
    const x = xmin + ((clientX - r.left - m.l) / Math.max(1, r.width - m.l - m.r)) * (xmax - xmin);
    let best = xs[0];
    for (const v of xs) if (Math.abs(v - x) < Math.abs(best - x)) best = v;
    return best;
  };
  const showAt = (clientX: number) => {
    const x = snap(clientX);
    hoverX = x;
    redraw();
    if (x == null) return;
    const at = opts.series
      .map((s) => ({ s, p: s.points.find((q) => q[0] === x) }))
      .filter((e) => e.p && !(yLog && e.p[1] <= 0))
      .sort((a, b) => b.p![1] - a.p![1]);
    tt.show(
      at.map((e) => ({ name: e.s.name, value: yF(e.p![1]), color: e.s.color })),
      `${opts.xLabel ?? "x"} ${xF(x)}`,
    );
  };
  canvas.addEventListener("pointerdown", (ev) => {
    canvas.setPointerCapture(ev.pointerId);
    showAt(ev.clientX);
  });
  canvas.addEventListener("pointermove", (ev) => {
    if (ev.pointerType === "mouse" || canvas.hasPointerCapture(ev.pointerId)) showAt(ev.clientX);
  });
  const leave = () => {
    hoverX = null;
    tt.hide();
    redraw();
  };
  canvas.addEventListener("pointerleave", (ev) => {
    if (ev.pointerType === "mouse") leave();
  });
  return wrap;
}

/** Legend keys for a set of series. */
export function legendFor(series: { name: string; color: string }[]): HTMLElement {
  return el(
    "div",
    { class: "legend" },
    series.map((s) => el("span", { class: "key" }, [el("i", { style: `background:${s.color};height:3px;border-radius:2px` }), s.name])),
  );
}
