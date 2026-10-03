// The Rem tab: debug pages for the sub-pixel remainder, independent of the
// chosen run (data/debug-rem.json, room (3,3)'s reference replayed on a real
// PICO-8, plus the rem census). Three cards:
//
//  1. One frame on one axis: the frame is piecewise affine in the remainder.
//     The input range [-1/2, 1/2) is cut where `amount = flr(rem + v + 1/2)`
//     changes; on each piece the output is the input shifted by `v - amount`.
//     Drawn as the input buckets of a rung (top) mapped onto the output
//     range (bottom): a bucket whose image straddles a bucket boundary is
//     two edges, one of them often a sliver.
//  2. Many frames: an interval pushed through the reference's real speeds,
//     exactly (translated, split at the cuts) against snapped to its
//     buckets every frame (today's rungs). The snapped rows multiply; the
//     exact ones only split where a piece really lands on another pixel.
//  3. The measurements behind it (rewrite rem-census).
//
// Units: raw 16.16 (65536 = 1 px); a remainder lies in [-32768, 32767].

import { dataUrl } from "./data";
import { chips, el, fitCanvas } from "./ui";
import { inkMuted, inkSecondary, gridline, slots } from "./color";
import type { View } from "./main";

interface Frame {
  f: number;
  freeze: number;
  x: number;
  y: number;
  sx: number;
  sy: number;
  rx: number;
  ry: number;
}
interface CensusRow {
  f: number;
  n: number;
  g: number;
  buckets: number[];
  ax: number[];
  ay: number[];
  bfx: number[];
  bfy: number[];
  cum: number[];
}
interface Over {
  frames: number[];
  exact: number[];
  levels: { level: string; tree: number[]; ideal: number[] }[];
}
interface Data {
  room: string;
  frames: Frame[];
  census: CensusRow[];
  over: Over;
}

const ONE = 65536;
const HALF = 32768;

/** One frame's move on one axis: the remainder after it, and `amount`. */
function step(rem: number, v: number): { rem: number; amount: number } {
  const u = rem + v + HALF;
  const amount = Math.floor(u / ONE);
  return { rem: u - HALF - amount * ONE, amount };
}
/** Pixels the cart's `move_x`/`move_y` actually moves: the loop runs
 *  `abs(amount) + 1` times with `step = sign(amount)`. */
const pixels = (amount: number) => (amount === 0 ? 0 : amount + Math.sign(amount));

interface Piece {
  lo: number;
  hi: number;
  /** Pixels moved since the start (the discrete position, relative). */
  px: number;
}

/** [lo, hi] cut where `amount` changes: per piece its input part, its
 *  amount, and its output (the input shifted by v - amount; exact). */
function split(lo: number, hi: number, v: number): { inLo: number; inHi: number; amount: number; outLo: number; outHi: number }[] {
  const aLo = step(lo, v).amount;
  const aHi = step(hi, v).amount;
  const out = [];
  for (let a = aLo; a <= aHi; a++) {
    // rem + v + 1/2 in [a, a+1)  <=>  rem in [a*ONE - HALF - v, a*ONE + HALF - 1 - v]
    const inLo = Math.max(lo, a * ONE - HALF - v);
    const inHi = Math.min(hi, a * ONE + HALF - 1 - v);
    if (inLo > inHi) continue;
    out.push({ inLo, inHi, amount: a, outLo: inLo + v - a * ONE, outHi: inHi + v - a * ONE });
  }
  return out;
}

/** The image of a piece under one frame at speed v (exact, no widening). */
function image(p: Piece, v: number): Piece[] {
  return split(p.lo, p.hi, v).map((s) => ({ lo: s.outLo, hi: s.outHi, px: p.px + pixels(s.amount) }));
}

/** The buckets of rung k a piece touches, each widened to the whole bucket
 *  (today's boundary widening). */
function snap(p: Piece, k: number): Piece[] {
  const w = ONE >> k;
  const b0 = Math.floor((p.lo + HALF) / w);
  const b1 = Math.floor((p.hi + HALF) / w);
  const out: Piece[] = [];
  for (let b = b0; b <= b1; b++) out.push({ lo: b * w - HALF, hi: (b + 1) * w - HALF - 1, px: p.px });
  return out;
}

/** Merge pieces on the same pixel whose intervals touch (exact: the frame
 *  treats them alike). */
function mergeExact(ps: Piece[]): Piece[] {
  const byPx = new Map<number, Piece[]>();
  for (const p of ps) (byPx.get(p.px) ?? byPx.set(p.px, []).get(p.px)!).push({ ...p });
  const out: Piece[] = [];
  for (const list of byPx.values()) {
    list.sort((a, b) => a.lo - b.lo);
    let cur = list[0];
    for (const p of list.slice(1)) {
      if (p.lo <= cur.hi + 1) cur.hi = Math.max(cur.hi, p.hi);
      else {
        out.push(cur);
        cur = p;
      }
    }
    out.push(cur);
  }
  return out;
}
/** Distinct (pixel, bucket) rows. */
function dedupeSnapped(ps: Piece[]): Piece[] {
  const seen = new Map<string, Piece>();
  for (const p of ps) seen.set(`${p.px}:${p.lo}`, p);
  return [...seen.values()];
}

const fmtPx = (raw: number) => (raw / ONE).toFixed(raw % ONE === 0 ? 0 : 3);
const pxColor = (px: number) => (px === 0 ? "#f3f2ec" : slots[((px % slots.length) + slots.length) % slots.length]);

export function remView(): View {
  const root = el("div", { class: "rem stack" });
  root.append(el("div", { class: "card" }, [el("p", { class: "note", text: "Loading the remainder data…" })]));
  void (async () => {
    let data: Data;
    try {
      const r = await fetch(dataUrl("debug-rem.json"));
      if (!r.ok) throw new Error(`${r.status} ${r.statusText}`);
      data = (await r.json()) as Data;
    } catch (e) {
      root.replaceChildren(el("div", { class: "card err-card" }, [el("h2", { text: "No remainder data" }), el("pre", { text: String((e as Error).message) })]));
      return;
    }
    root.replaceChildren(oneFrameCard(data), manyFramesCard(data), measurementsCard(data));
  })();
  return { root };
}

// ---------------------------------------------------------------- card 1 --

function oneFrameCard(data: Data): HTMLElement {
  // The speeds the reference actually used, most common first.
  const counts = new Map<number, number>();
  for (const f of data.frames) for (const v of [f.sx, f.sy]) if (v !== 0) counts.set(v, (counts.get(v) ?? 0) + 1);
  // Every speed used at least twice, positive ones first (the cut is the
  // story: an integer speed has none, a fractional one does).
  const speeds = [...counts.entries()].filter(([, c]) => c >= 2).map(([v]) => v).filter((v) => v > 0).sort((a, b) => a - b).slice(0, 12);
  const frac = speeds.filter((v) => v % ONE !== 0);
  const st = { v: frac.find((v) => v > 30000) ?? frac[0] ?? speeds[0], k: 2 };
  const canvas = el("canvas", { class: "rem-canvas", style: "height: 260px" });
  const caption = el("p", { class: "note" });
  const draw = fitCanvas(canvas, (ctx, w, h) => {
    ctx.clearRect(0, 0, w, h);
    const pad = 12;
    const X = (raw: number) => pad + ((raw + HALF) / ONE) * (w - 2 * pad);
    const yIn = 34, yOut = h - 40, bar = 18;
    const kw = ONE >> st.k;
    const n = 1 << st.k;
    ctx.font = "12px Inter, system-ui, sans-serif";
    ctx.fillStyle = inkSecondary;
    ctx.fillText("input rem (one bucket each)", pad, yIn - 10);
    ctx.fillText("output rem", pad, yOut + bar + 16);
    let slivers = 0, edges = 0;
    for (let b = 0; b < n; b++) {
      const lo = b * kw - HALF, hi = (b + 1) * kw - HALF - 1;
      const col = slots[b % slots.length];
      ctx.fillStyle = col;
      ctx.globalAlpha = 0.85;
      ctx.fillRect(X(lo) + 0.5, yIn, X(hi + 1) - X(lo) - 1, bar);
      for (const sp of split(lo, hi, st.v)) {
        const p = { lo: sp.outLo, hi: sp.outHi };
        const [iL, iH] = [sp.inLo, sp.inHi];
        edges++;
        const width = p.hi - p.lo + 1;
        if (width < kw / 16) slivers++;
        ctx.globalAlpha = 0.28;
        ctx.beginPath();
        ctx.moveTo(X(iL), yIn + bar);
        ctx.lineTo(X(iH + 1), yIn + bar);
        ctx.lineTo(X(p.hi + 1), yOut);
        ctx.lineTo(X(p.lo), yOut);
        ctx.closePath();
        ctx.fill();
        ctx.globalAlpha = 0.9;
        ctx.fillRect(X(p.lo), yOut, Math.max(1.5, X(p.hi + 1) - X(p.lo)), bar);
      }
    }
    ctx.globalAlpha = 1;
    // The output bucket grid.
    ctx.strokeStyle = gridline;
    ctx.lineWidth = 1;
    for (let b = 0; b <= n; b++) {
      const x = X(b * kw - HALF);
      ctx.beginPath();
      ctx.moveTo(x, yOut - 4);
      ctx.lineTo(x, yOut + bar + 4);
      ctx.stroke();
    }
    // The cut: where amount changes, in input coordinates.
    const cut = Math.ceil((HALF - st.v) / ONE) * ONE - HALF - st.v; // rem + v + 1/2 = integer
    const cuts: number[] = [];
    for (let c = cut - 2 * ONE; c <= cut + 2 * ONE; c += ONE) if (c > -HALF && c < HALF) cuts.push(c);
    ctx.strokeStyle = "#f3f2ec";
    ctx.setLineDash([4, 3]);
    for (const c of cuts) {
      ctx.beginPath();
      ctx.moveTo(X(c), yIn - 4);
      ctx.lineTo(X(c), yIn + bar + 4);
      ctx.stroke();
    }
    ctx.setLineDash([]);
    const aLo = step(-HALF, st.v).amount, aHi = step(HALF - 1, st.v).amount;
    caption.textContent =
      `Speed ${fmtPx(st.v)} px/frame. The dashed line is the cut: left of it amount = ${aLo} ` +
      `(the player moves ${pixels(aLo)} px), right of it amount = ${aHi} (moves ${pixels(aHi)} px). ` +
      `Each piece's output is its input shifted by v − amount, so widths never change. ` +
      `At rung ${st.k} (${n} bucket${n === 1 ? "" : "s"}) the ${n} input bucket${n === 1 ? "" : "s"} make ${edges} edges; ` +
      `${slivers} of them land as a sliver (under 1/16 of a bucket) that today's widening would grow to a whole bucket.`;
  });
  const speedChips = chips(
    speeds.map((v) => ({ value: v, label: fmtPx(v) })),
    st.v,
    (v) => {
      st.v = v;
      draw();
    },
    { label: "speed", scroll: true },
  );
  const kChips = chips(
    [0, 1, 2, 3, 4].map((k) => ({ value: k, label: `rem ${k}`, title: `${1 << k} buckets` })),
    st.k,
    (k) => {
      st.k = k;
      draw();
    },
    { label: "rung" },
  );
  return el("div", { class: "card" }, [
    el("h2", { text: "One frame, one axis" }),
    el("p", {
      class: "note",
      text: "The cart moves rem += v, amount = flr(rem + ½), rem −= amount, then steps the player |amount|+1 pixels. So one frame is a few pieces: inside a piece the player does the same thing and the remainder just shifts.",
    }),
    speedChips.root,
    kChips.root,
    canvas,
    caption,
  ]);
}

// ---------------------------------------------------------------- card 2 --

function manyFramesCard(data: Data): HTMLElement {
  const frames = data.frames;
  const starts = [51, 80, 113].filter((f) => frames.some((x) => x.f === f));
  const st = { axis: "x" as "x" | "y", k: 4, start: starts[0], width: 4 };
  const canvas = el("canvas", { class: "rem-canvas" });
  const summary = el("p", { class: "note" });
  const rowH = 7;

  interface Row {
    f: number;
    exact: Piece[];
    snapped: Piece[];
    ref: number;
    event: string;
  }
  const simulate = (): Row[] => {
    const i0 = frames.findIndex((x) => x.f === st.start);
    const ref0 = st.axis === "x" ? frames[i0].rx : frames[i0].ry;
    // The start: the bucket of rung `width` holding the reference's remainder.
    const w0 = ONE >> st.width;
    const b0 = Math.floor((ref0 + HALF) / w0);
    const start: Piece = { lo: b0 * w0 - HALF, hi: (b0 + 1) * w0 - HALF - 1, px: 0 };
    let exact: Piece[] = [start];
    let snapped: Piece[] = snap(start, st.k);
    const rows: Row[] = [{ f: frames[i0].f, exact, snapped, ref: ref0, event: "start" }];
    for (let i = i0 + 1; i < frames.length && rows.length < 70; i++) {
      const prev = frames[i - 1], cur = frames[i];
      if (cur.f !== prev.f + 1) break;
      const v = st.axis === "x" ? prev.sx : prev.sy;
      const refPrev = st.axis === "x" ? prev.rx : prev.ry;
      const ref = st.axis === "x" ? cur.rx : cur.ry;
      let event = "";
      if (prev.freeze > 0) {
        event = "freeze";
      } else if (step(refPrev, v).rem !== ref) {
        // The reference collided: rem reset. Approximation: every piece
        // resets with it (a piece on another pixel might not collide).
        exact = [{ lo: ref, hi: ref, px: 0 }];
        snapped = snap({ lo: ref, hi: ref, px: 0 }, st.k);
        rows.push({ f: cur.f, exact, snapped, ref, event: "collision: rem := 0" });
        continue;
      } else {
        exact = mergeExact(exact.flatMap((p) => image(p, v)));
        snapped = dedupeSnapped(snapped.flatMap((p) => image(p, v)).flatMap((p) => snap(p, st.k)));
      }
      rows.push({ f: cur.f, exact, snapped, ref, event });
    }
    return rows;
  };

  const draw = fitCanvas(canvas, (ctx, w, h) => {
    const rows = simulate();
    ctx.clearRect(0, 0, w, h);
    const left = 34, right = 58;
    const X = (raw: number) => left + ((raw + HALF) / ONE) * (w - left - right);
    ctx.font = "10px Inter, system-ui, sans-serif";
    // the bucket grid of rung k (faint)
    const n = 1 << st.k;
    ctx.strokeStyle = gridline;
    for (let b = 0; b <= n; b++) {
      const x = X(b * (ONE >> st.k) - HALF);
      ctx.beginPath();
      ctx.moveTo(x, 0);
      ctx.lineTo(x, 12 + rows.length * rowH);
      ctx.stroke();
    }
    ctx.fillStyle = inkMuted;
    ctx.fillText("−½", X(-HALF), 9);
    ctx.fillText("rem", X(0) - 8, 9);
    ctx.fillText("½", X(HALF) - 8, 9);
    let maxSnap = 0, maxExact = 0;
    rows.forEach((r, j) => {
      const y = 12 + j * rowH;
      // snapped rows: translucent, by pixel
      for (const p of r.snapped) {
        ctx.fillStyle = pxColor(p.px);
        ctx.globalAlpha = 0.22;
        ctx.fillRect(X(p.lo), y, Math.max(1, X(p.hi + 1) - X(p.lo)), rowH - 1);
      }
      // exact pieces: solid
      ctx.globalAlpha = 1;
      for (const p of r.exact) {
        ctx.fillStyle = pxColor(p.px);
        ctx.fillRect(X(p.lo), y + 1, Math.max(1.5, X(p.hi + 1) - X(p.lo)), rowH - 3);
      }
      // the reference's own remainder
      ctx.fillStyle = "#ffcf5c";
      ctx.fillRect(X(r.ref) - 1, y, 2, rowH - 1);
      ctx.fillStyle = inkMuted;
      if (j % 5 === 0) ctx.fillText(`f${r.f}`, 2, y + rowH);
      ctx.fillStyle = r.event.startsWith("collision") ? "#e66767" : inkSecondary;
      ctx.fillText(`${r.exact.length}/${r.snapped.length}`, w - right + 4, y + rowH);
      maxSnap = Math.max(maxSnap, r.snapped.length);
      maxExact = Math.max(maxExact, r.exact.length);
    });
    ctx.globalAlpha = 1;
    const last = rows[rows.length - 1];
    summary.textContent =
      `${rows.length} frames from f${rows[0].f} on ${st.axis}, starting from the rem-${st.width} bucket holding the reference's remainder (yellow tick). ` +
      `Solid: the exact pieces (translated, split at the cuts, merged where they touch on the same pixel). ` +
      `Faint: the same interval snapped to rem-${st.k} buckets every frame, as the rungs do today. ` +
      `Colour = how many pixels a piece has moved relative to the start piece (white = the same). ` +
      `Right column: exact/snapped rows; at most ${maxExact} exact against ${maxSnap} snapped, ending at ${last.exact.length}/${last.snapped.length}. ` +
      `Red: the reference collided and every piece is reset (an approximation).`;
    canvas.style.height = `${12 + rows.length * rowH}px`;
  });
  const axisChips = chips(
    [
      { value: "x" as const, label: "x" },
      { value: "y" as const, label: "y" },
    ],
    st.axis,
    (a) => {
      st.axis = a;
      draw();
    },
    { label: "axis" },
  );
  const kChips = chips(
    [2, 4, 6, 8].map((k) => ({ value: k, label: `rem ${k}` })),
    st.k,
    (k) => {
      st.k = k;
      draw();
    },
    { label: "snap to" },
  );
  const widthChips = chips(
    [0, 2, 4, 6].map((k) => ({ value: k, label: k === 0 ? "1 px" : `1/${1 << k}` })),
    st.width,
    (k) => {
      st.width = k;
      draw();
    },
    { label: "start width" },
  );
  const startChips = chips(
    starts.map((f) => ({ value: f, label: `f${f}` })),
    st.start,
    (f) => {
      st.start = f;
      draw();
    },
    { label: "from" },
  );
  return el("div", { class: "card" }, [
    el("h2", { text: "Many frames: exact vs snapped" }),
    el("p", {
      class: "note",
      text: "Room (3,3)'s reference speeds (real PICO-8 replay), applied to an interval of remainders. Each line is one frame; the strip is the remainder range −½ … ½.",
    }),
    axisChips.root,
    kChips.root,
    widthChips.root,
    startChips.root,
    canvas,
    summary,
  ]);
}

// ---------------------------------------------------------------- card 3 --

function measurementsCard(data: Data): HTMLElement {
  const fmt = (n: number) => (n >= 1e6 ? `${(n / 1e6).toFixed(2)}M` : n >= 1e3 ? `${(n / 1e3).toFixed(1)}k` : String(n));
  const o = data.over;
  const overTable = el("table", { class: "rem-table" }, [
    el("thead", {}, [el("tr", {}, [el("th", { text: "visited by" }), ...o.frames.map((f) => el("th", { text: `f${f}` }))])]),
    el("tbody", {}, [
      ...o.levels.map((l) =>
        el("tr", {}, [
          el("th", { text: l.level }),
          ...l.tree.map((t, i) => el("td", {}, [fmt(t), el("small", { text: ` ${(t / l.ideal[i]).toFixed(1)}× ideal` })])),
        ]),
      ),
      el("tr", {}, [el("th", { text: "ideal r8" }), ...o.levels[2].ideal.map((v) => el("td", { text: fmt(v) }))]),
      el("tr", {}, [el("th", { text: "exact game" }), ...o.exact.map((v) => el("td", { text: fmt(v) }))]),
    ]),
  ]);
  const last = data.census[data.census.length - 1];
  const census = el("table", { class: "rem-table" }, [
    el("thead", {}, [el("tr", {}, [el("th", { text: "frame" }), el("th", { text: "exact" }), el("th", { text: "A rows" }), el("th", { text: "r4" }), el("th", { text: "r8" }), el("th", { text: "A x/y px" }), el("th", { text: "B x/y @r6" })])]),
    el(
      "tbody",
      {},
      data.census
        .filter((r) => r.f % 2 === 0)
        .map((r) =>
          el("tr", {}, [
            el("th", { text: `f${r.f}` }),
            el("td", { text: fmt(r.n) }),
            el("td", { text: fmt(r.g) }),
            el("td", { text: fmt(r.buckets[4]) }),
            el("td", { text: fmt(r.buckets[8]) }),
            el("td", { text: `${r.ax[0].toFixed(2)}/${r.ay[0].toFixed(2)}` }),
            el("td", { text: `${(r.bfx[2] * 100).toFixed(1)}%/${(r.bfy[2] * 100).toFixed(1)}%` }),
          ]),
        ),
    ),
  ]);
  return el("div", { class: "card" }, [
    el("h2", { text: "Measurements" }),
    el("p", {
      class: "note",
      text: "Room (3,3), the first frames after spawn. An unfiltered forward at each rung against its IDEAL: the exact game's states projected through the same rung (what a rung that never widened past the truth would visit).",
    }),
    el("div", { class: "rem-scroll" }, [overTable]),
    el("p", {
      class: "note",
      text: `At rem 6 and 8 today's rungs visit more states than the exact game itself. Below: per frame of the exact tree, its states, A's rows (one rem rectangle per everything-but-rem combination) and the rung counts; A's median rectangle width in px, and B's mean bucket fill (the hull of the true remainders in a rem-6 bucket, as a share of the bucket). By f${last.f}: A x ${last.ax[0].toFixed(2)} px wide, B ${(last.bfx[2] * 100).toFixed(1)}% of a bucket.`,
    }),
    el("div", { class: "rem-scroll" }, [census]),
  ]);
}
