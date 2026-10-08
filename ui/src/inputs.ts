// The buttons a concrete path holds, under the room: per path (ours, the
// database TAS) a UCT-style pad - the four arrows and jump / dash, lit when
// held at the frame shown - and a strip of the frames around it, one column
// per frame and one lane per button, so presses and releases read as bars.
//
// Frame convention (pico8_diff/replay.py, concrete_run): frame f is the
// f-th `_update()`, which reads `inputs[f - 1]`; the path's position at f is
// the state AFTER it. So the byte shown at f is the one that produced the
// box drawn at f, and a dash marked at f (the cart's own dash branch ran on
// frame f) has the dash bit set at f. Frame 0 consumed nothing.
//
// Frozen frames: a dash start sets the cart's `freeze=2`, and `_update`
// returns before any object runs while it counts down, so the two frames
// after each dash ignore their input entirely (the player's `p_jump` /
// `p_dash` are not updated either). They are derived from the `dashes`
// list, so the other freeze (the orb's `freeze=10`, room (5,2)) is not
// marked, and a path without dashes (older exports) marks none.
//
// A FRESH press of jump or dash (held now, not held on the last frame the
// player consumed input) is what the game acts on (`btn(k_jump) and not
// this.p_jump`); it is drawn brighter, with a ring, than a held one.

import type { Witness } from "./data";
import { rgbCss, type RGB } from "./color";
import { el, fitCanvas, show } from "./ui";

/** The cart's `freeze=2` on a dash start. */
const DASH_FREEZE = 2;

/** The buttons, in the strip's lane order (top to bottom). */
const LANES: { bit: number; glyph: string; name: string }[] = [
  { bit: 0, glyph: "←", name: "left" },
  { bit: 1, glyph: "→", name: "right" },
  { bit: 2, glyph: "↑", name: "up" },
  { bit: 3, glyph: "↓", name: "down" },
  { bit: 4, glyph: "J", name: "jump" },
  { bit: 5, glyph: "X", name: "dash" },
];
const JUMP = 4;
const DASH = 5;

/** One path's inputs, by frame. */
export class InputTrack {
  private readonly frozen = new Set<number>();
  constructor(readonly inputs: number[], dashes: [number, string][]) {
    for (const [d] of dashes) for (let k = 1; k <= DASH_FREEZE; k++) this.frozen.add(d + k);
  }
  /** The byte the game read on frame `f`, null outside the run. */
  byte(f: number): number | null {
    return f >= 1 && f <= this.inputs.length ? this.inputs[f - 1] : null;
  }
  isFrozen(f: number): boolean {
    return this.frozen.has(f);
  }
  held(f: number, bit: number): boolean {
    const b = this.byte(f);
    return b != null && ((b >> bit) & 1) === 1;
  }
  /** Held at `f`, and not on the last frame before it the player read. */
  fresh(f: number, bit: number): boolean {
    if (!this.held(f, bit) || this.isFrozen(f)) return false;
    let g = f - 1;
    while (g >= 1 && this.isFrozen(g)) g--;
    return !this.held(g, bit);
  }
}

export interface InputRow {
  w: Witness;
  /** `ours`, `TAS1`. */
  name: string;
  /** The second line: `search`, `database`. */
  sub: string;
  color: RGB;
}

/** Strip geometry (CSS px). */
const LANE_H = 7;
const LANE_GAP = 1;
/** The extra gap between the arrows and jump / dash. */
const GROUP_GAP = 4;
const COL_W = 9;
const GUTTER = 11;
const AXIS_H = 11;
/** Room above and below the lanes for the frame-shown outline. */
const PAD = 2;
const lanesH = LANES.length * LANE_H + (LANES.length - 1) * LANE_GAP + GROUP_GAP;
const laneY = (i: number) => i * (LANE_H + LANE_GAP) + (i >= JUMP ? GROUP_GAP : 0);

/** The panel: one row per path. `set(f)` shows frame `f` (null hides it). */
export function inputPanel(rows: InputRow[]): { root: HTMLElement; set(f: number | null): void } {
  let frame = 0;
  const font = () => getComputedStyle(document.body).fontFamily;
  const parts = rows.map((r, ri) => {
    const track = new InputTrack(r.w.inputs, r.w.dashes ?? []);
    const c = rgbCss(r.color);
    // The pad: up over left / down / right, jump and dash tall at the right.
    const keys = LANES.map((l) => el("span", { class: `ik k-${l.name}`, title: l.name, text: l.glyph }));
    const pad = el("div", { class: "ipad", style: `--c:${c}` }, keys);
    const axis = ri === rows.length - 1;
    const canvas = el("canvas", { class: "iroll", style: `height:${lanesH + 2 * PAD + (axis ? AXIS_H : 0)}px`, "aria-hidden": true });
    const redraw = fitCanvas(canvas, (ctx, w, h) => {
      ctx.clearRect(0, 0, w, h);
      ctx.translate(0, PAD);
      // An odd count, so the frame shown is the middle column.
      const fit = Math.max(1, Math.floor((w - GUTTER) / COL_W));
      const ncol = fit % 2 ? fit : fit - 1;
      const half = (ncol - 1) / 2;
      const x0 = w - ncol * COL_W;
      ctx.font = `700 8px ${font()}`;
      ctx.textBaseline = "middle";
      ctx.textAlign = "center";
      ctx.fillStyle = "rgba(255,255,255,0.5)";
      LANES.forEach((l, i) => ctx.fillText(l.glyph, x0 - GUTTER / 2, laneY(i) + LANE_H / 2 + 0.5));
      ctx.font = `600 9px ${font()}`;
      for (let k = 0; k < ncol; k++) {
        const g = frame - half + k;
        const x = x0 + k * COL_W;
        if (axis && g >= 0 && g % 10 === 0) {
          ctx.fillStyle = "rgba(255,255,255,0.5)";
          ctx.fillText(String(g), x + COL_W / 2, lanesH + PAD + AXIS_H / 2);
        }
        if (track.byte(g) == null) continue;
        const frozen = track.isFrozen(g);
        if (frozen) {
          // A hatch: the frame happened, the game read nothing.
          ctx.save();
          ctx.beginPath();
          ctx.rect(x, 0, COL_W - 1, lanesH);
          ctx.clip();
          ctx.strokeStyle = "rgba(255,255,255,0.16)";
          ctx.lineWidth = 1;
          for (let s = -COL_W; s < lanesH; s += 4) {
            ctx.moveTo(x, s + COL_W);
            ctx.lineTo(x + COL_W, s);
          }
          ctx.stroke();
          ctx.restore();
        }
        LANES.forEach((l, i) => {
          const y = laneY(i);
          if (!track.held(g, l.bit)) {
            if (!frozen) {
              ctx.fillStyle = "rgba(255,255,255,0.06)";
              ctx.fillRect(x, y, COL_W - 1, LANE_H);
            }
            return;
          }
          const button = l.bit === JUMP || l.bit === DASH;
          const fresh = button && track.fresh(g, l.bit);
          ctx.fillStyle = rgbCss(r.color, frozen ? 0.3 : button && !fresh ? 0.45 : 1);
          ctx.fillRect(x, y, COL_W - 1, LANE_H);
          if (fresh) {
            ctx.fillStyle = "#fff";
            ctx.fillRect(x, y, 2, LANE_H);
          }
        });
      }
      // The frame shown.
      const xc = x0 + half * COL_W;
      ctx.strokeStyle = "rgba(255,255,255,0.9)";
      ctx.lineWidth = 1;
      ctx.strokeRect(xc - 1.5, -1.5, COL_W + 2, lanesH + 3);
    });
    const sub = el("span", { text: r.sub });
    const label = el("div", { class: "iname" }, [el("b", { style: `color:${c}`, text: r.name }), sub]);
    const root = el("div", { class: "irow" }, [label, pad, canvas]);
    const set = (f: number) => {
      const frozen = track.isFrozen(f);
      pad.dataset.state = track.byte(f) == null ? "none" : frozen ? "frozen" : "live";
      sub.textContent = frozen ? "frozen" : r.sub;
      sub.classList.toggle("frozen", frozen);
      LANES.forEach((l, i) => {
        keys[i].classList.toggle("on", track.held(f, l.bit));
        keys[i].classList.toggle("fresh", track.fresh(f, l.bit) && (l.bit === JUMP || l.bit === DASH));
      });
      redraw();
    };
    return { root, set };
  });
  const root = el("div", { class: "inputs", "aria-hidden": true }, parts.map((p) => p.root));
  return {
    root,
    set(f) {
      show(root, f != null);
      if (f == null) return;
      frame = f;
      for (const p of parts) p.set(f);
    },
  };
}
