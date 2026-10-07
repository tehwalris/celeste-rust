// The room renderer: the search's state space is the room's pixel grid,
// so a scene is a stack of per-cell count grids over the room's tiles.
// Compositing happens at CELL resolution (one cell = one pixel of an
// offscreen canvas, the export's cell box) and is then scaled up with
// nearest-neighbour, so a state cell is a crisp square at any size.
//
// A heat layer maps count -> brightness on a log scale against a fixed
// `max` (per level over the whole run, so brightness is comparable across
// frames); its ramp carries the identity (the level's hue, or warm white
// for the marked set) and its alpha blends it over what is below.

import type { Box2, Sparse, Tiles } from "./data";
import { NO_POSITION } from "./data";
import type { RGB } from "./color";

export interface HeatLayer {
  grid: Float32Array;
  max: number;
  ramp: RGB[];
  alpha: number;
  /** Paint every non-empty cell the ramp's last colour at `alpha`,
   *  ignoring the count: a set, not a magnitude. */
  flat?: boolean;
}

export interface Scene {
  layers: HeatLayer[];
}

/** The first x that is in the NEXT room: the cart moves the player back
 *  inside at `x > 121`, so no in-room state has x >= 128. An export made
 *  before the exit-placement fix put won states at the next room's spawn,
 *  one room over (room (0,0): (136, 128) and (136, 124)); those cells are
 *  not painted. */
const NEXT_ROOM_X = 128;

const SPIKE_IDS = new Set([17, 27, 43, 59]);

/** The concrete paths' colours, apart from every heat ramp (blue through
 *  violet and magenta to orange, warm-white marks): ours cyan, the
 *  reference (a community TAS) yellow. */
export const OURS: RGB = [70, 220, 255];
export const REFERENCE: RGB = [255, 214, 50];

/** A concrete path to draw (`RoomRenderer.paths`). */
export interface DrawPath {
  path: ([number, number] | null)[];
  /** `[frame, "R" | "U" | ...]` where a dash starts. */
  dashes: [number, string][];
  color: RGB;
  /** The current frame's box filled (else a heavy outline). */
  filled: boolean;
}

export class RoomRenderer {
  readonly box: Box2;
  private readonly tiles: Tiles;
  private readonly base: Float32Array; // RGB per cell, the tile background
  private readonly off: HTMLCanvasElement;
  private readonly img: ImageData;
  private readonly acc: Float32Array;
  /** Per cell: whether a state can be there (x < NEXT_ROOM_X). */
  private readonly inRoom: Uint8Array;

  constructor(box: Box2, tiles: Tiles) {
    this.box = box;
    this.tiles = tiles;
    const n = box.w * box.h;
    this.base = new Float32Array(n * 3);
    this.acc = new Float32Array(n * 3);
    this.inRoom = new Uint8Array(n);
    for (let i = 0; i < n; i++) this.inRoom[i] = (i % box.w) + box.x0 < NEXT_ROOM_X ? 1 : 0;
    this.off = document.createElement("canvas");
    this.off.width = box.w;
    this.off.height = box.h;
    this.img = new ImageData(box.w, box.h);
    this.paintTiles();
  }

  /** Cell index of a start-room-relative pixel, or -1. */
  cellIndex(x: number, y: number): number {
    const lx = x - this.box.x0;
    const ly = y - this.box.y0;
    if (lx < 0 || ly < 0 || lx >= this.box.w || ly >= this.box.h) return -1;
    return lx + ly * this.box.w;
  }
  cellXY(idx: number): [number, number] {
    return [(idx % this.box.w) + this.box.x0, Math.floor(idx / this.box.w) + this.box.y0];
  }

  private set(lx: number, ly: number, c: RGB) {
    if (lx < 0 || ly < 0 || lx >= this.box.w || ly >= this.box.h) return;
    const i = (lx + ly * this.box.w) * 3;
    this.base[i] = c[0];
    this.base[i + 1] = c[1];
    this.base[i + 2] = c[2];
  }

  private paintTiles() {
    const { w, h, x0, y0 } = this.box;
    // Outside every room: black. Inside a room: the plane. Solid tiles: a
    // step off the surface; spikes a dim warm gray so they read as hazard
    // without competing with data.
    for (let ly = 0; ly < h; ly++) {
      for (let lx = 0; lx < w; lx++) {
        const x = lx + x0;
        const y = ly + y0;
        const inRoom = y >= 0 && y < 128 && x >= 0 && x < this.tiles.w * 8;
        const c: RGB = inRoom ? (x < 128 ? [20, 20, 19] : [14, 14, 13]) : [0, 0, 0];
        this.set(lx, ly, c);
      }
    }
    const solid: RGB = [58, 58, 55];
    const solidFar: RGB = [40, 40, 38];
    const spike: RGB = [120, 70, 60];
    const spring: RGB = [130, 110, 50];
    const fruit: RGB = [70, 120, 70];
    for (let ty = 0; ty < this.tiles.h; ty++) {
      for (let tx = 0; tx < this.tiles.w; tx++) {
        const i = tx + ty * this.tiles.w;
        const id = this.tiles.ids[i];
        const px = tx * 8 - x0;
        const py = ty * 8 - y0;
        if (this.tiles.solid[i]) {
          const c = tx < 16 ? solid : solidFar;
          for (let dy = 0; dy < 8; dy++) for (let dx = 0; dx < 8; dx++) this.set(px + dx, py + dy, c);
          // A one-cell darker seam between tiles keeps the tile grid legible.
          for (let d = 0; d < 8; d++) {
            this.set(px + d, py + 7, [c[0] - 10, c[1] - 10, c[2] - 10]);
            this.set(px + 7, py + d, [c[0] - 10, c[1] - 10, c[2] - 10]);
          }
        } else if (SPIKE_IDS.has(id)) {
          // Up-spikes (17): two triangles per tile. The others are rare.
          for (let dy = 0; dy < 8; dy++) {
            const half = dy < 4 ? dy : 7 - dy;
            for (let dx = 0; dx < 8; dx++) {
              const m = dx % 4;
              const tri = m === 1 || m === 2 ? 3 : m === 0 || m === 3 ? 1 : 0;
              if (id === 17 ? 7 - dy < tri * 2 + 1 : id === 27 ? dy < tri * 2 + 1 : half >= 1) {
                this.set(px + dx, py + dy, spike);
              }
            }
          }
        } else if (id === 18) {
          for (let dx = 1; dx < 7; dx++) for (let dy = 5; dy < 8; dy++) this.set(px + dx, py + dy, spring);
        } else if (id === 26) {
          for (let dy = 2; dy < 6; dy++) for (let dx = 2; dx < 6; dx++) this.set(px + dx, py + dy, fruit);
        }
      }
    }
  }

  /** Draw concrete paths over the last `render` of `canvas`, the first one
   *  lowest. Per path: the route as a thick translucent ribbon through the
   *  player's 8x8 sprite box centres (brighter up to frame `upto`), a dot
   *  per frame (their spacing is the speed), an arrow where each dash
   *  starts (pointing its way; faint until it has happened), the three
   *  frames before `upto` as fading box outlines, and the box at `upto`
   *  solid - filled for a `filled` path, a heavy outline otherwise, so two
   *  paths on the same spot still read as two. A position is the player's
   *  `(x, y)`, the cell the heat layers count it in: the box's top-left. A
   *  frame without a player, or in the next room, breaks the ribbon; past
   *  its last frame (the exit) a path shows no box. */
  paths(canvas: HTMLCanvasElement, list: DrawPath[], upto: number) {
    const ctx = canvas.getContext("2d")!;
    const s = canvas.width / this.box.w;
    const ink = "rgba(8, 10, 12, 0.85)";
    ctx.save();
    ctx.lineJoin = "round";
    ctx.lineCap = "round";
    for (const d of list) {
      const { path, color } = d;
      const col = (a: number) => `rgba(${color[0]},${color[1]},${color[2]},${a})`;
      const corner = (i: number): [number, number] | null => {
        const p = i >= 0 ? path[i] : null;
        if (!p || p[0] >= NEXT_ROOM_X) return null;
        return [(p[0] - this.box.x0) * s, (p[1] - this.box.y0) * s];
      };
      const centre = (i: number): [number, number] | null => {
        const q = corner(i);
        return q && [q[0] + 4 * s, q[1] + 4 * s];
      };
      const last = path.length - 1;
      const u = Math.min(last, upto);
      // The outlined path's ribbon is wider, so where the two paths run
      // together the reference shows as a yellow edge around ours; a dark
      // casing under each keeps it apart from any heat colour.
      const width = (d.filled ? 2 : 3.6) * s;
      const ribbon = (from: number, to: number, stroke: string, w: number) => {
        ctx.beginPath();
        let pen = false;
        for (let i = from; i <= to; i++) {
          const q = centre(i);
          if (!q) {
            pen = false;
            continue;
          }
          if (pen) ctx.lineTo(q[0], q[1]);
          else ctx.moveTo(q[0], q[1]);
          pen = true;
        }
        ctx.strokeStyle = stroke;
        ctx.lineWidth = w;
        ctx.stroke();
      };
      ribbon(0, last, "rgba(8, 10, 12, 0.6)", width + Math.max(2, s * 0.8));
      ribbon(Math.max(0, u), last, col(0.35), width);
      ribbon(0, u, col(0.7), width);
      // A dark dot per frame on the ribbon.
      for (let i = 0; i <= last; i++) {
        const q = centre(i);
        if (!q) continue;
        ctx.beginPath();
        ctx.arc(q[0], q[1], Math.max(1.2, s * 0.45), 0, 2 * Math.PI);
        ctx.fillStyle = i <= u ? "rgba(8, 10, 12, 0.8)" : "rgba(8, 10, 12, 0.45)";
        ctx.fill();
      }
      // The dashes: an arrow on the box centre, pointing its way.
      for (const [f, dir] of d.dashes) {
        const q = centre(f);
        if (!q) continue;
        const dx = (dir.includes("R") ? 1 : 0) - (dir.includes("L") ? 1 : 0);
        const dy = (dir.includes("D") ? 1 : 0) - (dir.includes("U") ? 1 : 0);
        const n = Math.hypot(dx, dy) || 1;
        const [ux, uy] = [dx / n, dy / n];
        const r = Math.max(7, s * 4.2);
        ctx.beginPath();
        ctx.moveTo(q[0] + ux * r, q[1] + uy * r);
        ctx.lineTo(q[0] - ux * r * 0.55 - uy * r * 0.7, q[1] - uy * r * 0.55 + ux * r * 0.7);
        ctx.lineTo(q[0] - ux * r * 0.2, q[1] - uy * r * 0.2);
        ctx.lineTo(q[0] - ux * r * 0.55 + uy * r * 0.7, q[1] - uy * r * 0.55 - ux * r * 0.7);
        ctx.closePath();
        ctx.fillStyle = col(f <= u ? 1 : 0.45);
        ctx.fill();
        ctx.lineWidth = Math.max(1, s * 0.4);
        ctx.strokeStyle = ink;
        ctx.stroke();
      }
      if (upto > last) continue;
      // The frames just before: fading outlines.
      for (let k = 3; k >= 1; k--) {
        const q = corner(u - k);
        if (!q) continue;
        ctx.lineWidth = Math.max(1, s * 0.5);
        ctx.strokeStyle = col(0.55 - 0.13 * k);
        ctx.strokeRect(q[0], q[1], 8 * s, 8 * s);
      }
      // The current frame's box.
      const q = corner(u);
      if (q) {
        const [x, y, w] = [q[0], q[1], 8 * s];
        const lw = Math.max(2, s * 0.9);
        ctx.lineWidth = lw + Math.max(2, s * 0.6);
        ctx.strokeStyle = ink;
        ctx.strokeRect(x, y, w, w);
        if (d.filled) {
          ctx.fillStyle = col(0.55);
          ctx.fillRect(x, y, w, w);
        }
        ctx.lineWidth = lw;
        ctx.strokeStyle = col(1);
        ctx.strokeRect(x, y, w, w);
      }
    }
    ctx.restore();
  }

  /** Composite `scene` into the offscreen and draw it scaled onto `canvas`:
   *  sized to its CSS box (times the device pixel ratio), or to `size`
   *  pixels exactly when given (the image / video export, off screen). */
  render(canvas: HTMLCanvasElement, scene: Scene, size?: { w: number; h: number }) {
    const { w, h } = this.box;
    const n = w * h;
    const acc = this.acc;
    const inRoom = this.inRoom;
    acc.set(this.base);
    for (const layer of scene.layers) {
      const { grid, ramp, alpha } = layer;
      const lmax = Math.log1p(Math.max(1, layer.max));
      const top = ramp.length - 1;
      for (let i = 0; i < n; i++) {
        const v = grid[i];
        if (v <= 0 || !inRoom[i]) continue;
        const t = layer.flat ? 1 : Math.min(1, Math.log1p(v) / lmax);
        const c = ramp[Math.round(t * top)];
        // Faint cells stay visible: alpha never drops below 55% of the layer's.
        const a = layer.flat ? alpha : alpha * (0.55 + 0.45 * t);
        const j = i * 3;
        acc[j] += (c[0] - acc[j]) * a;
        acc[j + 1] += (c[1] - acc[j + 1]) * a;
        acc[j + 2] += (c[2] - acc[j + 2]) * a;
      }
    }
    const px = this.img.data;
    for (let i = 0, j = 0; i < n; i++, j += 3) {
      const k = i * 4;
      px[k] = acc[j];
      px[k + 1] = acc[j + 1];
      px[k + 2] = acc[j + 2];
      px[k + 3] = 255;
    }
    this.off.getContext("2d")!.putImageData(this.img, 0, 0);

    const dpr = window.devicePixelRatio || 1;
    const cw = canvas.clientWidth || w;
    const ch = Math.round((cw * h) / w);
    const pw = size ? size.w : Math.round(cw * dpr);
    const ph = size ? size.h : Math.round(ch * dpr);
    if (canvas.width !== pw || canvas.height !== ph) {
      canvas.width = pw;
      canvas.height = ph;
    }
    const ctx = canvas.getContext("2d")!;
    ctx.imageSmoothingEnabled = false;
    ctx.drawImage(this.off, 0, 0, pw, ph);
  }
}

/** Add a sparse record into a dense grid (NO_POSITION entries are
 *  returned as the off-grid total instead). */
export function addSparse(grid: Float32Array, s: Sparse, scale = 1): number {
  let off = 0;
  const n = s.idx.length;
  for (let i = 0; i < n; i++) {
    const idx = s.idx[i];
    if (idx === NO_POSITION) off += s.count[i];
    else grid[idx] += s.count[i] * scale;
  }
  return off;
}

export function sparseMax(s: Sparse): number {
  let m = 0;
  for (let i = 0; i < s.count.length; i++) if (s.idx[i] !== NO_POSITION && s.count[i] > m) m = s.count[i];
  return m;
}
