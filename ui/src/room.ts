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

  /** Composite `scene` into the offscreen and draw it scaled onto `canvas`. */
  render(canvas: HTMLCanvasElement, scene: Scene) {
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
    const pw = Math.round(cw * dpr);
    const ph = Math.round(ch * dpr);
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
