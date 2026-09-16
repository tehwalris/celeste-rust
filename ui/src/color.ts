// Colors, by the job they do (dataviz method, dark mode throughout):
//  - identity of a LEVEL (an ordered rung of the precision ladder): one
//    hue per level on a cool->warm OKLCH path, coarse = blue, exact =
//    orange. Fixed per level, never by rank among the levels shown.
//  - magnitude (states per cell): the level's hue, dark->bright.
//  - the marked set: warm white, the one thing drawn over everything.
//  - the timing phases: categorical slots (forward = blue, backward =
//    orange) with an ordinal step per split inside each.

export type RGB = [number, number, number];

export const inkSecondary = "#c3c2b7";
export const inkMuted = "#898781";
export const gridline = "#2c2c2a";
export const baseline = "#383835";

/** Dark-mode categorical slots (validated adjacent order). */
export const slots = ["#3987e5", "#d95926", "#199e70", "#c98500", "#d55181", "#008300", "#9085e9", "#e66767"];

export const rgbCss = (c: RGB, a = 1) => `rgba(${c[0]},${c[1]},${c[2]},${a})`;

// OKLCH -> sRGB (Björn Ottosson's OKLab), enough for a hue path.
function oklabToLinear(L: number, a: number, b: number): RGB {
  const l_ = L + 0.3963377774 * a + 0.2158037573 * b;
  const m_ = L - 0.1055613458 * a - 0.0638541728 * b;
  const s_ = L - 0.0894841775 * a - 1.291485548 * b;
  const l = l_ * l_ * l_;
  const m = m_ * m_ * m_;
  const s = s_ * s_ * s_;
  return [
    4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
    -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
    -0.0041960863 * l - 0.7034186147 * m + 1.707614701 * s,
  ];
}
const gamma = (c: number) => {
  const v = Math.max(0, Math.min(1, c));
  return v <= 0.0031308 ? 12.92 * v : 1.055 * Math.pow(v, 1 / 2.4) - 0.055;
};
export function oklch(L: number, C: number, hDeg: number): RGB {
  const h = (hDeg * Math.PI) / 180;
  const [r, g, b] = oklabToLinear(L, C * Math.cos(h), C * Math.sin(h));
  return [Math.round(gamma(r) * 255), Math.round(gamma(g) * 255), Math.round(gamma(b) * 255)];
}

export const LEVELS = 17;
/** Hue of a level: 0 (coarsest) = blue 255deg, going through violet and
 *  magenta to 16 (exact) = orange 55deg. Fixed by level index. */
export function levelHue(level: number): number {
  const t = Math.max(0, Math.min(1, level / (LEVELS - 1)));
  // 255 -> 415 (= 55 mod 360): blue, violet, magenta, red, orange.
  return (255 + 160 * t) % 360;
}
const levelColorCache = new Map<number, RGB>();
/** The level's identity color (for lines, chips, legends). */
export function levelColor(level: number): RGB {
  let c = levelColorCache.get(level);
  if (!c) {
    c = oklch(0.72, 0.16, levelHue(level));
    levelColorCache.set(level, c);
  }
  return c;
}
export const levelCss = (level: number, a = 1) => rgbCss(levelColor(level), a);

/** A one-hue magnitude ramp: t in [0,1] -> lightness `lo`..`hi` at `hue`,
 *  chroma peaking mid-ramp so the top end reads as light, not neon. */
export function hueRamp(hue: number, lo: number, hi: number, chroma = 0.15): RGB[] {
  const out: RGB[] = [];
  for (let i = 0; i < 64; i++) {
    const t = i / 63;
    const L = lo + (hi - lo) * t;
    const C = 0.04 + chroma * Math.sin(Math.PI * Math.min(1, 0.15 + t * 0.9));
    out.push(oklch(L, C, hue));
  }
  return out;
}

const rampCache = new Map<string, RGB[]>();
/** The level's bright ramp (its frontier) or dim ramp (its visited set). */
export function levelRamp(level: number, kind: "bright" | "dim" | "band"): RGB[] {
  const key = `${level}:${kind}`;
  let r = rampCache.get(key);
  if (!r) {
    const hue = levelHue(level);
    r =
      kind === "bright" ? hueRamp(hue, 0.6, 0.97) : kind === "dim" ? hueRamp(hue, 0.33, 0.56, 0.1) : hueRamp(hue, 0.3, 0.5, 0.07);
    rampCache.set(key, r);
  }
  return r;
}

/** The marked set: warm white, dark->bright. */
export function marksRamp(kind: "bright" | "dim"): RGB[] {
  const key = `marks:${kind}`;
  let r = rampCache.get(key);
  if (!r) {
    r = kind === "bright" ? hueRamp(75, 0.6, 0.99, 0.06) : hueRamp(75, 0.4, 0.7, 0.04);
    rampCache.set(key, r);
  }
  return r;
}

/** The "height map" look: the 17 levels collapsed into 7 perceptual
 *  BANDS (the marked-set curve is flat inside each), coloured so that
 *  salience rises as the band gets thinner: the broad coarse bands are
 *  dark and desaturated (they are area), lightness and chroma climb
 *  monotonically with the band, and the exact route is the brightest
 *  thing on screen. */
export const HEIGHT_BANDS: { label: string; from: number; to: number }[] = [
  { label: "L0", from: 0, to: 0 },
  { label: "L1", from: 1, to: 1 },
  { label: "L2–5", from: 2, to: 5 },
  { label: "L6–7", from: 6, to: 7 },
  { label: "L8–12", from: 8, to: 12 },
  { label: "L13–14", from: 13, to: 14 },
  { label: "L15 + exact", from: 15, to: 16 },
];
const BAND_LCH: [number, number, number][] = [
  [0.27, 0.02, 260],
  [0.36, 0.05, 250],
  [0.47, 0.09, 225],
  [0.6, 0.13, 185],
  [0.74, 0.17, 140],
  [0.87, 0.19, 105],
  [0.98, 0.12, 95],
];
export function heightBand(level: number): number {
  const i = HEIGHT_BANDS.findIndex((b) => level >= b.from && level <= b.to);
  return i < 0 ? HEIGHT_BANDS.length - 1 : i;
}
export function bandColor(band: number): RGB {
  const key = `band:${band}`;
  let c = rampCache.get(key);
  if (!c) {
    const [L, C, h] = BAND_LCH[Math.max(0, Math.min(BAND_LCH.length - 1, band))];
    c = [oklch(L, C, h)];
    rampCache.set(key, c);
  }
  return c[0];
}
/** The halo under the thinnest bands: the band's colour, dimmer. */
export function bandHalo(band: number): RGB {
  const key = `halo:${band}`;
  let c = rampCache.get(key);
  if (!c) {
    const [L, C, h] = BAND_LCH[Math.max(0, Math.min(BAND_LCH.length - 1, band))];
    c = [oklch(L * 0.72, C * 0.8, h)];
    rampCache.set(key, c);
  }
  return c[0];
}
/** The moving set in the height-map look: magenta, absent from the ramp. */
export const movingColor: RGB = oklch(0.75, 0.22, 345);


/** Phase colors for the waterfall: forward and backward are the first two
 *  categorical slots; their splits are ordinal steps of the same hue. */
export const phaseColor = { fwd: slots[0], bwd: slots[1] };
export const fwdSplit = { emit: "#3987e5", own: "#6da7ec", ckpt: "#1c5cab", pos: "#9ec5f4" };
export const bwdSplit = { load: "#ef8a5f", par: "#d95926" };
export const prebuildColor = "#898781";
