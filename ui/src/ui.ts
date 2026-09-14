// Small DOM helpers and the touch controls every view shares: chips (a
// choice among a few options, drawn as a segmented control), the
// scrubber (a full-width range you drag with a finger; pointer capture
// so the drag survives leaving it; a bubble names the position while it
// is held), buttons, and hi-DPI canvases.

export function el<K extends keyof HTMLElementTagNameMap>(
  tag: K,
  attrs: Record<string, string | number | boolean | undefined> = {},
  children: (Node | string | null | undefined)[] = [],
): HTMLElementTagNameMap[K] {
  const e = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (v === undefined || v === false) continue;
    if (k === "class") e.className = String(v);
    else if (k === "text") e.textContent = String(v);
    else e.setAttribute(k, v === true ? "" : String(v));
  }
  for (const c of children) {
    if (c == null) continue;
    e.append(typeof c === "string" ? document.createTextNode(c) : c);
  }
  return e;
}

export function clear(e: HTMLElement) {
  while (e.firstChild) e.removeChild(e.firstChild);
}

/** Show / hide an element regardless of its CSS `display` (the `hidden`
 *  attribute alone loses to any `display:` rule on the element). */
export function show(e: HTMLElement, on: boolean) {
  e.classList.toggle("is-hidden", !on);
  e.hidden = !on;
}

export interface ChipSpec<T> {
  value: T;
  label: string;
  /** A colored key drawn before the label (identity lives in the mark). */
  color?: string;
  title?: string;
  /** A second, quieter line or suffix (units, a hint). */
  hint?: string;
}

export interface Chips<T> {
  root: HTMLElement;
  /** Move the selection without firing onChange. */
  set(value: T): void;
}

/** A row of chips: one is chosen (radio semantics), drawn as a segmented
 *  control. `label` names the axis it controls. */
export function chips<T>(
  specs: ChipSpec<T>[],
  selected: T,
  onChange: (value: T) => void,
  opts: { label?: string; class?: string; scroll?: boolean } = {},
): Chips<T> {
  const seg = el("div", { class: `seg${opts.scroll ? " seg-scroll" : ""}`, role: "radiogroup" });
  const root = el("div", { class: `chips${opts.class ? " " + opts.class : ""}` });
  if (opts.label) {
    root.append(el("span", { class: "chips-label", text: opts.label }));
    seg.setAttribute("aria-label", opts.label);
  }
  root.append(seg);
  let current = selected;
  const buttons: HTMLButtonElement[] = [];
  const sync = () => {
    buttons.forEach((b, i) => {
      const on = specs[i].value === current;
      b.classList.toggle("on", on);
      b.setAttribute("aria-checked", on ? "true" : "false");
      b.tabIndex = on ? 0 : -1;
    });
  };
  specs.forEach((s, i) => {
    const b = el("button", { class: "chip", type: "button", title: s.title, role: "radio" });
    if (s.color) b.append(el("span", { class: "chip-key", style: `background:${s.color}` }));
    b.append(el("span", { class: "chip-text", text: s.label }));
    if (s.hint) b.append(el("span", { class: "chip-hint", text: s.hint }));
    b.addEventListener("click", () => {
      if (current === s.value) return;
      current = s.value;
      sync();
      onChange(current);
    });
    // Arrow keys move within the group, as a radio group does.
    b.addEventListener("keydown", (ev) => {
      if (ev.key !== "ArrowLeft" && ev.key !== "ArrowRight") return;
      ev.preventDefault();
      const j = (i + (ev.key === "ArrowRight" ? 1 : specs.length - 1)) % specs.length;
      buttons[j].focus();
      buttons[j].click();
    });
    buttons.push(b);
    seg.append(b);
  });
  sync();
  return {
    root,
    set(v) {
      current = v;
      sync();
      // Keep the chosen chip in view when the row scrolls.
      const on = buttons.find((b) => b.classList.contains("on"));
      if (on && opts.scroll) requestAnimationFrame(() => (seg.scrollLeft = on.offsetLeft - seg.clientWidth / 2 + on.offsetWidth / 2));
    },
  };
}

export interface Scrubber {
  root: HTMLElement;
  /** Move the playhead; `label` is what the bubble says while held. */
  set(value: number, max: number, label?: string): void;
  /** Draw a per-step backdrop (e.g. the pass bands) on the track. */
  backdrop(draw: (ctx: CanvasRenderingContext2D, w: number, h: number) => void): void;
}

/** A scrubber over integer positions 0..max. Dragging anywhere on it
 *  moves the playhead; the track shows an optional backdrop. */
export function scrubber(onScrub: (value: number) => void, onGrab?: (grabbing: boolean) => void): Scrubber {
  const canvas = el("canvas", { class: "scrub-track" });
  const knob = el("div", { class: "scrub-knob" });
  const bubble = el("div", { class: "scrub-bubble" });
  const root = el("div", { class: "scrubber", role: "slider", tabindex: 0, "aria-valuemin": 0, "aria-label": "position in the ladder" }, [canvas, knob, bubble]);
  let value = 0;
  let max = 1;
  let label = "";
  let drawBackdrop: ((ctx: CanvasRenderingContext2D, w: number, h: number) => void) | null = null;
  const place = () => {
    const w = root.clientWidth;
    const x = max > 0 ? (value / max) * w : 0;
    knob.style.transform = `translateX(${x}px)`;
    // The bubble stays inside the track: clamp its centre.
    const bw = bubble.offsetWidth || 40;
    bubble.style.transform = `translateX(${Math.max(bw / 2, Math.min(w - bw / 2, x))}px)`;
    bubble.textContent = label;
    root.setAttribute("aria-valuenow", String(value));
    root.setAttribute("aria-valuemax", String(max));
    root.setAttribute("aria-valuetext", label);
  };
  const paint = () => {
    const w = root.clientWidth;
    const h = root.clientHeight;
    if (!w || !h) return;
    const dpr = window.devicePixelRatio || 1;
    canvas.width = Math.round(w * dpr);
    canvas.height = Math.round(h * dpr);
    const ctx = canvas.getContext("2d")!;
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, w, h);
    if (drawBackdrop) drawBackdrop(ctx, w, h);
    place();
  };
  const fromEvent = (ev: PointerEvent) => {
    const r = root.getBoundingClientRect();
    const t = Math.max(0, Math.min(1, (ev.clientX - r.left) / r.width));
    return Math.round(t * max);
  };
  const move = (v: number) => {
    if (v === value) return;
    value = v;
    place();
    onScrub(v);
  };
  root.addEventListener("pointerdown", (ev) => {
    root.setPointerCapture(ev.pointerId);
    root.classList.add("grabbing");
    onGrab?.(true);
    move(fromEvent(ev));
    ev.preventDefault();
  });
  root.addEventListener("pointermove", (ev) => {
    if (!root.hasPointerCapture(ev.pointerId)) return;
    move(fromEvent(ev));
  });
  const release = (ev: PointerEvent) => {
    if (root.hasPointerCapture(ev.pointerId)) root.releasePointerCapture(ev.pointerId);
    root.classList.remove("grabbing");
    onGrab?.(false);
  };
  root.addEventListener("pointerup", release);
  root.addEventListener("pointercancel", release);
  root.addEventListener("keydown", (ev) => {
    const step = ev.shiftKey ? 10 : 1;
    let v = value;
    if (ev.key === "ArrowLeft") v = Math.max(0, value - step);
    else if (ev.key === "ArrowRight") v = Math.min(max, value + step);
    else if (ev.key === "Home") v = 0;
    else if (ev.key === "End") v = max;
    else return;
    ev.preventDefault();
    move(v);
  });
  new ResizeObserver(paint).observe(root);
  return {
    root,
    set(v, m, l = "") {
      const changedMax = m !== max;
      max = Math.max(0, m);
      value = Math.max(0, Math.min(max, v));
      label = l;
      if (changedMax) paint();
      else place();
    },
    backdrop(draw) {
      drawBackdrop = draw;
      paint();
    },
  };
}

/** A hi-DPI canvas sized to its CSS box; `draw` runs on every resize. */
export function fitCanvas(
  canvas: HTMLCanvasElement,
  draw: (ctx: CanvasRenderingContext2D, w: number, h: number) => void,
): () => void {
  const redraw = () => {
    const w = canvas.clientWidth;
    const h = canvas.clientHeight;
    if (!w || !h) return;
    const dpr = window.devicePixelRatio || 1;
    const pw = Math.round(w * dpr);
    const ph = Math.round(h * dpr);
    if (canvas.width !== pw || canvas.height !== ph) {
      canvas.width = pw;
      canvas.height = ph;
    }
    const ctx = canvas.getContext("2d")!;
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    draw(ctx, w, h);
  };
  new ResizeObserver(redraw).observe(canvas);
  return redraw;
}

/** A touch button; `label` may be text or an icon node. */
export function button(label: string | Node, onClick: () => void, cls = "", title?: string): HTMLButtonElement {
  const b = el("button", { class: `btn ${cls}`, type: "button", title, "aria-label": title });
  b.append(label);
  b.addEventListener("click", onClick);
  return b;
}

/** Inline SVG icons (24-unit grid, currentColor). */
export function icon(name: "play" | "pause" | "prev" | "next" | "stepBack" | "stepFwd" | "target" | "chevron"): SVGSVGElement {
  const paths: Record<string, string> = {
    play: "M8 5.5v13l11-6.5z",
    pause: "M7 5h4v14H7zm6 0h4v14h-4z",
    prev: "M6 6h2v12H6zm12 0-9 6 9 6z",
    next: "M16 6h2v12h-2zM6 6l9 6-9 6z",
    stepBack: "M15 6l-8 6 8 6z",
    stepFwd: "M9 6l8 6-8 6z",
    target: "M12 4a8 8 0 1 0 0 16 8 8 0 0 0 0-16zm0 2a6 6 0 1 1 0 12 6 6 0 0 1 0-12zm0 3.5a2.5 2.5 0 1 0 0 5 2.5 2.5 0 0 0 0-5z",
    chevron: "M8.5 5.5 15 12l-6.5 6.5-1.4-1.4L12.2 12 7.1 6.9z",
  };
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("viewBox", "0 0 24 24");
  svg.setAttribute("width", "20");
  svg.setAttribute("height", "20");
  svg.setAttribute("aria-hidden", "true");
  const p = document.createElementNS("http://www.w3.org/2000/svg", "path");
  p.setAttribute("d", paths[name]);
  p.setAttribute("fill", "currentColor");
  svg.append(p);
  return svg;
}

/** A key/value strip: `[[label, value], ...]` as small tabular pairs. */
export function kv(pairs: [string, string][]): HTMLElement {
  return el(
    "div",
    { class: "kv" },
    pairs.map(([k, v]) => el("span", { class: "kv-pair" }, [el("b", { text: v }), el("span", { text: k })])),
  );
}
