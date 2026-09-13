// Small DOM helpers and the two touch controls every view shares: chips
// (toggle / choice rows) and the scrubber (a full-width, 48px-tall range
// you drag with a finger; pointer capture so the drag survives leaving it).

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

export interface ChipSpec<T> {
  value: T;
  label: string;
  /** A colored key drawn before the label (identity lives in the mark). */
  color?: string;
  title?: string;
}

/** A row of chips. `multi` toggles membership; otherwise one is chosen. */
export function chips<T>(
  specs: ChipSpec<T>[],
  selected: Set<T>,
  onChange: (sel: Set<T>) => void,
  opts: { multi?: boolean; label?: string } = {},
): HTMLElement {
  const row = el("div", { class: "chips", role: opts.multi ? "group" : "radiogroup" });
  if (opts.label) row.append(el("span", { class: "chips-label", text: opts.label }));
  const buttons: HTMLButtonElement[] = [];
  const sync = () => {
    buttons.forEach((b, i) => {
      const on = selected.has(specs[i].value);
      b.classList.toggle("on", on);
      b.setAttribute(opts.multi ? "aria-pressed" : "aria-checked", on ? "true" : "false");
    });
  };
  specs.forEach((s) => {
    const b = el("button", { class: "chip", type: "button", title: s.title, role: opts.multi ? undefined : "radio" });
    if (s.color) b.append(el("span", { class: "chip-key", style: `background:${s.color}` }));
    b.append(el("span", { text: s.label }));
    b.addEventListener("click", () => {
      if (opts.multi) {
        if (selected.has(s.value)) selected.delete(s.value);
        else selected.add(s.value);
      } else {
        selected.clear();
        selected.add(s.value);
      }
      sync();
      onChange(selected);
    });
    buttons.push(b);
    row.append(b);
  });
  sync();
  return row;
}

export interface Scrubber {
  root: HTMLElement;
  set(value: number, max: number): void;
  /** Draw a per-step backdrop (e.g. the chapter bands) on the track. */
  backdrop(draw: (ctx: CanvasRenderingContext2D, w: number, h: number) => void): void;
}

/** A scrubber over integer positions 0..max. Dragging anywhere on it
 *  moves the playhead; the track shows an optional backdrop. */
export function scrubber(onScrub: (value: number) => void, onGrab?: (grabbing: boolean) => void): Scrubber {
  const canvas = el("canvas", { class: "scrub-track" });
  const knob = el("div", { class: "scrub-knob" });
  const root = el("div", { class: "scrubber", role: "slider", tabindex: 0, "aria-valuemin": 0 }, [canvas, knob]);
  let value = 0;
  let max = 1;
  let drawBackdrop: ((ctx: CanvasRenderingContext2D, w: number, h: number) => void) | null = null;
  const place = () => {
    const w = root.clientWidth;
    const x = max > 0 ? (value / max) * w : 0;
    knob.style.transform = `translateX(${x}px)`;
    root.setAttribute("aria-valuenow", String(value));
    root.setAttribute("aria-valuemax", String(max));
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
  root.addEventListener("pointerdown", (ev) => {
    root.setPointerCapture(ev.pointerId);
    root.classList.add("grabbing");
    onGrab?.(true);
    const v = fromEvent(ev);
    if (v !== value) {
      value = v;
      place();
      onScrub(v);
    }
    ev.preventDefault();
  });
  root.addEventListener("pointermove", (ev) => {
    if (!root.hasPointerCapture(ev.pointerId)) return;
    const v = fromEvent(ev);
    if (v !== value) {
      value = v;
      place();
      onScrub(v);
    }
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
    if (v !== value) {
      value = v;
      place();
      onScrub(v);
    }
  });
  new ResizeObserver(paint).observe(root);
  return {
    root,
    set(v, m) {
      const changedMax = m !== max;
      max = Math.max(0, m);
      value = Math.max(0, Math.min(max, v));
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

/** Big touch button. */
export function button(label: string, onClick: () => void, cls = ""): HTMLButtonElement {
  const b = el("button", { class: `btn ${cls}`, type: "button", text: label });
  b.addEventListener("click", onClick);
  return b;
}
