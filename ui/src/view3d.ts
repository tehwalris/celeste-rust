// The room in 3D: a WebGL2 view of instanced boxes you orbit, tilt, pan
// and zoom with phone gestures. It knows nothing about the search: the
// space view hands it two instance buffers (the static floor and tiles,
// the dynamic data) and asks for a draw.
//
// World: X = cell x, Z = cell y (down the screen when seen from above),
// Y = up. One instance is a box: its footprint corner (cx, cz), its base
// height y0 and height h, its footprint (sx, sz), and a colour.
//
// Gestures: one finger orbits (drag sideways turns the map, drag up and
// down tilts it from top-down to nearly edge-on), two fingers pan and
// pinch-zoom, a double tap resets; a mouse wheel zooms, shift- or
// right-drag pans.

export const FLOATS_PER_INSTANCE = 9;

const VS = `#version 300 es
precision highp float;
layout(location=0) in vec3 aPos;
layout(location=1) in vec3 aNrm;
layout(location=2) in vec4 aBox;   // cx, cz, y0, h
layout(location=3) in vec2 aSize;  // sx, sz
layout(location=4) in vec3 aCol;
uniform mat4 uVP;
uniform float uDimBelow;           // instances whose base is below this are dimmed
uniform float uDim;
out vec3 vCol;
void main() {
  vec3 p = vec3(aBox.x + aPos.x * aSize.x, aBox.z + aPos.y * aBox.w, aBox.y + aPos.z * aSize.y);
  gl_Position = uVP * vec4(p, 1.0);
  vec3 L = normalize(vec3(0.35, 1.0, 0.5));
  float d = max(dot(aNrm, L), 0.0);
  float shade = 0.45 + 0.55 * d;
  float dim = aBox.z < uDimBelow ? uDim : 1.0;
  vCol = aCol * shade * dim;
}`;
const FS = `#version 300 es
precision mediump float;
in vec3 vCol;
out vec4 o;
void main() { o = vec4(vCol, 1.0); }`;

// A unit cube [0,1]^3 as 12 triangles with flat normals.
function cubeGeometry(): Float32Array {
  const faces: { n: [number, number, number]; v: [number, number, number][] }[] = [
    { n: [0, 1, 0], v: [[0, 1, 0], [0, 1, 1], [1, 1, 1], [1, 1, 0]] }, // top
    { n: [0, -1, 0], v: [[0, 0, 0], [1, 0, 0], [1, 0, 1], [0, 0, 1]] }, // bottom
    { n: [1, 0, 0], v: [[1, 0, 0], [1, 1, 0], [1, 1, 1], [1, 0, 1]] }, // +x
    { n: [-1, 0, 0], v: [[0, 0, 0], [0, 0, 1], [0, 1, 1], [0, 1, 0]] }, // -x
    { n: [0, 0, 1], v: [[0, 0, 1], [1, 0, 1], [1, 1, 1], [0, 1, 1]] }, // +z
    { n: [0, 0, -1], v: [[0, 0, 0], [0, 1, 0], [1, 1, 0], [1, 0, 0]] }, // -z
  ];
  const out: number[] = [];
  for (const f of faces) {
    const [a, b, c, d] = f.v;
    for (const p of [a, b, c, a, c, d]) out.push(...p, ...f.n);
  }
  return new Float32Array(out);
}

type Mat4 = Float32Array;
function perspective(fovY: number, aspect: number, near: number, far: number): Mat4 {
  const f = 1 / Math.tan(fovY / 2);
  const m = new Float32Array(16);
  m[0] = f / aspect;
  m[5] = f;
  m[10] = (far + near) / (near - far);
  m[11] = -1;
  m[14] = (2 * far * near) / (near - far);
  return m;
}
function lookAt(eye: number[], target: number[], up: number[]): Mat4 {
  const sub = (a: number[], b: number[]) => [a[0] - b[0], a[1] - b[1], a[2] - b[2]];
  const cross = (a: number[], b: number[]) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]];
  const norm = (a: number[]) => {
    const l = Math.hypot(a[0], a[1], a[2]) || 1;
    return [a[0] / l, a[1] / l, a[2] / l];
  };
  const dot = (a: number[], b: number[]) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
  const z = norm(sub(eye, target));
  const x = norm(cross(up, z));
  const y = cross(z, x);
  const m = new Float32Array(16);
  m[0] = x[0]; m[4] = x[1]; m[8] = x[2]; m[12] = -dot(x, eye);
  m[1] = y[0]; m[5] = y[1]; m[9] = y[2]; m[13] = -dot(y, eye);
  m[2] = z[0]; m[6] = z[1]; m[10] = z[2]; m[14] = -dot(z, eye);
  m[15] = 1;
  return m;
}
function mul(a: Mat4, b: Mat4): Mat4 {
  const o = new Float32Array(16);
  for (let c = 0; c < 4; c++) for (let r = 0; r < 4; r++) o[c * 4 + r] = a[r] * b[c * 4] + a[4 + r] * b[c * 4 + 1] + a[8 + r] * b[c * 4 + 2] + a[12 + r] * b[c * 4 + 3];
  return o;
}

interface Camera {
  target: [number, number, number];
  yaw: number;
  pitch: number;
  dist: number;
}

export interface View3DOptions {
  /** The map's footprint in world units: the camera's home is over its centre. */
  width: number;
  height: number;
  /** Home tilt in degrees above the plane (90 = straight down). */
  homePitch?: number;
}

export class View3D {
  readonly canvas: HTMLCanvasElement;
  private readonly gl: WebGL2RenderingContext;
  private readonly prog: WebGLProgram;
  private readonly uVP: WebGLUniformLocation;
  private readonly uDimBelow: WebGLUniformLocation;
  private readonly uDim: WebGLUniformLocation;
  private readonly vaoStatic: WebGLVertexArrayObject;
  private readonly vaoDynamic: WebGLVertexArrayObject;
  private readonly bufStatic: WebGLBuffer;
  private readonly bufDynamic: WebGLBuffer;
  private nStatic = 0;
  private nDynamic = 0;
  private dynamicCap = 0;
  /** Draw only the first `dynamicLimit` dynamic instances (a prefix). */
  private dynamicLimit = -1;
  private dimBelow = -1e9;
  private dim = 1;
  private readonly cam: Camera;
  private readonly home: Camera;
  private raf = 0;
  private readonly opts: View3DOptions;
  onChange: (() => void) | null = null;

  constructor(opts: View3DOptions) {
    this.opts = opts;
    this.canvas = document.createElement("canvas");
    this.canvas.className = "view3d";
    const gl = this.canvas.getContext("webgl2", { antialias: true, alpha: false, powerPreference: "high-performance" });
    if (!gl) throw new Error("WebGL2 is not available");
    this.gl = gl;
    this.prog = this.link(VS, FS);
    this.uVP = gl.getUniformLocation(this.prog, "uVP")!;
    this.uDimBelow = gl.getUniformLocation(this.prog, "uDimBelow")!;
    this.uDim = gl.getUniformLocation(this.prog, "uDim")!;

    const geom = gl.createBuffer()!;
    gl.bindBuffer(gl.ARRAY_BUFFER, geom);
    gl.bufferData(gl.ARRAY_BUFFER, cubeGeometry(), gl.STATIC_DRAW);
    this.bufStatic = gl.createBuffer()!;
    this.bufDynamic = gl.createBuffer()!;
    this.vaoStatic = this.makeVao(geom, this.bufStatic);
    this.vaoDynamic = this.makeVao(geom, this.bufDynamic);

    gl.enable(gl.DEPTH_TEST);
    gl.enable(gl.CULL_FACE);
    gl.cullFace(gl.BACK);
    gl.clearColor(0x0d / 255, 0x0d / 255, 0x0d / 255, 1);

    const homePitch = ((opts.homePitch ?? 62) * Math.PI) / 180;
    this.home = { target: [opts.width / 2, 0, opts.height / 2], yaw: 0, pitch: homePitch, dist: Math.max(opts.width, opts.height) * 1.9 };
    this.cam = { ...this.home, target: [...this.home.target] };
    this.gestures();
    new ResizeObserver(() => this.requestDraw()).observe(this.canvas);
  }

  private link(vs: string, fs: string): WebGLProgram {
    const gl = this.gl;
    const sh = (type: number, src: string) => {
      const s = gl.createShader(type)!;
      gl.shaderSource(s, src);
      gl.compileShader(s);
      if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(s) ?? "shader");
      return s;
    };
    const p = gl.createProgram()!;
    gl.attachShader(p, sh(gl.VERTEX_SHADER, vs));
    gl.attachShader(p, sh(gl.FRAGMENT_SHADER, fs));
    gl.linkProgram(p);
    if (!gl.getProgramParameter(p, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(p) ?? "link");
    return p;
  }

  private makeVao(geom: WebGLBuffer, inst: WebGLBuffer): WebGLVertexArrayObject {
    const gl = this.gl;
    const vao = gl.createVertexArray()!;
    gl.bindVertexArray(vao);
    gl.bindBuffer(gl.ARRAY_BUFFER, geom);
    gl.enableVertexAttribArray(0);
    gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 24, 0);
    gl.enableVertexAttribArray(1);
    gl.vertexAttribPointer(1, 3, gl.FLOAT, false, 24, 12);
    gl.bindBuffer(gl.ARRAY_BUFFER, inst);
    const stride = FLOATS_PER_INSTANCE * 4;
    gl.enableVertexAttribArray(2);
    gl.vertexAttribPointer(2, 4, gl.FLOAT, false, stride, 0);
    gl.vertexAttribDivisor(2, 1);
    gl.enableVertexAttribArray(3);
    gl.vertexAttribPointer(3, 2, gl.FLOAT, false, stride, 16);
    gl.vertexAttribDivisor(3, 1);
    gl.enableVertexAttribArray(4);
    gl.vertexAttribPointer(4, 3, gl.FLOAT, false, stride, 24);
    gl.vertexAttribDivisor(4, 1);
    gl.bindVertexArray(null);
    return vao;
  }

  /** The floor and the tiles: uploaded once. */
  setStatic(data: Float32Array, count: number) {
    const gl = this.gl;
    gl.bindBuffer(gl.ARRAY_BUFFER, this.bufStatic);
    gl.bufferData(gl.ARRAY_BUFFER, data.subarray(0, count * FLOATS_PER_INSTANCE), gl.STATIC_DRAW);
    this.nStatic = count;
    this.requestDraw();
  }

  /** The data: re-uploaded whenever the scene changes. */
  setDynamic(data: Float32Array, count: number) {
    const gl = this.gl;
    gl.bindBuffer(gl.ARRAY_BUFFER, this.bufDynamic);
    const view = data.subarray(0, count * FLOATS_PER_INSTANCE);
    if (count > this.dynamicCap) {
      gl.bufferData(gl.ARRAY_BUFFER, view, gl.DYNAMIC_DRAW);
      this.dynamicCap = count;
    } else gl.bufferSubData(gl.ARRAY_BUFFER, 0, view);
    this.nDynamic = count;
    this.dynamicLimit = -1;
    this.requestDraw();
  }

  /** Draw only the first `n` of the uploaded dynamic instances (or all if -1). */
  setDynamicLimit(n: number) {
    this.dynamicLimit = n;
    this.requestDraw();
  }

  /** Dim every dynamic instance whose base is below `y` by `factor`. */
  setDimBelow(y: number, factor: number) {
    this.dimBelow = y;
    this.dim = factor;
    this.requestDraw();
  }

  resetView() {
    this.cam.target = [...this.home.target];
    this.cam.yaw = this.home.yaw;
    this.cam.pitch = this.home.pitch;
    this.cam.dist = this.home.dist;
    this.requestDraw();
    this.onChange?.();
  }

  /** Look straight down (the 2D view, in 3D). */
  topDown() {
    this.cam.pitch = (89 * Math.PI) / 180;
    this.cam.yaw = 0;
    this.requestDraw();
    this.onChange?.();
  }

  requestDraw() {
    if (this.raf) return;
    this.raf = requestAnimationFrame(() => {
      this.raf = 0;
      this.draw();
    });
  }

  private eye(): number[] {
    const { target, yaw, pitch, dist } = this.cam;
    return [target[0] + dist * Math.sin(yaw) * Math.cos(pitch), target[1] + dist * Math.sin(pitch), target[2] + dist * Math.cos(yaw) * Math.cos(pitch)];
  }

  private draw() {
    const gl = this.gl;
    const c = this.canvas;
    const dpr = Math.min(2, window.devicePixelRatio || 1);
    const w = Math.max(1, Math.round(c.clientWidth * dpr));
    const h = Math.max(1, Math.round(c.clientHeight * dpr));
    if (c.width !== w || c.height !== h) {
      c.width = w;
      c.height = h;
    }
    gl.viewport(0, 0, w, h);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    const far = this.cam.dist * 4 + 400;
    const proj = perspective((38 * Math.PI) / 180, w / h, 1, far);
    const view = lookAt(this.eye(), this.cam.target, [0, 1, 0]);
    gl.useProgram(this.prog);
    gl.uniformMatrix4fv(this.uVP, false, mul(proj, view));
    gl.uniform1f(this.uDimBelow, -1e9);
    gl.uniform1f(this.uDim, 1);
    if (this.nStatic) {
      gl.bindVertexArray(this.vaoStatic);
      gl.drawArraysInstanced(gl.TRIANGLES, 0, 36, this.nStatic);
    }
    const n = this.dynamicLimit < 0 ? this.nDynamic : Math.min(this.nDynamic, this.dynamicLimit);
    if (n) {
      gl.uniform1f(this.uDimBelow, this.dimBelow);
      gl.uniform1f(this.uDim, this.dim);
      gl.bindVertexArray(this.vaoDynamic);
      gl.drawArraysInstanced(gl.TRIANGLES, 0, 36, n);
    }
    gl.bindVertexArray(null);
  }

  // ---- gestures ---------------------------------------------------------------
  private gestures() {
    const c = this.canvas;
    const cam = this.cam;
    const pts = new Map<number, { x: number; y: number }>();
    let lastTap = 0;
    let moved = false;
    const clampPitch = (p: number) => Math.max((8 * Math.PI) / 180, Math.min((89 * Math.PI) / 180, p));
    const clampDist = (d: number) => Math.max(12, Math.min(Math.max(this.opts.width, this.opts.height) * 6, d));
    /** World units per CSS pixel at the target's depth. */
    const unitsPerPx = () => (2 * cam.dist * Math.tan((38 * Math.PI) / 360)) / Math.max(1, c.clientHeight);
    const pan = (dx: number, dy: number) => {
      const k = unitsPerPx();
      const sy = Math.sin(cam.yaw);
      const cy = Math.cos(cam.yaw);
      // Right on screen is +right; down on screen is toward the camera on the ground.
      const fore = 1 / Math.max(0.15, Math.sin(cam.pitch));
      cam.target[0] -= dx * k * cy - dy * k * fore * sy;
      cam.target[2] -= -dx * k * sy - dy * k * fore * cy;
    };
    const orbit = (dx: number, dy: number) => {
      cam.yaw -= dx * 0.008;
      cam.pitch = clampPitch(cam.pitch + dy * 0.008);
    };
    c.addEventListener("pointerdown", (ev) => {
      c.setPointerCapture(ev.pointerId);
      pts.set(ev.pointerId, { x: ev.clientX, y: ev.clientY });
      if (pts.size === 1) moved = false;
      ev.preventDefault();
    });
    c.addEventListener("pointermove", (ev) => {
      const prev = pts.get(ev.pointerId);
      if (!prev) return;
      const cur = { x: ev.clientX, y: ev.clientY };
      if (Math.abs(cur.x - prev.x) + Math.abs(cur.y - prev.y) > 3) moved = true;
      if (pts.size === 1) {
        const dx = cur.x - prev.x;
        const dy = cur.y - prev.y;
        if (ev.shiftKey || (ev.buttons & 2) === 2) pan(dx, dy);
        else orbit(dx, dy);
      } else if (pts.size === 2) {
        const [ia, ib] = [...pts.keys()];
        const a0 = pts.get(ia)!;
        const b0 = pts.get(ib)!;
        const a1 = ia === ev.pointerId ? cur : a0;
        const b1 = ib === ev.pointerId ? cur : b0;
        const d0 = Math.hypot(a0.x - b0.x, a0.y - b0.y) || 1;
        const d1 = Math.hypot(a1.x - b1.x, a1.y - b1.y) || 1;
        cam.dist = clampDist(cam.dist * (d0 / d1));
        pan((a1.x + b1.x - a0.x - b0.x) / 2, (a1.y + b1.y - a0.y - b0.y) / 2);
      }
      pts.set(ev.pointerId, cur);
      this.requestDraw();
      this.onChange?.();
    });
    const up = (ev: PointerEvent) => {
      if (!pts.has(ev.pointerId)) return;
      pts.delete(ev.pointerId);
      if (c.hasPointerCapture(ev.pointerId)) c.releasePointerCapture(ev.pointerId);
      if (pts.size === 0 && !moved) {
        const now = performance.now();
        if (now - lastTap < 320) {
          this.resetView();
          lastTap = 0;
        } else lastTap = now;
      }
    };
    c.addEventListener("pointerup", up);
    c.addEventListener("pointercancel", up);
    c.addEventListener("contextmenu", (ev) => ev.preventDefault());
    c.addEventListener(
      "wheel",
      (ev) => {
        ev.preventDefault();
        cam.dist = clampDist(cam.dist * Math.exp(ev.deltaY * 0.0012));
        this.requestDraw();
        this.onChange?.();
      },
      { passive: false },
    );
  }
}

/** A growable instance list. */
export class Instances {
  data: Float32Array;
  count = 0;
  constructor(cap = 4096) {
    this.data = new Float32Array(cap * FLOATS_PER_INSTANCE);
  }
  reset() {
    this.count = 0;
  }
  push(cx: number, cz: number, y0: number, h: number, sx: number, sz: number, r: number, g: number, b: number) {
    let o = this.count * FLOATS_PER_INSTANCE;
    if (o + FLOATS_PER_INSTANCE > this.data.length) {
      const bigger = new Float32Array(this.data.length * 2);
      bigger.set(this.data);
      this.data = bigger;
    }
    const d = this.data;
    d[o++] = cx; d[o++] = cz; d[o++] = y0; d[o++] = h; d[o++] = sx; d[o++] = sz; d[o++] = r; d[o++] = g; d[o++] = b;
    this.count++;
  }
}
