import { defineConfig } from "vite";

// Served under https://taxw-ux.porgy-vimba.ts.net/celeste/ (UI-HOSTING.md):
// every asset URL is relative to /celeste/.
export default defineConfig({
  base: "/celeste/",
  // public/data is a symlink to the exported run; the dev server serves it,
  // but the build must not copy 9 MB of it into dist - serve.mjs reads the
  // data directory directly.
  build: { outDir: "dist", assetsDir: "assets", sourcemap: false, target: "es2022", copyPublicDir: false },
  server: { port: 3011, strictPort: true },
});
