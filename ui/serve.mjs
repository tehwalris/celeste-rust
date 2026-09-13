// Static server for the built UI: `dist/` under the /celeste/ prefix on
// port 3011 (UI-HOSTING.md), the exported run data from DATA_DIR under
// /celeste/data/. Gzip for the text and binary payloads; no cache on the
// data so a re-export shows up on reload.
//
//   systemd-run --user --scope -p MemoryMax=2G --quiet node serve.mjs &

import http from "node:http";
import fs from "node:fs";
import path from "node:path";
import zlib from "node:zlib";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const DIST = path.join(here, "dist");
const DATA_DIR = process.env.DATA_DIR || "/var/tmp/celeste-ui/data";
const PORT = Number(process.env.PORT || 3011);
const PREFIX = "/celeste/";

const MIME = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".json": "application/json",
  ".bin": "application/octet-stream",
  ".svg": "image/svg+xml",
  ".png": "image/png",
  ".ico": "image/x-icon",
  ".map": "application/json",
};

function send(req, res, file, cacheable) {
  fs.stat(file, (err, st) => {
    if (err || !st.isFile()) {
      res.writeHead(404, { "content-type": "text/plain" });
      res.end("not found");
      return;
    }
    const ext = path.extname(file);
    const headers = {
      "content-type": MIME[ext] || "application/octet-stream",
      "cache-control": cacheable ? "public, max-age=31536000, immutable" : "no-cache",
    };
    const gz = /\bgzip\b/.test(req.headers["accept-encoding"] || "") && (ext === ".json" || ext === ".bin" || ext === ".js" || ext === ".css" || ext === ".html");
    if (gz) {
      headers["content-encoding"] = "gzip";
      headers["vary"] = "accept-encoding";
      res.writeHead(200, headers);
      fs.createReadStream(file).pipe(zlib.createGzip({ level: 6 })).pipe(res);
    } else {
      headers["content-length"] = st.size;
      res.writeHead(200, headers);
      fs.createReadStream(file).pipe(res);
    }
  });
}

const server = http.createServer((req, res) => {
  const url = new URL(req.url, "http://localhost");
  let p = decodeURIComponent(url.pathname);
  if (p === "/celeste") {
    res.writeHead(302, { location: PREFIX });
    res.end();
    return;
  }
  if (!p.startsWith(PREFIX)) {
    res.writeHead(404, { "content-type": "text/plain" });
    res.end("not under " + PREFIX);
    return;
  }
  p = p.slice(PREFIX.length);
  if (p.includes("..")) {
    res.writeHead(400);
    res.end();
    return;
  }
  if (p.startsWith("data/")) {
    send(req, res, path.join(DATA_DIR, p.slice(5)), false);
    return;
  }
  if (p === "" || p.endsWith("/")) p += "index.html";
  const file = path.join(DIST, p);
  // Hashed assets are immutable; index.html is not.
  send(req, res, file, p.startsWith("assets/"));
});

server.listen(PORT, "127.0.0.1", () => {
  console.log(`celeste ui: http://127.0.0.1:${PORT}${PREFIX} (dist ${DIST}, data ${DATA_DIR})`);
});
