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
  ".mp4": "video/mp4",
  ".webm": "video/webm",
};

// `cache`: "immutable" for hashed assets, "revalidate" for the data (a
// re-export shows on reload; unchanged files answer 304 to the browser's
// If-Modified-Since, so a multi-MB binary is not re-sent), "none" for
// index.html (1 KB, and the one file whose staleness hides a new build).
function send(req, res, file, cache) {
  fs.stat(file, (err, st) => {
    if (err || !st.isFile()) {
      res.writeHead(404, { "content-type": "text/plain" });
      res.end("not found");
      return;
    }
    const ext = path.extname(file);
    const headers = {
      "content-type": MIME[ext] || "application/octet-stream",
      "cache-control": cache === "immutable" ? "public, max-age=31536000, immutable" : cache === "revalidate" ? "no-cache" : "no-store",
    };
    if (cache === "revalidate") {
      headers["last-modified"] = st.mtime.toUTCString();
      const since = req.headers["if-modified-since"];
      if (since && Math.floor(st.mtimeMs / 1000) <= Math.floor(Date.parse(since) / 1000)) {
        res.writeHead(304, headers);
        res.end();
        return;
      }
    }
    const gz = /\bgzip\b/.test(req.headers["accept-encoding"] || "") && (ext === ".json" || ext === ".bin" || ext === ".js" || ext === ".css" || ext === ".html");
    if (gz) {
      headers["content-encoding"] = "gzip";
      headers["vary"] = "accept-encoding";
      res.writeHead(200, headers);
      fs.createReadStream(file).pipe(zlib.createGzip({ level: 6 })).pipe(res);
    } else if (/^bytes=\d*-\d*$/.test(req.headers.range || "")) {
      // A byte range: what a <video> asks for (iOS Safari plays nothing
      // from a server that answers it with the whole file).
      const [a, b] = req.headers.range.slice(6).split("-");
      const start = a === "" ? Math.max(0, st.size - Number(b)) : Number(a);
      const end = a === "" || b === "" ? st.size - 1 : Math.min(Number(b), st.size - 1);
      if (start > end || start >= st.size) {
        res.writeHead(416, { "content-range": `bytes */${st.size}` });
        res.end();
        return;
      }
      headers["content-range"] = `bytes ${start}-${end}/${st.size}`;
      headers["content-length"] = end - start + 1;
      res.writeHead(206, headers);
      fs.createReadStream(file, { start, end }).pipe(res);
    } else {
      headers["accept-ranges"] = "bytes";
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
    send(req, res, path.join(DATA_DIR, p.slice(5)), "revalidate");
    return;
  }
  if (p === "" || p.endsWith("/")) p += "index.html";
  const file = path.join(DIST, p);
  // Hashed assets are immutable; index.html is never cached.
  send(req, res, file, p.startsWith("assets/") ? "immutable" : "none");
});

server.listen(PORT, "127.0.0.1", () => {
  console.log(`celeste ui: http://127.0.0.1:${PORT}${PREFIX} (dist ${DIST}, data ${DATA_DIR})`);
});
