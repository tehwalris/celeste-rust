# Hosting a web UI from this session (visible on Philippe's phone)

Philippe reaches session UIs through `https://taxw-ux.porgy-vimba.ts.net/` —
a Vite dev server that proxies path prefixes to local ports. This session has
been allocated:

    prefix:  /celeste        (wired in ux-exploration/divetree-demo/vite.config.ts)
    port:    localhost:3011

## Rules

- Serve plain HTTP on **port 3011**, and serve everything **under the `/celeste/`
  path prefix** — the prefix is passed through unchanged. Use relative asset
  URLs or a `/celeste/` base path. WebSockets under the prefix work too.
- Run the server detached and under a memory cap, e.g.:

      systemd-run --user --scope -p MemoryMax=2G --quiet <your serve command> &

  (One runaway process in the shared tmux scope has previously taken down every
  server and every Claude session on this machine. Never serve uncapped.)
- Do not take other ports; 3001–3004 and 3010 are taken (conversation server,
  transcription, bridge, VAD, Perch).
- A "Celeste" button appears automatically in the main chat UI header whenever
  this session is running — it links to `/celeste/`. Nothing to register.
- Tell Philippe the URL is `https://taxw-ux.porgy-vimba.ts.net/celeste/`.

Anything fancier (extra ports, server-side APIs) — ask the UX session.
