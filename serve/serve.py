#!/usr/bin/env python3
"""
Compressed HTTP Server with Zstandard support.

A simple HTTP server that serves static files with transparent compression.
Supports Zstandard (zstd), gzip, and deflate based on client Accept-Encoding.

Usage:
    # Start the server (runs on port 8000 by default):
    ./serve.py

    # Start on a custom port:
    ./serve.py 9000

    # Run in background with nohup:
    nohup ./serve.py &

    # Stop the server:
    kill $(cat serve.pid)  # if PID file exists
    # or
    pkill -f "serve.py"

The server automatically:
- Compresses responses based on Accept-Encoding header
- Prefers zstd > gzip > deflate
- Skips compression for already-compressed files (images, etc.)
- Writes PID to serve.pid for easy process management
"""

import http.server
import socketserver
import zstandard
import gzip
import zlib
import os
import sys
from functools import lru_cache


# File extensions that are already compressed - skip compression
SKIP_COMPRESSION = {
    '.gz', '.zip', '.png', '.jpg', '.jpeg', '.gif', '.webp',
    '.mp3', '.mp4', '.webm', '.ogg', '.woff', '.woff2', '.br', '.zst'
}

# MIME types to compress
COMPRESSIBLE_TYPES = {
    'text/', 'application/json', 'application/javascript',
    'application/xml', 'application/xhtml+xml', 'image/svg+xml'
}


def should_compress(content_type, path):
    """Check if response should be compressed based on content type and path."""
    ext = os.path.splitext(path)[1].lower()
    if ext in SKIP_COMPRESSION:
        return False
    if not content_type:
        return False
    return any(ct in content_type for ct in COMPRESSIBLE_TYPES)


def get_preferred_encoding(accept_encoding):
    """Parse Accept-Encoding and return the best supported encoding."""
    if not accept_encoding:
        return None

    encodings = []
    for part in accept_encoding.split(','):
        part = part.strip()
        if ';' in part:
            enc, q = part.split(';', 1)
            enc = enc.strip()
            try:
                q = float(q.split('=')[1])
            except (IndexError, ValueError):
                q = 1.0
        else:
            enc = part
            q = 1.0
        encodings.append((enc, q))

    # Sort by quality, prefer zstd > gzip > deflate
    priority = {'zstd': 3, 'gzip': 2, 'deflate': 1}
    encodings.sort(key=lambda x: (-x[1], -priority.get(x[0], 0)))

    for enc, q in encodings:
        if q > 0 and enc in ('zstd', 'gzip', 'deflate'):
            return enc
    return None


class CompressingHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    """HTTP request handler with transparent compression support."""

    # Zstandard compressor (reused for efficiency)
    _zstd_compressor = zstandard.ZstdCompressor(level=3)

    def end_headers(self):
        # Add CORS headers for local development
        self.send_header('Access-Control-Allow-Origin', '*')
        super().end_headers()

    def do_GET(self):
        """Handle GET request with compression."""
        # Get the file path
        path = self.translate_path(self.path)

        # Check if file exists
        if not os.path.isfile(path):
            # Let parent handle directory listing or 404
            return super().do_GET()

        # Get content type
        ctype = self.guess_type(path)

        # Check if we should compress
        accept_encoding = self.headers.get('Accept-Encoding', '')
        encoding = get_preferred_encoding(accept_encoding)

        if not encoding or not should_compress(ctype, path):
            # No compression needed
            return super().do_GET()

        try:
            with open(path, 'rb') as f:
                content = f.read()
        except IOError:
            return super().do_GET()

        # Compress the content
        if encoding == 'zstd':
            compressed = self._zstd_compressor.compress(content)
        elif encoding == 'gzip':
            compressed = gzip.compress(content, compresslevel=6)
        elif encoding == 'deflate':
            compressed = zlib.compress(content, level=6)
        else:
            return super().do_GET()

        # Send response
        self.send_response(200)
        self.send_header('Content-Type', ctype)
        self.send_header('Content-Length', len(compressed))
        self.send_header('Content-Encoding', encoding)
        self.send_header('Vary', 'Accept-Encoding')
        self.end_headers()

        self.wfile.write(compressed)

    def log_message(self, format, *args):
        """Log with compression info."""
        # Add encoding info if available
        encoding = get_preferred_encoding(self.headers.get('Accept-Encoding', ''))
        enc_str = f" [{encoding}]" if encoding else ""
        print(f"{self.address_string()} - {format % args}{enc_str}")


class ThreadedHTTPServer(socketserver.ThreadingMixIn, socketserver.TCPServer):
    """Threaded HTTP server for handling concurrent requests."""
    allow_reuse_address = True
    daemon_threads = True


def main():
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8000

    # Write PID file
    pid_file = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'serve.pid')
    with open(pid_file, 'w') as f:
        f.write(str(os.getpid()))

    # Change to script directory to serve files from there
    os.chdir(os.path.dirname(os.path.abspath(__file__)))

    with ThreadedHTTPServer(("", port), CompressingHTTPRequestHandler) as httpd:
        print(f"Serving at http://localhost:{port} with compression (zstd/gzip/deflate)")
        print(f"PID {os.getpid()} written to {pid_file}")
        print("Press Ctrl+C to stop")
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\nShutting down...")
        finally:
            # Clean up PID file
            try:
                os.remove(pid_file)
            except:
                pass


if __name__ == '__main__':
    main()
