#!/usr/bin/env python3
"""Simple HTTP server with gzip compression for JSON files."""

import gzip
import http.server
import os
import socketserver

class GzipHandler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        # Add CORS headers for local development
        self.send_header('Access-Control-Allow-Origin', '*')
        super().end_headers()

    def do_GET(self):
        # Check if client accepts gzip
        accept_encoding = self.headers.get('Accept-Encoding', '')
        if 'gzip' not in accept_encoding:
            return super().do_GET()

        # Get the file path
        path = self.translate_path(self.path)

        # Only compress JSON files
        if not path.endswith('.json') or not os.path.isfile(path):
            return super().do_GET()

        try:
            with open(path, 'rb') as f:
                content = f.read()

            # Compress the content
            compressed = gzip.compress(content)

            self.send_response(200)
            self.send_header('Content-Type', 'application/json')
            self.send_header('Content-Encoding', 'gzip')
            self.send_header('Content-Length', len(compressed))
            self.end_headers()
            self.wfile.write(compressed)
        except Exception as e:
            self.send_error(500, str(e))

if __name__ == '__main__':
    PORT = 8000
    with socketserver.TCPServer(("", PORT), GzipHandler) as httpd:
        print(f"Serving at http://localhost:{PORT}/tree_viewer.html")
        print("JSON files are served with gzip compression")
        httpd.serve_forever()
