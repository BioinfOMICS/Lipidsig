#!/bin/bash
# docker-entrypoint.sh
# Starts shiny-server, then immediately warms up the app so the R process
# is fully initialised before the first real user arrives.

set -e

# Start shiny-server in the background
/usr/bin/shiny-server &
SHINY_PID=$!

# Wait until the HTTP server is accepting connections (max 120 s)
echo "[entrypoint] Waiting for shiny-server to start..."
for i in $(seq 1 60); do
  if curl -sf http://localhost:3838/ -o /dev/null 2>/dev/null; then
    echo "[entrypoint] Server is up. Sending warmup request..."
    # A second request actually triggers the app R process to load global.R
    curl -sf http://localhost:3838/ -o /dev/null 2>/dev/null || true
    echo "[entrypoint] Warmup complete."
    break
  fi
  sleep 2
done

# Hand control back to shiny-server (foreground)
wait $SHINY_PID
