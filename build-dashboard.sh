#!/bin/bash

# Build and run ABM Dashboard in Docker
set -e

# Configuration
IMAGE_NAME="abmdash"
CONTAINER_NAME="abmdash-build"
PASSWORD="${STATICRYPT_PASSWORD:-}"

echo "Building Docker image..."
docker build -t "$IMAGE_NAME" . --progress=plain

echo "Creating dashboard..."
docker run --rm \
    --name "$CONTAINER_NAME" \
    -v "$(pwd)/docs:/app/docs" \
    -e STATICRYPT_PASSWORD="$PASSWORD" \
    -e R_LIBS_USER="/app/renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu" \
    -e R_LIBS="/app/renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu:/usr/local/lib/R/site-library:/usr/local/lib/R/library" \
    "$IMAGE_NAME" \
    bash -c "cd inst/dashboard && quarto render index.qmd --output-dir /tmp/dashboard && cp -r /tmp/dashboard/* /app/docs/ && if [ -n \"\$STATICRYPT_PASSWORD\" ]; then echo 'Encrypting dashboard files...' && cd /app/docs && echo 'Files in /app/docs before encryption:' && ls -la && echo 'Running staticrypt command...' && npx staticrypt ./*.html -r -d . -p \"\$STATICRYPT_PASSWORD\" --short --template-color-primary '#6667AB' --template-color-secondary '#f9f9f3' --template-title 'MDL R01 GABM Dashboard' --template-instructions 'Enter the Password' --template-button 'Access' && echo 'Files in /app/docs after encryption:' && ls -la; else echo 'No password set, skipping encryption'; fi"

echo "Dashboard created successfully in ./docs/"

if [ -n "$PASSWORD" ]; then
    echo "Dashboard has been encrypted with provided password."
else
    echo "Warning: No STATICRYPT_PASSWORD provided. Dashboard is not encrypted."
fi

echo "To serve the dashboard locally, run:"
echo "  python3 -m http.server 8000 --directory docs"
echo "  # or"
echo "  cd docs && python3 -m http.server 8000"
