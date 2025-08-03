#!/bin/bash
# Debug script to check what's happening with renv
set -e

IMAGE_NAME="abmdash"

echo "🔍 Debugging renv library transfer..."
docker run --rm \
  --platform linux/amd64 \
  "$IMAGE_NAME" \
  bash -c '
    echo "=== Working directory ==="
    pwd
    
    echo -e "\n=== Check .Rprofile ==="
    cat .Rprofile
    
    echo -e "\n=== Check renv directory structure ==="
    find renv -type f -name "*.R" | head -10
    
    echo -e "\n=== Check if packages exist in renv library ==="
    find renv/library -name "quarto" -o -name "rmarkdown" | head -10
    
    echo -e "\n=== Check renv library hash ==="
    ls -la renv/library/
    
    echo -e "\n=== Try to use renv without bootstrapping ==="
    R --vanilla -e "renv::status()" 2>&1 | head -20
    
    echo -e "\n=== Check RENV environment variables ==="
    env | grep RENV || echo "No RENV variables set"
  '