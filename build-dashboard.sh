#!/bin/bash
set -e

IMAGE_NAME="abmdash"
PASSWORD="${STATICRYPT_PASSWORD:-}"

echo "🔨 Building Docker image..."
docker build --platform linux/amd64 -t "$IMAGE_NAME" .

echo "📁 Preparing output directory..."
rm -rf docs
mkdir -p docs

echo "🚀 Rendering dashboard..."
docker run --rm \
  --platform linux/amd64 \
  -v "$(pwd)/docs:/project/docs" \
  -e STATICRYPT_PASSWORD="$PASSWORD" \
  -w /project \
  "$IMAGE_NAME" \
  bash -c "
    set -e
    echo '🔧 Using prebuilt renv library...'
    Rscript -e \"
      lib <- Sys.glob('/project/renv/library/project-*/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu');
      .libPaths(c(lib, .libPaths()));
      source('/project/renv/activate.R');
      library(quarto);
      setwd('/tmp');
      file.copy('/project/inst/dashboard/index.qmd', '/tmp/index.qmd', overwrite = TRUE);
      file.copy('/project/inst/dashboard/_quarto.yml', '/tmp/_quarto.yml', overwrite = TRUE);
      if (dir.exists('/project/inst/dashboard/.quarto')) {
        file.copy('/project/inst/dashboard/.quarto', '/tmp/', recursive = TRUE);
      }
      quarto::quarto_render('/tmp/index.qmd');
      file.copy('/tmp/index.html', '/project/docs/index.html', overwrite = TRUE);
      if (dir.exists('/tmp/site_libs')) {
        file.copy('/tmp/site_libs', '/project/docs/', recursive = TRUE);
      }
    \"

    if [ -n \"\$STATICRYPT_PASSWORD\" ]; then
      echo '🔒 Encrypting output...'
      cd /project/docs
      for html in *.html; do
        [ -f \"\$html\" ] || continue
        staticrypt \"\$html\" \
          -p \"\$STATICRYPT_PASSWORD\" \
          --short \
          --remember 30 \
          --template-color-primary \"#6667AB\" \
          --template-color-secondary \"#f9f9f3\" \
          --template-title \"MDL R01 GABM Dashboard\" \
          --template-instructions \"Enter the Password\" \
          --template-button \"Access\"
      done
    fi
  "

if [ -f "docs/index.html" ]; then
  echo "✅ Dashboard created successfully in ./docs/"
  [ -n "$PASSWORD" ] && echo "🔒 Dashboard is encrypted" || echo "⚠️  No encryption (set STATICRYPT_PASSWORD to encrypt)"
  echo ""
  echo "📡 To view locally: cd docs && python3 -m http.server 8000"
else
  echo "❌ ERROR: Dashboard was not created"
  exit 1
fi
