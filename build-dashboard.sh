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
  -e REDCAP_API_TOKEN="$REDCAP_API_TOKEN" \
  -e GOOGLE_SERVICE_ACCOUNT_JSON="$GOOGLE_SERVICE_ACCOUNT_JSON" \
  -w /project \
  "$IMAGE_NAME" \
  bash -c "
    set -e
    echo '🔧 Using prebuilt renv library...'
    echo '=== DEBUGGING RUNTIME ENVIRONMENT ==='
    ls -la /project/renv/library/
    echo 'Files in renv lib subdirs:'
    find /project/renv/library -name '*abmdash*' || echo 'No abmdash found anywhere'
    echo 'Environment variables:'
    env | grep RENV
    echo '=== R SESSION DEBUG ==='
    Rscript -e \"
      source('/project/renv/activate.R');
      cat('Library paths:', .libPaths(), '\n');
      cat('Contents of main library:', list.files(.libPaths()[1]), '\n');
      cat('Looking for abmdash dir:', dir.exists(file.path(.libPaths()[1], 'abmdash')), '\n');
      cat('Looking for abmdash:', 'abmdash' %in% rownames(installed.packages()), '\n');
      # Always install abmdash to ensure it's available
      cat('Installing abmdash package...\n');
      install.packages('/project', repos=NULL, type='source', dependencies=FALSE);
      cat('abmdash installation completed\n');
      
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
        # Move encrypted file from encrypted/ subdirectory back to main directory
        if [ -f \"encrypted/\$html\" ]; then
          mv \"encrypted/\$html\" \"\$html\"
          rmdir encrypted 2>/dev/null || true
        fi
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
