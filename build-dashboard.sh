#!/bin/bash
set -e

# Auto-load environment variables from .Renviron if not already set
if [[ -f ".Renviron" ]]; then
  echo "🔧 Loading environment variables from .Renviron..."
  line_num=0
  while IFS='=' read -r key value; do
    ((line_num++))
    echo "  DEBUG line $line_num: key=<$key> value=<$value>"

    [[ -z "$key" || "$key" =~ ^#.*$ ]] && continue

    # Remove both double and single quotes from start/end
    value=$(echo "$value" | sed "s/^[\"']//;s/[\"']$//")

    # Only export if not already set
    if [[ -z "${!key}" ]]; then
      export "$key=$value"
      echo "  ✓ Exported $key"
    else
      echo "  - Skipped $key (already set)"
    fi
  done < .Renviron
fi

IMAGE_NAME="abmdash"
PASSWORD="${STATICRYPT_PASSWORD:-}"

# Debug: Check if ABS credentials are loaded
echo "🔍 Checking environment variables..."
if [[ -n "$ABS_USERNAME" ]]; then
  echo "  ✓ ABS_USERNAME is set"
else
  echo "  ✗ ABS_USERNAME is NOT set"
fi
if [[ -n "$ABS_PASSWORD" ]]; then
  echo "  ✓ ABS_PASSWORD is set"
else
  echo "  ✗ ABS_PASSWORD is NOT set"
fi

echo "🔨 Building Docker image..."
# Use buildx with cache to persist across Docker restarts
docker buildx build --platform linux/amd64 \
  --cache-from type=local,src=/tmp/docker-cache-abmdash \
  --cache-to type=local,dest=/tmp/docker-cache-abmdash,mode=max \
  -t "$IMAGE_NAME" \
  --load \
  .

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
  -e ABS_USERNAME="$ABS_USERNAME" \
  -e ABS_PASSWORD="$ABS_PASSWORD" \
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

      cat('=== LOCAL COOKIE FILE TEST ===\n');
      session <- abmdash::abs_login(check_connection = FALSE);
      cookie_file <- file.path(tempdir(), 'abs_session_cookies.txt');
      if (file.exists(cookie_file)) {
        cat('Cookie file exists, size:', file.size(cookie_file), 'bytes\n');
        contents <- readLines(cookie_file, warn = FALSE);
        cat('Cookie file contents:\n');
        cat(contents, sep='\n');
        cat('\nNumber of cookie lines (non-comment):', sum(!grepl('^#', contents)), '\n');
      } else {
        cat('Cookie file does NOT exist\n');
      }
      cat('==============================\n\n');

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
