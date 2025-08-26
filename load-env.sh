#!/bin/bash

# Load environment variables from .Renviron for local testing
# Usage: source ./load-env.sh

if [[ ! -f ".Renviron" ]]; then
    echo "❌ .Renviron file not found"
    return 1 2>/dev/null || exit 1
fi

echo "🔧 Loading environment variables from .Renviron..."

# Export each variable from .Renviron
while IFS='=' read -r key value; do
    # Skip empty lines and comments
    [[ -z "$key" || "$key" =~ ^#.*$ ]] && continue
    
    # Remove quotes if present and export
    value=$(echo "$value" | sed 's/^"//;s/"$//')
    export "$key=$value"
    echo "   ✅ $key"
done < .Renviron

echo "🚀 Environment variables loaded. You can now run R scripts locally."