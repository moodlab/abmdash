# Stage 1: Base image with renv restoration
FROM rocker/r-ver:4.5.1 AS base

# Install system dependencies needed for packages
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    pandoc \
    git \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Set up project directory
WORKDIR /project

# Copy renv metadata
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
RUN mkdir -p renv
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Disable renv cache and force project-relative paths
ENV RENV_PATHS_CACHE=FALSE
ENV RENV_PATHS_LIBRARY=/project/renv/library
ENV RENV_CONFIG_CACHE_ENABLED=FALSE

# Restore renv packages
RUN R -s -e "renv::restore()"

# Stage 2: Runtime image
FROM rocker/r-ver:4.5.1

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    pandoc \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Node.js and staticrypt
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - && \
    apt-get install -y nodejs && \
    npm install -g staticrypt && \
    rm -rf /var/lib/apt/lists/*

# Install Quarto CLI
RUN mkdir -p /opt && \
    wget -q https://github.com/quarto-dev/quarto-cli/releases/download/v1.4.550/quarto-1.4.550-linux-amd64.tar.gz && \
    tar xzf quarto-1.4.550-linux-amd64.tar.gz -C /opt && \
    rm quarto-1.4.550-linux-amd64.tar.gz && \
    ln -s /opt/quarto-1.4.550/bin/quarto /usr/local/bin/quarto

WORKDIR /project

# Copy entire project from base stage (including renv library and metadata)
COPY --from=base /project .

ENV R_LIBS_SITE=/project/renv/library

# Copy application files (do not overwrite renv library)
COPY --chmod=755 DESCRIPTION ./
COPY --chmod=755 NAMESPACE ./
COPY --chmod=755 R ./R
COPY --chmod=755 inst ./inst

# Local package will be installed at runtime

# Environment variables to enforce use of restored renv
ENV RENV_PATHS_CACHE=FALSE
ENV RENV_PATHS_LIBRARY=/project/renv/library
ENV RENV_PATHS_LIBRARY_ROOT=/project/renv/library
ENV RENV_CONFIG_CACHE_ENABLED=FALSE
ENV STATICRYPT_PASSWORD=""
ENV REDCAP_API_TOKEN=""
ENV GOOGLE_SERVICE_ACCOUNT_JSON=""

# Create output directory
RUN mkdir -p docs && chmod 755 docs

LABEL cache_bust="2026-02-02-v4"  

CMD ["bash"]
