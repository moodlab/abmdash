FROM rocker/r-ver:4.4.2

# Install system dependencies
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

# Install Node.js for staticrypt
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - \
    && apt-get install -y nodejs

# Install Quarto
RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.4.550/quarto-1.4.550-linux-amd64.deb \
    && dpkg -i quarto-1.4.550-linux-amd64.deb \
    && rm quarto-1.4.550-linux-amd64.deb

# Install staticrypt globally
RUN npm install -g staticrypt

# Set working directory
WORKDIR /app

# Install renv globally
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# Copy renv infrastructure first
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile  
COPY renv/ renv/

# Restore packages using existing renv setup
RUN R -s -e "renv::restore()"

# Copy rest of project and install package
COPY . .
RUN R -s -e "renv::install('.')"

# Verify the package was installed
RUN R -s -e "library(abmdash); cat('abmdash package loaded successfully\n')"

# Set the correct renv library path as environment variable
ENV R_LIBS_USER="/app/renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu"
ENV R_LIBS="/app/renv/library/linux-ubuntu-noble/R-4.4/x86_64-pc-linux-gnu:/usr/local/lib/R/site-library:/usr/local/lib/R/library"

# Set environment variable for password (can be overridden at runtime)
ENV STATICRYPT_PASSWORD=""

# Create output directory
RUN mkdir -p docs

# Default command
CMD ["bash"]
