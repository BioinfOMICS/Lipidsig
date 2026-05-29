# ─────────────────────────────────────────────────────────────
#  LipidSig 2.0  –  Docker image
#  Base: rocker/shiny (R 4.4, Shiny Server pre-installed)
# ─────────────────────────────────────────────────────────────
FROM --platform=linux/amd64 rocker/shiny:4.4.1

# ── 1. System libraries ───────────────────────────────────────
RUN apt-get update && apt-get install -y --no-install-recommends \
    # SSL / curl (httr, curl, openssl)
    libssl-dev \
    libcurl4-openssl-dev \
    # XML / tidyverse
    libxml2-dev \
    # git (remotes / devtools GitHub installs)
    libgit2-dev \
    # igraph
    libglpk-dev \
    libgmp-dev \
    # GSL (some ML packages)
    libgsl-dev \
    # text rendering (ggplot2 / systemfonts)
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    # Java (topGO / org.* annotation packages)
    default-jdk \
    # misc build tools
    build-essential \
    && R CMD javareconf \
    && rm -rf /var/lib/apt/lists/*

# ── 2. Core R infrastructure (remotes + BiocManager) ─────────
RUN Rscript -e "install.packages(c('remotes', 'BiocManager'), repos='https://cloud.r-project.org')"

# ── 3. CRAN packages ──────────────────────────────────────────
#    Split into logical groups so Docker can cache each layer
#    independently; only the changed group needs rebuilding.

# Shiny UI / server framework
RUN Rscript -e "install.packages(c(\
  'shiny', 'shinythemes', 'shinyjs', 'shinyWidgets', \
  'shinycssloaders', 'shinyhelper' \
), repos='https://cloud.r-project.org')"

# Data wrangling
RUN Rscript -e "install.packages(c(\
  'magrittr', 'data.table', 'tidyr', 'dplyr', 'purrr', \
  'tibble', 'readr', 'readxl', 'stringr', 'stringi', 'bit64' \
), repos='https://cloud.r-project.org')"

# Visualisation
RUN Rscript -e "install.packages(c(\
  'ggplot2', 'plotly', 'iheatmapr', 'visNetwork', \
  'RColorBrewer', 'scales', 'wordcloud', 'hwordcloud' \
), repos='https://cloud.r-project.org')"

# Tables, downloads, misc
RUN Rscript -e "install.packages(c(\
  'DT', 'htmltools', 'zip', 'igraph' \
), repos='https://cloud.r-project.org')"

# Machine learning
RUN Rscript -e "install.packages('SHAPforxgboost', repos='https://cloud.r-project.org')"

# ── 4. Bioconductor packages ──────────────────────────────────
RUN Rscript -e "BiocManager::install(c(\
  'SummarizedExperiment', \
  'S4Vectors', \
  'topGO', \
  'org.Hs.eg.db', \
  'rgoslin' \
), ask = FALSE, update = FALSE)"

# ── 5. GitHub packages ────────────────────────────────────────

# LipidSigR – the core analysis engine
RUN Rscript -e "remotes::install_github('TMSWCChenglab/LipidSigR', upgrade = 'never')"

# ── 5b. Pin xfun to a version compatible with shinycssloaders ──
#   xfun >= 0.45 removed `attr` from its exports, breaking shinycssloaders.
#   This step runs AFTER all other packages so later installs cannot
#   silently upgrade xfun again.
RUN Rscript -e "remotes::install_version('xfun', version='0.44', repos='https://cloud.r-project.org')"

# ── 6. Copy application files ─────────────────────────────────
RUN mkdir -p /srv/shiny-server/lipidsig
COPY . /srv/shiny-server/lipidsig/

# ── 7. Shiny Server configuration ────────────────────────────
#    Override the default config so /lipidsig maps to the app.
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# ── 8. Permissions & expose ───────────────────────────────────
RUN chown -R shiny:shiny /srv/shiny-server/lipidsig

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
