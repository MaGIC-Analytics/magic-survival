FROM rocker/shiny-verse:4.5.3
LABEL authors="Alex Lemenze" \
    description="Docker image for MaGIC Survival Analysis Tool"

# ── System dependencies ──────────────────────────────────────────────────────
RUN apt-get update && apt-get install -y \
    sudo \
    libhdf5-dev \
    build-essential \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libssl-dev \
    libv8-dev \
    libsodium-dev \
    libglpk40 \
    libpng-dev \
    libjpeg-dev \
    libtiff-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libnlopt-dev \
    cmake && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# ── CRAN packages (install deps that need system libs first) ─────────────────
RUN R -e "install.packages(c('nloptr', 'lme4', 'pbkrtest', 'car', 'rstatix'), \
    repos='https://cran.rstudio.com/', dependencies=TRUE)"

RUN R -e "install.packages(c( \
    'shiny', \
    'shinyjs', \
    'shinythemes', \
    'shinycssloaders', \
    'shinyWidgets', \
    'DT', \
    'tidyverse', \
    'data.table', \
    'RColorBrewer', \
    'colourpicker', \
    'survival', \
    'survminer', \
    'maxstat', \
    'ggpubr' \
    ), repos='https://cran.rstudio.com/', dependencies=TRUE)"

# ── Copy application files ───────────────────────────────────────────────────
COPY ./app /srv/shiny-server/
COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

# ── Permissions ──────────────────────────────────────────────────────────────
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 8080
USER shiny
CMD ["/usr/bin/shiny-server"]
