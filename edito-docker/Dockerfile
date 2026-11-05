# Carpediem V2, batch image for the EDITO process catalogue
#
# Build from the repository root (branch v2-engineering, based on carpediem_v2):
#   docker build -f edito-docker/Dockerfile -t carpediem:dev .
# (the build context is the repository root; edito-docker/Dockerfile.dockerignore keeps the
# study data, documentation and migration scripts out of it)
#
# Run (study folder or archive given by CARPEDIEM_STUDY, outputs in ./out):
#   docker run --rm -v /path/to/study:/study:ro -v "$PWD/out:/output" -e CARPEDIEM_STUDY=/study carpediem:dev
#
# See edito-docker/docker/README.md for the environment variables.

ARG BASE_IMAGE=rocker/r-ver:4.5.1
FROM ${BASE_IMAGE}

# System libraries needed at runtime by the binary R packages of renv.lock
# (sf / terra / s2: GDAL, GEOS, PROJ, udunits; RPostgres: libpq; ragg / systemfonts: fonts)
RUN apt-get update && apt-get install -y --no-install-recommends \
        libgdal-dev libgeos-dev libproj-dev libudunits2-dev \
        libpq-dev libssl-dev libcurl4-openssl-dev libxml2-dev \
        libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
        libpng-dev libjpeg-dev libtiff5-dev libwebp-dev \
        pandoc ca-certificates curl \
        rclone \
    && rm -rf /var/lib/apt/lists/*
# rclone is used to read inputs from / write outputs to EDITO S3 storage (MinIO)

WORKDIR /opt/carpediem

# R packages, restored from renv.lock at their locked versions (layer cached as long as
# renv.lock does not change). Binaries come from Posit Package Manager for Ubuntu noble.
# (renv cache disabled: packages are installed directly in the project library)
ENV RENV_PATHS_LIBRARY=/opt/renv/library \
    RENV_CONFIG_CACHE_ENABLED=FALSE \
    RENV_CONFIG_REPOS_OVERRIDE=https://p3m.dev/cran/__linux__/noble/latest \
    RENV_CONFIG_PAK_ENABLED=FALSE
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/settings.json renv/
RUN R -q -e 'renv::restore(prompt = FALSE)' \
    && rm -rf /tmp/Rtmp* /tmp/renv*

# Application code (no study data in the image: the study is read at run time, see
# CARPEDIEM_STUDY)
COPY carpediem_v2.Rproj ./
COPY engine/ engine/
COPY execution/ execution/
COPY LICENSE ./
COPY edito-docker/docker/entrypoint.sh /usr/local/bin/carpediem-entrypoint
COPY edito-docker/docker/run_manifest.R /usr/local/lib/carpediem/run_manifest.R
# Explicit modes: the files may come from a checkout with restrictive permissions (e.g. 600),
# and the container runs as uid 1000, which must be able to read and execute them.
RUN chmod 0755 /usr/local/bin/carpediem-entrypoint /usr/local/lib/carpediem \
    && chmod 0644 /usr/local/lib/carpediem/run_manifest.R \
    && mkdir -p /output \
    && chmod -R a+rwX /opt/carpediem /output

# Defaults (see docker/README.md)
# Version recorded in run.json (set with --build-arg, see docker/README.md)
ARG CARPEDIEM_VERSION=dev
ARG CARPEDIEM_COMMIT=unknown
ENV CARPEDIEM_VERSION=${CARPEDIEM_VERSION} \
    CARPEDIEM_COMMIT=${CARPEDIEM_COMMIT} \
    CARPEDIEM_CONFIG=config/config.yml \
    HOME=/tmp

# Non root user (uid 1000), as usually required on Kubernetes platforms
USER 1000
ENTRYPOINT ["/usr/local/bin/carpediem-entrypoint"]
