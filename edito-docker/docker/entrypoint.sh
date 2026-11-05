#!/usr/bin/env bash
# Carpediem V2 container entrypoint
#
# Each run produces one folder, <CARPEDIEM_OUTPUT>/<run id>/, described in
# edito-docker/RUNS_LAYOUT.md:
#   run.json   description of the run (status, dates, parameters, files), written at the
#              start (status "running") and completed at the end
#   run.log    full log
#   study/     copy of the study folder used (config/ + inputs/)
#   outputs/   results of the pipeline (GeoPackages, maps, graphs, HTML reports...)
#
# Steps:
# 1. Fetch the study folder (config/ + inputs/) from a local path or from S3
# 2. Run execution/run_batch.R with the selected YAML config
# 3. Write the run folder to a local path or to S3 (also on failure)
#
# Environment variables (see docker/README.md):
#   CARPEDIEM_STUDY            (required) the study, with the layout config/ + inputs/:
#                              - a folder: local path or s3://bucket/prefix
#                              - or a .tar.gz / .tgz archive of that folder: local path,
#                                s3://bucket/key or https:// URL (e.g. a public file on EDITO)
#   CARPEDIEM_CONFIG           YAML config path, relative to the study folder
#                              (default: config/config.yml)
#   CARPEDIEM_OUTPUT           local folder or s3://bucket/prefix under which the run folder is
#                              created. Default: /output-data when that folder exists (EDITO
#                              processes created with add-your-process), otherwise /output
#   CARPEDIEM_RUN_DESCRIPTION  free text describing the run; stored in run.json and used, in
#                              short form, as the suffix of the run id
#   In CARPEDIEM_STUDY and CARPEDIEM_OUTPUT, the text ${AWS_BUCKET_NAME} is replaced by the
#   bucket of the user (variable injected by EDITO), e.g. s3://${AWS_BUCKET_NAME}/carpediem/runs
#   AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_SESSION_TOKEN, AWS_S3_ENDPOINT (host) or
#   S3_ENDPOINT (full URL), AWS_DEFAULT_REGION
#                              S3 credentials (injected by EDITO), only needed for s3:// locations
set -euo pipefail

APP_DIR=/opt/carpediem
TOOLS_DIR=/usr/local/lib/carpediem
STUDY_DIR="${APP_DIR}/study"
CARPEDIEM_STUDY="${CARPEDIEM_STUDY:-}"
CARPEDIEM_CONFIG="${CARPEDIEM_CONFIG:-config/config.yml}"
CARPEDIEM_RUN_DESCRIPTION="${CARPEDIEM_RUN_DESCRIPTION:-}"
if [[ -z "${CARPEDIEM_OUTPUT:-}" ]]; then
  if [[ -d /output-data ]]; then CARPEDIEM_OUTPUT=/output-data; else CARPEDIEM_OUTPUT=/output; fi
fi

START_TS=$(date +%s)
export RUN_STARTED_AT
RUN_STARTED_AT=$(date -u -d "@${START_TS}" +%Y-%m-%dT%H:%M:%SZ)

# ---- Helpers -------------------------------------------------------------------------------

# CPU and memory limits seen by the container (cgroup v2, else v1)
cpu_limit() {
  if [[ -r /sys/fs/cgroup/cpu.max ]]; then
    read -r q p < /sys/fs/cgroup/cpu.max
    [[ "$q" == max ]] && nproc || awk -v q="$q" -v p="$p" 'BEGIN { printf "%.1f", q / p }'
  elif [[ -r /sys/fs/cgroup/cpu/cpu.cfs_quota_us ]] && [[ $(cat /sys/fs/cgroup/cpu/cpu.cfs_quota_us) -gt 0 ]]; then
    awk -v q="$(cat /sys/fs/cgroup/cpu/cpu.cfs_quota_us)" -v p="$(cat /sys/fs/cgroup/cpu/cpu.cfs_period_us)" 'BEGIN { printf "%.1f", q / p }'
  else nproc; fi
}
# memory values in GiB, empty when unknown or unlimited
mem_limit_gib() {
  local f=/sys/fs/cgroup/memory.max
  [[ -r $f ]] || f=/sys/fs/cgroup/memory/memory.limit_in_bytes
  local v; v=$(cat "$f" 2>/dev/null || echo max)
  if [[ "$v" =~ ^[0-9]+$ && "$v" -lt 1000000000000 ]]; then awk -v v="$v" 'BEGIN { printf "%.2f", v / 1073741824 }'; fi
}
mem_peak_gib() {
  local f=/sys/fs/cgroup/memory.peak
  [[ -r $f ]] || f=/sys/fs/cgroup/memory/memory.max_usage_in_bytes
  if [[ -r $f ]]; then awk '{ printf "%.2f", $1 / 1073741824 }' "$f"; fi
}

# ${AWS_BUCKET_NAME} placeholder (literal text, not expanded by the platform)
expand_bucket() {
  local v="$1" ph='${AWS_BUCKET_NAME}'
  if [[ "$v" == *"$ph"* ]]; then
    [[ -n "${AWS_BUCKET_NAME:-}" ]] || { echo "[carpediem] AWS_BUCKET_NAME is not set, cannot resolve: $v" >&2; exit 2; }
    v="${v//"$ph"/$AWS_BUCKET_NAME}"
  fi
  printf '%s' "$v"
}

# Short form of a text for the run id: ASCII, lowercase, dashes, at most 40 characters,
# cut between two words
slugify() {
  local s
  s=$(printf '%s' "$1" | iconv -f utf-8 -t ascii//TRANSLIT 2>/dev/null | tr '[:upper:]' '[:lower:]' \
    | sed -E 's/[^a-z0-9]+/-/g; s/^-+//; s/-+$//')
  if (( ${#s} > 40 )); then
    s="${s:0:41}"
    if [[ "$s" == *-* ]]; then s="${s%-*}"; else s="${s:0:40}"; fi
  fi
  printf '%s' "$s"
}

is_s3() { [[ "$1" == s3://* ]]; }
is_url() { [[ "$1" == http://* || "$1" == https://* ]]; }
is_archive() { [[ "$1" == *.tar.gz || "$1" == *.tgz ]]; }

# s3://bucket/prefix -> s3:bucket/prefix (rclone remote "s3")
to_rclone() { echo "s3:${1#s3://}"; }

# rclone remote "s3" configured from the standard AWS_* variables (no config file)
setup_s3() {
  : "${AWS_ACCESS_KEY_ID:?AWS_ACCESS_KEY_ID is required for s3:// locations}"
  : "${AWS_SECRET_ACCESS_KEY:?AWS_SECRET_ACCESS_KEY is required for s3:// locations}"
  local endpoint="${AWS_S3_ENDPOINT:-${S3_ENDPOINT:-minio.dive.edito.eu}}"
  [[ "$endpoint" == http* ]] || endpoint="https://${endpoint}"
  export RCLONE_CONFIG_S3_TYPE=s3
  export RCLONE_CONFIG_S3_PROVIDER=Minio
  export RCLONE_CONFIG_S3_ENV_AUTH=true
  export RCLONE_CONFIG_S3_ENDPOINT="$endpoint"
  export RCLONE_CONFIG_S3_REGION="${AWS_DEFAULT_REGION:-us-east-1}"
  export RCLONE_CONFIG=/dev/null
}

CARPEDIEM_STUDY="$(expand_bucket "$CARPEDIEM_STUDY")"
CARPEDIEM_OUTPUT="$(expand_bucket "$CARPEDIEM_OUTPUT")"
export CARPEDIEM_STUDY CARPEDIEM_CONFIG CARPEDIEM_RUN_DESCRIPTION

# ---- Run id and run folder ---------------------------------------------------------------------
suffix=$(slugify "$CARPEDIEM_RUN_DESCRIPTION")
if [[ -z "$suffix" ]]; then
  cfg_name=$(basename "$CARPEDIEM_CONFIG"); cfg_name="${cfg_name%.*}"; cfg_name="${cfg_name#config_}"
  suffix=$(slugify "$cfg_name")
fi
export RUN_ID
RUN_ID="$(date -u -d "@${START_TS}" +%Y%m%dT%H%M%SZ)-${suffix:-run}"
RUN_DIR="/tmp/carpediem-runs/${RUN_ID}"
RUN_DEST="${CARPEDIEM_OUTPUT%/}/${RUN_ID}"
RUN_LOG="${RUN_DIR}/run.log"
mkdir -p "$RUN_DIR"
: > "$RUN_LOG"

# Messages go to the console and to run.log
log() {
  local m="[carpediem] $(date -u +%H:%M:%S) $*"
  echo "$m"
  echo "$m" >> "$RUN_LOG"
}

# run.json, written by run_manifest.R (R with the project library, from APP_DIR)
write_manifest() {
  export RUN_STATUS="$1"
  export RUN_CPU_LIMIT RUN_MEM_LIMIT_GIB RUN_MEM_PEAK_GIB RUN_CONFIG_PATH="${STUDY_DIR}/${CARPEDIEM_CONFIG}"
  RUN_CPU_LIMIT=$(cpu_limit); RUN_MEM_LIMIT_GIB=$(mem_limit_gib); RUN_MEM_PEAK_GIB=$(mem_peak_gib)
  (cd "$APP_DIR" && Rscript "${TOOLS_DIR}/run_manifest.R" "$RUN_DIR") >> "$RUN_LOG" 2>&1 \
    || log "Warning: run.json could not be written"
}

# Copy the run folder (or only run.json) to its destination
publish() {
  if is_s3 "$RUN_DEST"; then
    if [[ "${1:-}" == manifest ]]; then
      rclone copyto "${RUN_DIR}/run.json" "$(to_rclone "$RUN_DEST")/run.json"
    else
      rclone copy "$RUN_DIR" "$(to_rclone "$RUN_DEST")"
    fi
  else
    mkdir -p "$RUN_DEST"
    if [[ "${1:-}" == manifest ]]; then cp "${RUN_DIR}/run.json" "$RUN_DEST/"; else cp -r "$RUN_DIR"/. "$RUN_DEST"/; fi
  fi
}

# End of run, whatever happened: complete run.json and publish the run folder
finish() {
  local code=$?
  set +e
  trap - EXIT
  export RUN_EXIT_CODE="$code" RUN_ENDED_AT RUN_DURATION_S RUN_WORKERS
  RUN_ENDED_AT=$(date -u +%Y-%m-%dT%H:%M:%SZ)
  RUN_DURATION_S=$(( $(date +%s) - START_TS ))
  RUN_WORKERS=$(grep -m1 -oE '^Workers : [0-9]+' "$RUN_LOG" 2>/dev/null | grep -oE '[0-9]+$')
  # Pipeline results: content of study/outputs/results_<date>/ (or of study/outputs/)
  local res; res=$(ls -d "$STUDY_DIR"/outputs/results_*/ 2>/dev/null | tail -1)
  if [[ -n "$res" ]]; then
    mkdir -p "${RUN_DIR}/outputs" && cp -r "$res". "${RUN_DIR}/outputs/"
  elif [[ -d "$STUDY_DIR/outputs" ]] && [[ -n "$(ls -A "$STUDY_DIR/outputs" 2>/dev/null)" ]]; then
    mkdir -p "${RUN_DIR}/outputs" && cp -r "$STUDY_DIR/outputs"/. "${RUN_DIR}/outputs/"
  fi
  log "End: exit code ${code}, duration $(( RUN_DURATION_S / 60 )) min $(( RUN_DURATION_S % 60 )) s, CPU limit $(cpu_limit), memory limit $(m=$(mem_limit_gib); [[ -n $m ]] && echo "$m GiB" || echo none), peak memory $(m=$(mem_peak_gib); [[ -n $m ]] && echo "$m GiB" || echo unknown)"
  if [[ "$code" == 0 ]]; then write_manifest succeeded; else write_manifest failed; fi
  log "Writing run folder to ${RUN_DEST}"
  publish || { echo "[carpediem] Could not write the run folder to ${RUN_DEST}" >&2; [[ "$code" == 0 ]] && code=3; }
  exit "$code"
}

# ---- Run -----------------------------------------------------------------------------------------
if is_s3 "$CARPEDIEM_STUDY" || is_s3 "$CARPEDIEM_OUTPUT"; then setup_s3; fi
log "Run ${RUN_ID} started (${RUN_STARTED_AT}), CPU limit: $(cpu_limit), memory limit: $(m=$(mem_limit_gib); [[ -n $m ]] && echo "$m GiB" || echo none)"
log "Run folder: ${RUN_DEST}"
trap finish EXIT

# 1. Study: fetched (folder or archive), then copied as is into the run folder (study/)
[[ -n "$CARPEDIEM_STUDY" ]] || { log "CARPEDIEM_STUDY is not set (study folder or .tar.gz archive)"; exit 2; }
rm -rf "$STUDY_DIR"
mkdir -p "$STUDY_DIR"
if is_archive "$CARPEDIEM_STUDY"; then
  archive=/tmp/carpediem-study.tar.gz
  log "Downloading study archive from ${CARPEDIEM_STUDY}"
  if is_url "$CARPEDIEM_STUDY"; then
    curl -fsSL --retry 3 -o "$archive" "$CARPEDIEM_STUDY" || { log "Study archive not found: $CARPEDIEM_STUDY"; exit 2; }
  elif is_s3 "$CARPEDIEM_STUDY"; then
    rclone copyto "$(to_rclone "$CARPEDIEM_STUDY")" "$archive" || { log "Study archive not found: $CARPEDIEM_STUDY"; exit 2; }
  else
    [[ -f "$CARPEDIEM_STUDY" ]] || { log "Study archive not found: $CARPEDIEM_STUDY"; exit 2; }
    cp "$CARPEDIEM_STUDY" "$archive"
  fi
  tar -xzf "$archive" -C "$STUDY_DIR" || { log "Study archive could not be extracted: $CARPEDIEM_STUDY"; exit 2; }
  rm -f "$archive"
  # archive containing a single top folder (e.g. study/): use its content
  if [[ ! -d "$STUDY_DIR/config" ]] && [[ $(ls -A "$STUDY_DIR" | wc -l) == 1 ]] && [[ -d "$STUDY_DIR/$(ls -A "$STUDY_DIR")" ]]; then
    top="$STUDY_DIR/$(ls -A "$STUDY_DIR")"; mv "$top" "${STUDY_DIR}.tmp" && rm -rf "$STUDY_DIR" && mv "${STUDY_DIR}.tmp" "$STUDY_DIR"
  fi
elif is_s3 "$CARPEDIEM_STUDY"; then
  log "Downloading study from ${CARPEDIEM_STUDY}"
  rclone copy "$(to_rclone "$CARPEDIEM_STUDY")" "$STUDY_DIR"
else
  [[ -d "$CARPEDIEM_STUDY" ]] || { log "Study folder not found: $CARPEDIEM_STUDY"; exit 2; }
  log "Using study from ${CARPEDIEM_STUDY}"
  cp -r "$CARPEDIEM_STUDY"/. "$STUDY_DIR"/
fi
rm -rf "$STUDY_DIR/outputs"
mkdir -p "${RUN_DIR}/study"
cp -r "$STUDY_DIR"/. "${RUN_DIR}/study/"
mkdir -p "$STUDY_DIR/outputs"

CONFIG_PATH="${STUDY_DIR}/${CARPEDIEM_CONFIG}"
[[ -f "$CONFIG_PATH" ]] || { log "Config not found: $CONFIG_PATH"; exit 2; }

# run.json with status "running", so that the run is visible while it is computed
write_manifest running
publish manifest

# 2. Run the pipeline -----------------------------------------------------
log "Running pipeline with ${CARPEDIEM_CONFIG}"
cd "$APP_DIR"
set +e
Rscript execution/run_batch.R "$CONFIG_PATH" 2>&1 | tee -a "$RUN_LOG"
status=${PIPESTATUS[0]}
set -e
log "Pipeline exit code: ${status}"

# 3. Run folder: written by finish() (trap on EXIT)
exit "$status"
