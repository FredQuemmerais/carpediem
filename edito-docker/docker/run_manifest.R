# Writes run.json, the description of a Carpediem run (see edito-docker/RUNS_LAYOUT.md).
# Called by entrypoint.sh, from /opt/carpediem (renv library), at the start and at the end
# of the run:
#   Rscript run_manifest.R <run folder>
# Everything else comes from environment variables set by entrypoint.sh (RUN_*, CARPEDIEM_*).

suppressPackageStartupMessages({
  library(jsonlite)
  library(yaml)
})

args <- commandArgs(trailingOnly = TRUE)
run_dir <- args[1]
env <- function(name, default = NA) {
  v <- Sys.getenv(name, "")
  if (nzchar(v)) v else default
}
num <- function(x) suppressWarnings(as.numeric(x))
`%||%` <- function(a, b) if (is.null(a)) b else a
cfg <- NULL

status <- env("RUN_STATUS", "running")

# ---- Parameters, from the configuration file actually used ------------------------------
params <- NULL
cfg_path <- env("RUN_CONFIG_PATH")
if (!is.na(cfg_path) && file.exists(cfg_path)) {
  cfg <- tryCatch(yaml::read_yaml(cfg_path), error = function(e) NULL)
  if (!is.null(cfg)) {
    srm <- trimws(unlist(strsplit(as.character(cfg$project$srm %||% ""), "[;,[:space:]]+")))
    an <- cfg$analyses
    enabled <- function(x) isTRUE(x$enabled)
    params <- list(
      srm = as.list(srm[nzchar(srm)]),
      agreg_sensib = cfg$Calc$agreg_sensib,
      th_sensib = cfg$Calc$th_sensib,
      seed = cfg$execution$seed,
      analyses = list(
        simple_analysis = TRUE,
        monte_carlo = enabled(an$monte_carlo),
        monte_carlo_simulations = if (enabled(an$monte_carlo)) an$monte_carlo$nbsimul else NULL,
        deterministic_robustness = enabled(an$robustesse_deterministe),
        stochastic_robustness = enabled(an$robustesse_stoch),
        shapley_sensitivity = enabled(an$sensibilite_formelle)
      )
    )
  }
}
# ---- Files of the run ------------------------------------------------------------------------
# kind: stable identifier for the web application (file names contain timestamps)
kind_of <- function(p) {
  f <- basename(p)
  rules <- list(
    c("^run\\.log$", "log"),
    c("^study/config/[^/]+\\.ya?ml$", "config"),
    c("^study/config/matrice_act_press\\.ods$", "matrix_activities_pressures"),
    c("^study/config/matrice_sensi_hab_press\\.ods$", "matrix_sensitivity"),
    c("^study/inputs/grid\\.gpkg$", "input_grid"),
    c("^study/inputs/habbenth\\.gpkg$", "input_habitats"),
    c("^study/inputs/.+\\.gpkg$", "input_activity"),
    c("^outputs/resultats_analyse_.+\\.gpkg$", "result_indices"),
    c("^outputs/details_analyse_.+\\.gpkg$", "result_details"),
    c("^outputs/monte_carlo_.+\\.gpkg$", "result_monte_carlo"),
    c("^outputs/maps/REFC_E1_.+\\.gpkg$", "map_refc"),
    c("^outputs/maps/REXC_E1_.+\\.gpkg$", "map_rexc"),
    c("^outputs/maps/IC_.+\\.gpkg$", "map_confidence"),
    c("^outputs/maps/.+\\.geojson$", "map_geojson"),
    c("^outputs/robustesse_det/.+\\.gpkg$", "robustness"),
    c("^outputs/robustesse_det/.+\\.html$", "robustness_map_html"),
    c("^outputs/robustesse_det/.+\\.geojson$", "robustness_geojson"),
    c("^outputs/report\\.html$", "report_html"),
    c("^outputs/carte_interactive\\.html$", "map_html"),
    c("^outputs/graphs/.+\\.(jpe?g|png)$", "graph"),
    c("^outputs/graphs/.+\\.csv$", "graph_data"),
    c("^outputs/stats_recap_.+\\.json$", "summary"),
    c("^outputs/run_info\\.json$", "pipeline_info")
  )
  for (r in rules) if (grepl(r[1], p)) return(r[2])
  "other"
}

files <- list()
summary <- NULL
if (status != "running") {
  # Activity datasets: Table_code -> activity code (act_col_mapping), from the configuration
  act_code <- list()
  if (!is.null(cfg)) {
    for (d in cfg$datasets) {
      if (identical(d$Data_type, "A")) {
        cols <- unlist(d$intensity_cols)
        act_code[[d$Table_code]] <- unname(unlist(cfg$act_col_mapping[cols]))
      }
    }
  }
  paths <- sort(list.files(run_dir, recursive = TRUE, all.files = FALSE))
  paths <- setdiff(paths, "run.json")
  for (p in paths) {
    full <- file.path(run_dir, p)
    ext <- tolower(tools::file_ext(p))
    entry <- list(path = p, kind = kind_of(p), format = ext, size = file.size(full))
    if (identical(p, file.path("study/config", basename(env("CARPEDIEM_CONFIG", ""))))) entry$active_config <- TRUE
    if (entry$kind == "input_activity") {
      tc <- tools::file_path_sans_ext(basename(p))
      entry$dataset <- tc
      if (!is.null(act_code[[tc]])) entry$activity_codes <- as.list(act_code[[tc]])
    }
    if (entry$kind == "robustness" || entry$kind == "robustness_map_html" || entry$kind == "robustness_geojson") {
      entry$factor <- strsplit(p, "/")[[1]][3]
    }
    if (ext == "gpkg") {
      lyr <- tryCatch(sf::st_layers(full)$name, error = function(e) character(0))
      entry$layers <- as.list(lyr)
    }
    files[[length(files) + 1]] <- entry
  }
  sr <- list.files(file.path(run_dir, "outputs"), pattern = "^stats_recap_.*\\.json$", full.names = TRUE)
  if (length(sr) > 0) summary <- tryCatch(jsonlite::read_json(sr[1]), error = function(e) NULL)
}

manifest <- list(
  schema_version = "1.0",
  id = env("RUN_ID"),
  description = env("CARPEDIEM_RUN_DESCRIPTION", ""),
  status = status,
  exit_code = num(env("RUN_EXIT_CODE")),
  started_at = env("RUN_STARTED_AT"),
  ended_at = env("RUN_ENDED_AT"),
  duration_seconds = num(env("RUN_DURATION_S")),
  software = list(
    carpediem_version = env("CARPEDIEM_VERSION"),
    code_commit = env("CARPEDIEM_COMMIT")
  ),
  study = list(
    source = env("CARPEDIEM_STUDY"),
    config = env("CARPEDIEM_CONFIG")
  ),
  parameters = params,
  resources = list(
    cpu_limit = num(env("RUN_CPU_LIMIT")),
    memory_limit_gib = num(env("RUN_MEM_LIMIT_GIB")),
    memory_peak_gib = num(env("RUN_MEM_PEAK_GIB")),
    workers = num(env("RUN_WORKERS"))
  ),
  summary = summary,
  files = files
)

jsonlite::write_json(manifest, file.path(run_dir, "run.json"),
                     auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA)
