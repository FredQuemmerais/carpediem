# engine/io_paths.R
# Centralise toute la résolution des chemins
# Convention: project_root = dossier contenant engine/, study/

io_paths <- function(config = NULL, project_root = NULL) {
  
  if (is.null(project_root)) project_root <- getwd()
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  
  if (!is.null(config) && is.list(config) &&
      !is.null(config$project) && !is.null(config$project$project_root) &&
      nzchar(trimws(as.character(config$project$project_root)))) {
    project_root <- normalizePath(as.character(config$project$project_root),
                                  winslash = "/", mustWork = TRUE)
  }
  
  study_dir <- file.path(project_root, "study")   # variable intermédiaire propre
  
  paths <- list(
    project_root      = project_root,
    engine_dir        = file.path(project_root, "engine"),
    functions_dir     = file.path(project_root, "engine", "functions"),
    study_dir         = study_dir,
    config_dir        = file.path(study_dir, "config"),
    inputs_dir        = file.path(study_dir, "inputs"),
    outputs_dir       = file.path(study_dir, "outputs"),
    f_aux_monte_carlo = file.path(project_root, "engine", "functions", "f_aux_monte_carlo.R")
  )
  
  dir.create(paths$outputs_dir, recursive = TRUE, showWarnings = FALSE)
  
  paths
}
