# execution/run_batch.R
# Lance le pipeline dans la console R
# version du 24.07.2026

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Le package 'here' est requis pour exécuter run_batch.R. Installez-le: install.packages('here')")
}
src <- function(...) source(here::here(...), chdir = TRUE)

# ---- Charger les scripts ------------------------------------------------
src("engine", "load_config.R")
src("engine", "validate_config.R")
src("engine", "db_connect.R")
src("engine", "io_paths.R")
src("engine", "steps",     "step0_checkup.R")
src("engine", "steps",     "step1_analyse_simple.R")
src("engine", "functions", "step1_init.R")
src("engine", "functions", "step1_build_context.R")
src("engine", "steps",     "step2_monte_carlo.R")
src("engine", "steps",     "step2b_robustesse_det.R")
src("engine", "steps",     "step2c_robustesse_stoch.R")
src("engine", "steps",     "step2d_sensibilite.R")
src("engine", "functions", "f_aux_monte_carlo.R")
src("engine", "steps",     "step3_graphiques.R")
src("engine", "functions", "f_aux_step3.R")

# ---- Charger les helpers engine/functions --------------------------------
helper_dir   <- here::here("engine", "functions")
helper_files <- sort(list.files(helper_dir, pattern = "\\.R$", full.names = TRUE))
invisible(lapply(helper_files, source, chdir = TRUE))

# ---- Vérifier librairies ------------------------------------------------
pkgs <- c("sf", "DBI", "RPostgres", "readODS", "dplyr", "tidyr",
          "tibble", "ggplot2")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Package manquant: ", p)
}
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- Charger config + chemins -------------------------------------------
args      <- commandArgs(trailingOnly = TRUE)
yaml_path <- if (length(args) >= 1 && nzchar(args[1])) {
  args[1]
} else {
  here::here("study", "config", "config.yml")
}
config <- load_config(yaml_path)
validate_config(config)
paths  <- io_paths(config)

# ---- Connexion DB -------------------------------------------------------
con <- get_db_conn(config, config$db$choice)
on.exit(db_disconnect_safe(con), add = TRUE)

# ---- Lancement des steps ------------------------------------------------

# STEP 0 - Checkup
res0 <- step0_checkup(config, paths, con = con)

# STEP 1 - Analyse simple avec modèle de référence
res1 <- step1_analyse_simple(config, paths, con = con, data = res0$list_sauv)

# ENGINE - bundle mutualisé pour step2 / step2b / step2d
engine <- NULL
if (any(c(
  isTRUE(config$analyses$monte_carlo$enabled),
  isTRUE(config$analyses$robustesse_deterministe$enabled),
  isTRUE(config$analyses$sensibilite_formelle$enabled)
))) {
  message("\n[ENGINE] Construction du bundle engine...")
  engine <- engine_step2(res1$ctx)
  message("[ENGINE] Bundle prêt.\n")
}

# STEP 2 — Tirages Monte Carlo
# Activé si config$analyses$monte_carlo$enabled == TRUE
res2 <- NULL
if (isTRUE(config$analyses$monte_carlo$enabled)) {
  
  message("\n[RUN] Step2 — Monte Carlo stochastique")
  res2 <- step2_monte_carlo(config, paths, con = con, res1 = res1, engine = engine)
  
} else {
  message("\n[SKIP] Step2 — Monte Carlo désactivé (config$analyses$monte_carlo$enabled = FALSE)")
}

# STEP 2B — Robustesse déterministe
# Activé si config$analyses$robustesse_deterministe$enabled == TRUE
# Indépendant de step2 — ne nécessite pas res2
res2b <- NULL
if (isTRUE(config$analyses$robustesse_deterministe$enabled)) {
  
  message("\n[RUN] Step2b — Robustesse déterministe")
  res2b <- step2b_robustesse_det(config, paths, con = con, res1 = res1, engine = engine)
  
} else {
  message("\n[SKIP] Step2b — Robustesse déterministe désactivée",
          " (config$analyses$robustesse_deterministe$enabled = FALSE)")
}

# STEP 2C — Robustesse stochastique
# Activé si config$analyses$robustesse_stoch$enabled == TRUE
# Nécessite res2 (step2 doit avoir tourné)
res2c <- NULL
if (isTRUE(config$analyses$robustesse_stoch$enabled)) {
  
  if (is.null(res2)) {
    warning(paste(
      "[SKIP] Step2c — robustesse_stoch activée mais res2 est NULL.",
      "Activez config$analyses$monte_carlo$enabled pour produire res2."
    ))
  } else {
    message("\n[RUN] Step2c — Robustesse stochastique")
    res2c <- step2c_robustesse_stoch(config, res1 = res1, res2 = res2)
  }
  
} else {
  message("\n[SKIP] Step2c — Robustesse stochastique désactivée",
          " (config$analyses$robustesse_stoch$enabled = FALSE)")
}

# STEP 2D — Sensibilité formelle
# Activé si config$analyses$sensibilite_formelle$enabled == TRUE
res2d <- NULL
if (isTRUE(config$analyses$sensibilite_formelle$enabled)) {
  
  message("\n[RUN] Step2d — Sensibilité formelle")
  res2d <- step2d_sensibilite(config, paths, res1 = res1, res2b = res2b, engine = engine)
  
} else {
  message("\n[SKIP] Step2d — Sensibilité formelle désactivée",
          " (config$analyses$sensibilite_formelle$enabled = FALSE)")
}

# STEP 3 — Graphiques
res3 <- NULL
if (isTRUE(config$Step3$enabled)) {
  
  mode <- config$Step3$mode %||% "simplifie"
  
  if (identical(mode, "complet")) {
    
    zone_field <- config$Step3$zone_field
    if (is.null(zone_field) || !nzchar(zone_field)) {
      stop("Step3: mode='complet' mais Step3$zone_field est manquant ou vide dans la config.")
    }
    
    res3 <- generate_carpediem_graphics(
      res1          = res1,
      zone_analysis = TRUE,
      zone_field    = zone_field
    )
    
  } else if (identical(mode, "simplifie")) {
    
    res3 <- quick_graphics(res1)
    
  } else {
    stop("Step3: mode invalide. Valeurs attendues: 'simplifie' ou 'complet'.")
  }
}

# Rapport HTML — carte interactive Leaflet ----------
if (requireNamespace("sf", quietly = TRUE) && !is.null(res1$ctx)) {
  src("engine", "functions", "report_leaflet.R")
  write_leaflet_html(config, res1$ctx)
}

# Rapport HTML — rapport global --------

if (!is.null(config$report) &&
    isTRUE(config$report$enabled) &&
    !is.null(res1$ctx)) {
  
  src("engine", "functions", "report_global_html.R")
  write_global_report_html(
    config = config,
    res1   = res1,
    res2   = res2,
    res2b  = res2b,
    res2c  = res2c,
    res2d  = res2d,
    res3   = if (!is.null(res3)) res3 else NULL
  )
}

message("\n========================================")
message("   PIPELINE CARPEDIEM TERMINÉ")
message("========================================\n")
