# engine/functions/utils_parallel.R
# Dimensionnement des workers et graine aléatoire des étapes parallélisées

# Nombre de workers pour future::plan(multisession).
# Priorité : config$execution$workers, puis variable d'environnement
# CARPEDIEM_WORKERS, sinon parallelly::availableCores(omit = 1).
# availableCores() tient compte des limites CPU du conteneur (cgroups),
# contrairement à parallel::detectCores() qui renvoie les cœurs de l'hôte.
n_workers_for <- function(config = NULL) {
  as_pos_int <- function(x) {
    v <- suppressWarnings(as.integer(x))
    if (length(v) == 1 && !is.na(v) && v >= 1L) v else NA_integer_
  }
  w <- as_pos_int(config$execution$workers)
  if (is.na(w)) w <- as_pos_int(Sys.getenv("CARPEDIEM_WORKERS", ""))
  if (is.na(w)) {
    w <- if (requireNamespace("parallelly", quietly = TRUE)) {
      as.integer(parallelly::availableCores(omit = 1L))
    } else {
      max(1L, parallel::detectCores() - 1L)
    }
  }
  max(1L, w)
}

# Fixe la graine si elle est renseignée (section d'analyse, sinon execution.seed).
# Avec future.seed = TRUE, les tirages deviennent reproductibles quel que soit
# le nombre de workers.
set_seed_if_any <- function(config, section_cfg = NULL) {
  seed <- section_cfg$seed %||% config$execution$seed
  seed <- suppressWarnings(as.integer(seed))
  if (length(seed) == 1 && !is.na(seed)) {
    set.seed(seed)
    message("Graine aléatoire : ", seed)
  }
  invisible(seed)
}

# Taille maximale des objets envoyés aux workers (option future.globals.maxSize).
# Par défaut, future refuse au-delà de 500 Mo ; sur une grande zone d'étude
# (Mer du Nord : environ 670 Mo pour le Monte Carlo), la limite est dépassée.
# Priorité : config$execution$globals_max_mb, puis variable d'environnement
# CARPEDIEM_GLOBALS_MAX_MB, sinon pas de limite (la mémoire disponible du
# conteneur reste la vraie contrainte : chaque worker reçoit une copie).
# Renvoie l'ancienne valeur, pour la restaurer avec options().
set_globals_max_size <- function(config = NULL) {
  mb <- suppressWarnings(as.numeric(config$execution$globals_max_mb))
  if (length(mb) != 1 || is.na(mb)) mb <- suppressWarnings(as.numeric(Sys.getenv("CARPEDIEM_GLOBALS_MAX_MB", "")))
  limit <- if (length(mb) == 1 && !is.na(mb) && mb > 0) mb * 1024^2 else Inf
  old <- options(future.globals.maxSize = limit)
  message("Taille max. des objets envoyés aux workers : ",
          if (is.finite(limit)) paste(mb, "Mo") else "sans limite")
  invisible(old)
}
