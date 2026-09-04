# engine/functions/export_to_postgres.R

# utilise x si x existe et n’est pas vide, sinon utilise y
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# util: prendre le fichier le plus récent
pick_latest <- function(files) {
  files <- files[file.exists(files)]
  if (length(files) == 0) return(character(0))
  files[which.max(file.info(files)$mtime)]
}

# util: normaliser un nom SQL safe
sanitize_name <- function(x) {
  x <- tolower(x)
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# ---- Low-level: écrire un sf dans PostGIS ---------------------------------
write_sf_table <- function(con, obj_sf, schema, table,
                           overwrite = TRUE,
                           create_spatial_index = TRUE,
                           geom_col = NULL) {
  stopifnot(inherits(obj_sf, "sf"))
  if (!requireNamespace("sf", quietly = TRUE)) stop("sf manquant")
  if (!requireNamespace("DBI", quietly = TRUE)) stop("DBI manquant")
  
  schema <- sanitize_name(schema)
  table  <- sanitize_name(table)
  
  DBI::dbExecute(con, sprintf('CREATE SCHEMA IF NOT EXISTS "%s";', schema))
  
  # Détecter colonne géométrie sf
  sfcol <- attr(obj_sf, "sf_column")
  if (is.null(sfcol) || !nzchar(sfcol)) stop("write_sf_table: sf_column absent")
  
  # Optionnel: renommer la géométrie (si vous voulez "geom" partout)
  if (!is.null(geom_col) && nzchar(geom_col) && !identical(sfcol, geom_col)) {
    names(obj_sf)[names(obj_sf) == sfcol] <- geom_col
    sf::st_geometry(obj_sf) <- geom_col
    sfcol <- geom_col
  }
  
  if (isTRUE(overwrite)) {
    DBI::dbExecute(con, sprintf('DROP TABLE IF EXISTS "%s"."%s" CASCADE;', schema, table))
  }
  
  sf::st_write(
    obj_sf,
    dsn   = con,
    layer = DBI::Id(schema = schema, table = table),
    append = FALSE,
    quiet  = TRUE
  )
  
  if (isTRUE(create_spatial_index)) {
    idx <- sprintf('%s_%s_gist', table, sfcol)
    try(DBI::dbExecute(
      con,
      sprintf('CREATE INDEX IF NOT EXISTS "%s" ON "%s"."%s" USING GIST ("%s");',
              idx, schema, table, sfcol)
    ), silent = TRUE)
    try(DBI::dbExecute(con, sprintf('ANALYZE "%s"."%s";', schema, table)), silent = TRUE)
  }
  
  invisible(TRUE)
}

# ---- Lire un GPKG multi-couches et tout importer --------------------------
import_gpkg_all_layers <- function(con, gpkg_path, schema,
                                   table_prefix = NULL,
                                   overwrite = TRUE,
                                   geom_col = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("sf manquant")
  stopifnot(file.exists(gpkg_path))
  
  layers <- sf::st_layers(gpkg_path)$name
  if (length(layers) == 0) stop("Aucune couche dans: ", gpkg_path)
  
  for (ly in layers) {
    obj <- sf::st_read(gpkg_path, layer = ly, quiet = TRUE)
    
    tname <- if (!is.null(table_prefix) && nzchar(table_prefix)) {
      paste0(table_prefix, "_", ly)
    } else {
      ly
    }
    
    write_sf_table(
      con, obj,
      schema = schema,
      table  = tname,
      overwrite = overwrite,
      geom_col = geom_col
    )
  }
  
  invisible(layers)
}

# ---- Lire un GPKG mono-couche et l’importer sous un nom stable ------------
import_gpkg_single_layer_as <- function(con, gpkg_path, schema, table,
                                        overwrite = TRUE,
                                        geom_col = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("sf manquant")
  stopifnot(file.exists(gpkg_path))
  
  layers <- sf::st_layers(gpkg_path)$name
  if (length(layers) < 1) stop("Aucune couche dans: ", gpkg_path)
  
  obj <- sf::st_read(gpkg_path, layer = layers[1], quiet = TRUE)
  
  write_sf_table(
    con, obj,
    schema = schema,
    table  = table,
    overwrite = overwrite,
    geom_col = geom_col
  )
  
  invisible(layers[1])
}

# ---- Fonction principale appelée depuis run_batch -------------------------
export_gpkgs_to_postgres <- function(config, outputs_dir,
                                     schema = NULL,
                                     overwrite = TRUE,
                                     geom_col = "geom",
                                     run_tag = NULL) {
  stopifnot(dir.exists(outputs_dir))
  if (!requireNamespace("DBI", quietly = TRUE)) stop("DBI manquant")
  
  # Schema stable: basé sur le dossier run (ex: 2026-01-21_16-27)
  if (is.null(schema) || !nzchar(schema)) {
    base <- basename(normalizePath(outputs_dir, winslash = "/", mustWork = TRUE))
    ts <- sub("^results[_-]?", "", base)
    schema <- sanitize_name(paste0("results_analyse_deterministe_", ts))
  } else {
    schema <- sanitize_name(schema)
  }
  
  # Connexion: on réutilise votre get_db_conn() déjà présent dans le projet
  if (!exists("get_db_conn", mode = "function")) {
    stop("export_gpkgs_to_postgres: get_db_conn() introuvable (source engine/db_connect.R avant).")
  }
  
  con <- get_db_conn(config, config$db$choice)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  
  # 1) resultats_analyse_*.gpkg (dans outputs_dir)
  res_gpkg <- list.files(outputs_dir, pattern = "^resultats_analyse_.*\\.gpkg$", full.names = TRUE)
  res_latest <- pick_latest(res_gpkg)
  if (length(res_latest) > 0) {
    import_gpkg_all_layers(con, res_latest, schema = schema, table_prefix = "resultats",
                           overwrite = overwrite, geom_col = geom_col)
  }
  
  # 2) details_analyse_*.gpkg (dans outputs_dir)
  det_gpkg <- list.files(outputs_dir, pattern = "^details_analyse_.*\\.gpkg$", full.names = TRUE)
  det_latest <- pick_latest(det_gpkg)
  if (length(det_latest) > 0) {
    import_gpkg_all_layers(con, det_latest, schema = schema, table_prefix = "details",
                           overwrite = overwrite, geom_col = geom_col)
  }
  
  # 3) mono-couches (REXC/REFC/IC) dans un dossier maps (souvent global)
  if (is.null(outputs_dir) || !nzchar(outputs_dir)) {
    outputs_dir <- file.path(outputs_dir, "maps")
  }
  
  if (dir.exists(outputs_dir)) {
    # Vos fichiers mono-couches ont un timestamp "YYYYMMDD_HHMMSS"
    # alors que ctx$datesauv est plutôt "YYYY-MM-DD_HH-MM".
    # On construit donc un motif robuste si run_tag est fourni.
    patt_suffix <- ".*"
    if (!is.null(run_tag) && nzchar(run_tag)) {
      # ex: 2026-01-21_16-27 -> 20260121_1627
      tag_digits <- gsub("[^0-9]+", "", run_tag)
      if (nchar(tag_digits) >= 12) {
        patt_suffix <- paste0(substr(tag_digits, 1, 8), "_", substr(tag_digits, 9, 12))
      }
    }
    
    rexc <- list.files(outputs_dir, pattern = paste0("^REXC_E1_", patt_suffix, ".*\\.gpkg$"), full.names = TRUE)
    refc <- list.files(outputs_dir, pattern = paste0("^REFC_E1_", patt_suffix, ".*\\.gpkg$"), full.names = TRUE)
    ic   <- list.files(outputs_dir, pattern = paste0("^IC_",      patt_suffix, ".*\\.gpkg$"), full.names = TRUE)
    
    # Fallback: si run_tag ne matche rien, on prend le dernier fichier tout court
    if (length(rexc) == 0) rexc <- list.files(outputs_dir, pattern = "^REXC_E1_.*\\.gpkg$", full.names = TRUE)
    if (length(refc) == 0) refc <- list.files(outputs_dir, pattern = "^REFC_E1_.*\\.gpkg$", full.names = TRUE)
    if (length(ic)   == 0) ic   <- list.files(outputs_dir, pattern = "^IC_.*\\.gpkg$",      full.names = TRUE)
    
    rexc_latest <- pick_latest(rexc)
    refc_latest <- pick_latest(refc)
    ic_latest   <- pick_latest(ic)
    
    if (length(rexc_latest) > 0) import_gpkg_single_layer_as(con, rexc_latest, schema, "rexc_e1",
                                                             overwrite = overwrite, geom_col = geom_col)
    if (length(refc_latest) > 0) import_gpkg_single_layer_as(con, refc_latest, schema, "refc_e1",
                                                             overwrite = overwrite, geom_col = geom_col)
    if (length(ic_latest) > 0)   import_gpkg_single_layer_as(con, ic_latest, schema, "ic_global",
                                                             overwrite = overwrite, geom_col = geom_col)
  }
  
  invisible(list(schema = schema))
}


