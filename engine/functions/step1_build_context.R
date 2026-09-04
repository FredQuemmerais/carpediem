# engine/functions/step1_build_context.R

step1_build_context <- function(config, paths, con, data, init) {
  
  # Données importées (Step0)
  list_sauv <- data
  
  # datasets sous forme data.frame
  dsets <- as.data.frame(config$datasets, stringsAsFactors = FALSE)
  if (is.null(dsets) || !is.data.frame(dsets) || nrow(dsets) == 0) {
    stop("ETAPE 2: config$datasets manquant ou vide.")
  }
  
  # Spec grid (nécessaire pour l'ETAPE 3) ---------------------
  grid_spec <- dsets[dsets$Table_code == "grid" & dsets$Data_type == "G", , drop = FALSE]
  if (nrow(grid_spec) != 1) {
    stop("ETAPE 2: config$datasets doit contenir exactement 1 ligne pour Data_type='G' et Table_code='grid'.")
  }
  
  # Spec habitats (nécessaire pour l'ETAPE 4) ---------------------
  hab_spec <- dsets[dsets$Data_type == "E", , drop = FALSE]
  if (nrow(hab_spec) != 1) {
    stop("ETAPE 2: config$datasets doit contenir exactement 1 ligne pour Data_type='E' (habitats).")
  }
  
  # Spec activités (nécessaire pour l'ETAPE 6) ---------------------------------------
  act_specs <- dsets |>
    dplyr::filter(.data$Data_type == "A")
  
  if (nrow(act_specs) < 1) stop("ETAPE 2: dataset activité (Data_type='A') introuvable.")
  
  # Support multi-tables activités
  data_source <- tolower(trimws(config$db$data_source %||% "postgis"))
  mode_flat   <- identical(data_source, "flat_files")
  
  if (mode_flat) {
    act_keys <- act_specs |>
      dplyr::distinct(.data$Table_code, .data$Source, .data$id_col, .data$geom_col)
  } else {
    act_keys <- act_specs |>
      dplyr::distinct(.data$Table_code, .data$Source, .data$DB_schema, .data$DB_table, .data$id_col, .data$geom_col)
  }
  
  extract_intensity_cols <- function(x) {
    if (is.null(x)) return(character(0))
    if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
    x <- as.character(x)
    x <- trimws(x)
    x[nzchar(x)]
  }
  
  activity_specs <- vector("list", nrow(act_keys))
  for (ii in seq_len(nrow(act_keys))) {
    key <- act_keys[ii, , drop = FALSE]
    
    if (mode_flat) {
      sub <- act_specs |>
        dplyr::filter(
          .data$Table_code == key$Table_code,
          .data$Source     == key$Source,
          .data$id_col     == key$id_col,
          .data$geom_col   == key$geom_col
        )
    } else {
      sub <- act_specs |>
        dplyr::filter(
          .data$Table_code == key$Table_code,
          .data$Source     == key$Source,
          .data$DB_schema  == key$DB_schema,
          .data$DB_table   == key$DB_table,
          .data$id_col     == key$id_col,
          .data$geom_col   == key$geom_col
        )
    }
    
    int_cols <- extract_intensity_cols(sub$intensity_cols)
    
    activity_specs[[ii]] <- list(
      table_code     = as.character(key$Table_code[[1]]),
      source         = as.character(key$Source[[1]]),
      db_schema      = if (!mode_flat) as.character(key$DB_schema[[1]]) else NA_character_,
      db_table       = if (!mode_flat) as.character(key$DB_table[[1]])  else NA_character_,
      id_col         = as.character(key$id_col[[1]]),
      geom_col       = as.character(key$geom_col[[1]]),
      intensity_cols = unique(int_cols)
    )
  }
  
  # 1 seule table "principale" (legacy)
  activity_spec <- activity_specs[[1]]
  
  # Construire le contexte unique utilisé par Step1 --------------------------
  ctx <- list(
    config         = config,
    paths          = paths,
    con            = con,
    list_sauv      = list_sauv,
    dsets          = dsets,
    sensi_path     = init$sensi_path,
    act_press_path = init$act_press_path,
    lcond_SRM      = init$lcond_SRM,
    datesauv       = init$datesauv,
    outputsdir     = init$outputsdir,
    activity_spec  = activity_spec,
    activity_specs = activity_specs,
    hab_spec       = hab_spec,
    grid_spec      = grid_spec,
    maps_dir       = init$maps_dir,
    graphs_dir     = init$graphs_dir,
    
    # --- Matrices CCR (chargées et validées dans step1_init) -----------------
    # MatAP_active  = CCR_mediane : scénario utilisé par le pipeline principal
    #                 (étapes 7 et 9). Remplace la relecture ODS en étape 7.
    # MatAP_precaution, MatAP_binaire : réservés au Monte Carlo (step2),
    #                 non utilisés dans le pipeline principal.
    MatAP_active     = init$MatAP_active,
    MatAP_mediane    = init$MatAP_mediane,
    MatAP_precaution = init$MatAP_precaution,
    MatAP_binaire    = init$MatAP_binaire,
    
    # --- Hiérarchie des pressions (chargée et validée dans step1_init) -------
    # Utilisée en étape 7 pour filtrer les pressions composées (mode_calcul==3)
    # et en étape 9 pour la récursion bottom-up du calcul de REFC.
    typo_press = init$typo_press
  )
  
  ctx
}