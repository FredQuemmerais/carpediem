# engine/steps/step1_analyse_simple.R
# Lance une analyse simple via le modèle de référence
# version du 19.08.2026

step1_analyse_simple <- function(config, paths, con, data) {
  
  # ---- Guards d'entrée Step1 --------------------------------------------
  stopifnot(
    "Step1: 'data' doit être une liste" = is.list(data),
    "Step1: data$G_grid manquant" = !is.null(data$G_grid)
  )
  
  # ---- ETAPE 1-2: Initialisation + Contexte ------------------------------
  init <- step1_init(config, paths, con, data)
  ctx <- step1_build_context(config, paths, con, data, init)
  
  list_sauv <- ctx$list_sauv
  grid_z <- ctx$grid_z
  champ_id_data_A <- ctx$activity_spec$id_col
  
  stopifnot(
    "ETAPE 6: champ_id_data_A introuvable" = !is.null(champ_id_data_A) && nzchar(champ_id_data_A)
  )
  
  # ---- ETAPE 3: Reformatage de G_grid (grille) ---------------------------
  {
    grid_spec <- ctx$grid_spec
    needed_cols <- c(grid_spec$id_col, grid_spec$geom_col, grid_spec$srm_col, grid_spec$surfmer_col)
    
    stopifnot(
      "ETAPE 3: colonnes manquantes dans G_grid" = all(needed_cols %in% names(ctx$list_sauv$G_grid)),
      "ETAPE 3: G_grid doit être un objet sf" = inherits(ctx$list_sauv$G_grid, "sf"),
      "ETAPE 3: ctx$lcond_SRM manquant" = !is.null(ctx$lcond_SRM) && length(ctx$lcond_SRM) > 0
    )
    
    grid0 <- ctx$list_sauv$G_grid
    
    # Validation géométrie
    geom <- sf::st_geometry(grid0)
    stopifnot(
      "ETAPE 3: géométrie invalide" = inherits(geom, "sfc") && length(geom) > 0 && 
        !anyNA(geom) && all(sf::st_is_valid(grid0))
    )
    
    # Définir géométrie active
    if (!identical(attr(grid0, "sf_column"), grid_spec$geom_col)) {
      sf::st_geometry(grid0) <- grid_spec$geom_col
    }
    
    # Filtrer et standardiser
    grid_z <- grid0 |>
      dplyr::transmute(
        srm = as.character(.data[[grid_spec$srm_col]]),
        surfmer = as.numeric(.data[[grid_spec$surfmer_col]]),
        idmesh = .data[[grid_spec$id_col]]
      ) |>
      dplyr::filter(.data$srm %in% ctx$lcond_SRM, !is.na(.data$surfmer), .data$surfmer > 0)
    
    # --- Conversion unités surfaces grille ---
    unit_grid_mer <- get_area_unit(config, "Grid_z", "G_surfmer")
    target_u <- config$units$areas$target
    
    grid_z$surfmer <- convert_area(grid_z$surfmer, unit_grid_mer, target_u)
    
    ctx$grid_z <- grid_z
    ctx$nz <- nrow(grid_z)
    ctx$col_idz <- grid_z$idmesh
    ctx$col_surfmer <- grid_z$surfmer
    
    # ---- Bornes lon/lat par maille (requises Step2) --------------------------
    bb <- lapply(sf::st_geometry(grid_z), sf::st_bbox)
    
    ctx$longmin_z <- vapply(bb, function(x) as.numeric(x["xmin"]), numeric(1))
    ctx$longmax_z <- vapply(bb, function(x) as.numeric(x["xmax"]), numeric(1))
    ctx$latmin_z  <- vapply(bb, function(x) as.numeric(x["ymin"]), numeric(1))
    ctx$latmax_z  <- vapply(bb, function(x) as.numeric(x["ymax"]), numeric(1))
    
    stopifnot(
      "ETAPE X: longmin_z longueur incohérente" = length(ctx$longmin_z) == nrow(grid_z),
      "ETAPE X: longmax_z longueur incohérente" = length(ctx$longmax_z) == nrow(grid_z),
      "ETAPE X: latmin_z  longueur incohérente" = length(ctx$latmin_z)  == nrow(grid_z),
      "ETAPE X: latmax_z  longueur incohérente" = length(ctx$latmax_z)  == nrow(grid_z)
    )
    
  }
  
  # ---- ETAPE 4: Reformatage de E_habbenth (habitats) ---------------------
  {
    stopifnot(
      "ETAPE 4: E_habbenth manquant"        = !is.null(ctx$list_sauv$E_habbenth),
      "ETAPE 4: E_habbenth doit être un sf" = inherits(ctx$list_sauv$E_habbenth, "sf"),
      # typo_press chargée et validée dans step1_init (contient Code_P, parent,
      # mode_calcul, demonstrateur)
      "ETAPE 4: ctx$typo_press manquant"    = !is.null(ctx$typo_press) && nrow(ctx$typo_press) > 0
    )
    
    hab0 <- ctx$list_sauv$E_habbenth
    sf::st_geometry(hab0) <- "geom"
    
    hab_spec <- ctx$hab_spec
    
    needed_hab <- c(
      hab_spec$id_col,
      hab_spec$surfmesh_col,
      hab_spec$surfmer_col,
      hab_spec$surfhab_col,
      hab_spec$habcode_col
    )
    stopifnot("ETAPE 4: colonnes manquantes" = all(needed_hab %in% names(hab0)))
    
    # --- Lire ODS sensibilités ---------------------------------------------
    sensi_path <- ctx$sensi_path
    stopifnot("ETAPE 4: sensi_path introuvable" = !is.null(sensi_path) && file.exists(sensi_path))
    
    agreg <- config$Calc$agreg_sensib
    stopifnot("ETAPE 4: agreg_sensib invalide" = agreg %in% c(1, 2))
    sheet_name <- if (agreg == 1) "precaution" else "median"
    
    ods <- readODS::read_ods(sensi_path, sheet = sheet_name)
    names(ods) <- norm_name(names(ods))
    
    if (!"type" %in% names(ods)) names(ods)[1] <- "type"
    stopifnot("ETAPE 4: colonne type introuvable"   = "type"   %in% names(ods))
    stopifnot("ETAPE 4: colonne code_h introuvable" = "code_h" %in% names(ods))
    
    ods$type   <- norm_type(ods$type)
    ods$code_h <- trimws(as.character(ods$code_h))
    
    # Colonnes pression (Pr_*) issues de la matrice de sensibilité
    pcols <- names(ods)[grep("^pr_", names(ods))]
    stopifnot("ETAPE 4: aucune colonne pression" = length(pcols) > 0)
    
    ods[pcols] <- lapply(ods[pcols], \(x) suppressWarnings(as.numeric(x)))
    
    ods_sens <- ods |> dplyr::filter(.data$type == "sensibilite") |> dplyr::select(-type)
    ods_ic   <- ods |> dplyr::filter(.data$type == "ic")          |> dplyr::select(-type)
    
    stopifnot("ETAPE 4: aucune ligne sensibilite" = nrow(ods_sens) > 0)
    if (nrow(ods_ic) == 0) warning("ETAPE 4: aucune ligne IC", call. = FALSE)
    
    ctx$ods_sensi_hab <- ods_sens
    ctx$ods_ic_hab    <- ods_ic
    
    # --- Filtrer habitats + join sensibilités ------------------------------
    HabBenth_z <- hab0 |>
      dplyr::filter(.data[[hab_spec$id_col]] %in% grid_z$idmesh) |>
      dplyr::transmute(
        idmesh     = .data[[hab_spec$id_col]],
        E_surfmesh = as.numeric(.data[[hab_spec$surfmesh_col]]),
        E_surfmer  = as.numeric(.data[[hab_spec$surfmer_col]]),
        E_surfhab  = tidyr::replace_na(as.numeric(.data[[hab_spec$surfhab_col]]), 0),
        E_habcode  = as.character(.data[[hab_spec$habcode_col]]),
        hab_iq     = suppressWarnings(as.numeric(.data$hab_iq))
      ) |>
      dplyr::left_join(ods_sens[, c("code_h", pcols)], by = c("E_habcode" = "code_h"))
    
    if (any(is.na(HabBenth_z$hab_iq))) {
      warning("ETAPE 4: hab_iq contient des NA. ETAPE 10.3 pénalisera ces habitats (hq01=0).",
              call. = FALSE)
    }
    
    unit_hab <- get_area_unit(config, "HabBenth_z", "E_surfhab")
    unit_mer <- get_area_unit(config, "HabBenth_z", "E_surfmer")
    target_u <- config$units$areas$target
    
    HabBenth_z$E_surfhab <- convert_area(HabBenth_z$E_surfhab, unit_hab, target_u)
    HabBenth_z$E_surfmer <- convert_area(HabBenth_z$E_surfmer, unit_mer, target_u)
    
    bad <- HabBenth_z$hab_iq[
      is.finite(HabBenth_z$hab_iq) & (HabBenth_z$hab_iq < 0 | HabBenth_z$hab_iq > 5)
    ]
    if (length(bad) > 0) {
      stop("ETAPE 4: hab_iq hors bornes [0,5]. Exemples: ",
           paste(head(unique(bad), 10), collapse = ", "))
    }
    
    # --- Agrégation par maille ---------------------------------------------
    temp <- sf::st_drop_geometry(HabBenth_z) |>
      dplyr::group_by(.data$idmesh) |>
      dplyr::summarise(
        surfhab_z  = sum(.data$E_surfhab, na.rm = TRUE),
        surfmesh_z = dplyr::first(.data$E_surfmesh),
        surfmer_z  = dplyr::first(.data$E_surfmer),
        .groups = "drop"
      )
    
    lHabBenth_z <- list(
      idmesh_z   = ctx$col_idz,
      surfhab_z  = tidyr::replace_na(temp$surfhab_z[ match(ctx$col_idz, temp$idmesh)], 0),
      surfmesh_z = temp$surfmesh_z[match(ctx$col_idz, temp$idmesh)],
      surfmer_z  = temp$surfmer_z[ match(ctx$col_idz, temp$idmesh)]
    )
    
    # --- P_evalDEM depuis ctx$typo_press -----------------------------------
    # Remplace la relecture ODS de Typologie_pression : ctx$typo_press est
    # chargé et validé dans step1_init (colonnes : Code_P, parent,
    # mode_calcul, demonstrateur).
    
    typo_press <- ctx$typo_press
    
    stopifnot(
      "ETAPE 4: colonne Code_P manquante dans ctx$typo_press" =
        "Code_P" %in% names(typo_press),
      "ETAPE 4: colonne demonstrateur manquante dans ctx$typo_press" =
        "demonstrateur" %in% names(typo_press),
      "ETAPE 4: Code_P contient des NA ou vides" =
        !any(is.na(typo_press$Code_P) | !nzchar(typo_press$Code_P)),
      "ETAPE 4: doublons Code_P dans ctx$typo_press" =
        !any(duplicated(typo_press$Code_P)),
      "ETAPE 4: demonstrateur invalide (attendu 0 ou 1)" =
        all(typo_press$demonstrateur %in% c(0L, 1L), na.rm = TRUE)
    )
    
    # pressure_ref : table de référence des pressions (pour exports / rapports)
    ref_cols_req <- c("Code_P", "Niveau", "Nom_pression", "demonstrateur")
    ref_cols_missing <- setdiff(ref_cols_req, names(typo_press))
    if (length(ref_cols_missing) > 0) {
      stop("ETAPE 4: colonnes manquantes dans typo_press pour pressure_ref : ",
           paste(ref_cols_missing, collapse = ", "))
    }
    ctx$pressure_ref <- typo_press[, ref_cols_req, drop = FALSE]
    
    # P_evalDEM : named integer vector Code_P -> demonstrateur, filtré sur pcols
    codes_tp <- tolower(as.character(typo_press$Code_P))
    demo_tp  <- as.integer(typo_press$demonstrateur)
    P_evalDEM <- stats::setNames(demo_tp, codes_tp)[pcols]
    
    # Pressions présentes dans la matrice de sensibilité mais absentes de
    # Typologie_pression : signal non bloquant (NA dans P_evalDEM -> exclues)
    press_non_couvertes <- pcols[is.na(P_evalDEM)]
    if (length(press_non_couvertes) > 0) {
      warning(
        "ETAPE 4: ", length(press_non_couvertes),
        " pression(s) de la matrice sensibilité absente(s) de Typologie_pression ",
        "(traitées comme demonstrateur=0) : ",
        paste(press_non_couvertes, collapse = ", "),
        call. = FALSE
      )
      P_evalDEM[is.na(P_evalDEM)] <- 0L
    }
    
    # --- Stockage ----------------------------------------------------------
    ctx$HabBenth_z  <- HabBenth_z
    ctx$lHabBenth_z <- lHabBenth_z
    ctx$P_code_ods  <- pcols
    ctx$sensi_sheet <- sheet_name
    ctx$P_evalDEM   <- P_evalDEM
    
    message("ETAPE 4 OK: ", length(pcols), " pressions (", sheet_name, ")")
  }
  
  # ---- ETAPE 5: Calcul sensibilité cumulée -------------------------------
  {
    stopifnot(
      "ETAPE 5: HabBenth_z manquant" = !is.null(ctx$HabBenth_z),
      "ETAPE 5: P_code_ods manquant" = !is.null(ctx$P_code_ods),
      "ETAPE 5: P_evalDEM manquant" = !is.null(ctx$P_evalDEM)
    )
    
    HabBenth_z <- ctx$HabBenth_z
    P_code <- ctx$P_code_ods
    P_evalDEM <- ctx$P_evalDEM[P_code]
    
    th_sensib <- config$Calc$th_sensib
    stopifnot("ETAPE 5: th_sensib invalide" = th_sensib >= 0 && th_sensib <= 100)
    seuil_nodata <- th_sensib / 100
    
    keep_press <- names(P_evalDEM)[P_evalDEM == 1L]
    drop_press <- names(P_evalDEM)[P_evalDEM == 0L]
    stopifnot("ETAPE 5: aucune pression retenue" = length(keep_press) > 0)
    
    # Construire matrice sensibilités
    Mat_Sensib_Pj <- HabBenth_z |>
      sf::st_drop_geometry() |>
      dplyr::select(E_habcode, idmesh, E_surfmesh, E_surfhab, E_surfmer, 
                    dplyr::any_of(P_code)) |>
      dplyr::select(-dplyr::any_of(drop_press))
    
    # Ajouter pressions manquantes à 99
    pressions_manquantes <- setdiff(keep_press, names(Mat_Sensib_Pj))
    if (length(pressions_manquantes) > 0) {
      Mat_Sensib_Pj[pressions_manquantes] <- 99
      message("ETAPE 5: pressions créées à 99: ", paste(pressions_manquantes, collapse = ", "))
    }
    
    P_code_sel <- intersect(keep_press, names(Mat_Sensib_Pj))
    
    # Calcul pondéré + exclusion nodata
    Mat_Sensib_Pj <- Mat_Sensib_Pj |>
      dplyr::mutate(
        E_surfhab = as.numeric(.data$E_surfhab),
        E_surfmer = as.numeric(.data$E_surfmer),
        w_hab = .data$E_surfhab
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(P_code_sel), ~ suppressWarnings(as.numeric(.x))),
        dplyr::across(dplyr::all_of(P_code_sel), ~ dplyr::na_if(.x, 99)),
        dplyr::across(dplyr::all_of(P_code_sel), ~ .x * .data$w_hab),
        dplyr::across(dplyr::all_of(P_code_sel), 
                      ~ dplyr::if_else(is.na(.x), .data$E_surfhab, 0),
                      .names = "{.col}_nodata")
      ) |>
      dplyr::group_by(.data$idmesh) |>
      dplyr::summarise(
        E_surfmer = dplyr::first(.data$E_surfmer),
        dplyr::across(dplyr::all_of(P_code_sel), ~ sum(.x, na.rm = TRUE)),
        dplyr::across(dplyr::ends_with("_nodata"), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )
    
    # Normalisation + exclusion nodata
    for (p in P_code_sel) {
      nd_col <- paste0(p, "_nodata")
      nd_val <- Mat_Sensib_Pj[[nd_col]]
      sm     <- Mat_Sensib_Pj$E_surfmer
      
      flag <- !is.na(nd_val) & !is.na(sm) & sm > 0 & (nd_val > seuil_nodata * sm)
      val_norm <- ifelse(!is.na(sm) & sm > 0, Mat_Sensib_Pj[[p]] / sm, NA_real_)
      Mat_Sensib_Pj[[p]] <- ifelse(flag, NA_real_, val_norm)
    }
    
    Mat_Sensib_Pj <- Mat_Sensib_Pj |>
      dplyr::select(-dplyr::ends_with("_nodata"))
    
    # Réaligner sur la grille complète (garde NA si inconnu) ----
    Mat_Sensib_Pj <- data.frame(idmesh = as.character(grid_z$idmesh), stringsAsFactors = FALSE) |>
      dplyr::left_join(
        Mat_Sensib_Pj |> dplyr::mutate(idmesh = as.character(.data$idmesh)),
        by = "idmesh")
    
    # Stockage ctx
    ctx$Mat_Sensib_Pj <- Mat_Sensib_Pj
    ctx$P_code_sel <- P_code_sel
    ctx$seuil_nodata <- seuil_nodata
  }
  
  # ---- ETAPE 6: Cartographie des activités (MULTI-TABLES) -------------------
  {
    stopifnot(
      "ETAPE 6: grid_z manquant" = !is.null(grid_z) && inherits(grid_z, "sf"),
      "ETAPE 6: f_lognorm manquant" = exists("f_lognorm", mode = "function"),
      "ETAPE 6: ctx$activity_specs manquant" = !is.null(ctx$activity_specs) && length(ctx$activity_specs) > 0,
      "ETAPE 6: config$act_col_mapping manquant" =
        !is.null(config$act_col_mapping) && length(config$act_col_mapping) > 0
    )
    
    # --- Mapping YAML : intensity_col_name -> Code_A officiel
    map <- config$act_col_mapping
    map <- map[!is.na(map)]
    map <- lapply(map, function(x) trimws(as.character(x)))
    map <- map[vapply(map, function(x) nzchar(x), logical(1))]
    stopifnot("ETAPE 6: act_col_mapping vide après nettoyage" = length(map) > 0)
    
    # Pivot activités
    A_zi_raw  <- data.frame(idmesh = as.character(grid_z$idmesh), stringsAsFactors = FALSE)
    A_missing <- data.frame(idmesh = as.character(grid_z$idmesh), stringsAsFactors = FALSE)
    
    act_table_all <- list()
    
    for (s in seq_along(ctx$activity_specs)) {
      
      spec <- ctx$activity_specs[[s]]
      table_code   <- as.character(spec$table_code)
      obj_name_act <- paste0("A_", table_code)
      
      if (is.null(list_sauv[[obj_name_act]])) {
        stop(sprintf("ETAPE 6: objet activité manquant dans list_sauv: %s", obj_name_act), call. = FALSE)
      }
      
      champ_id_data_A <- as.character(spec$id_col)
      if (is.null(champ_id_data_A) || !nzchar(champ_id_data_A)) {
        stop(sprintf("ETAPE 6: id_col manquant pour table_code=%s", table_code), call. = FALSE)
      }
      
      intensity_cols <- trimws(as.character(spec$intensity_cols))
      intensity_cols <- intensity_cols[nzchar(intensity_cols)]
      if (length(intensity_cols) == 0) {
        stop(sprintf("ETAPE 6: intensity_cols vide pour table_code=%s", table_code), call. = FALSE)
      }
      
      miss <- setdiff(intensity_cols, names(map))
      if (length(miss) > 0) {
        stop(
          sprintf("ETAPE 6: intensity_cols sans mapping (table_code=%s): %s",
                  table_code, paste(miss, collapse = ", ")),
          call. = FALSE
        )
      }
      
      codes_off <- unname(unlist(map[intensity_cols]))
      codes_off <- trimws(as.character(codes_off))
      if (!all(nzchar(codes_off))) {
        stop(sprintf("ETAPE 6: codes officiels vides (table_code=%s)", table_code), call. = FALSE)
      }
      
      # Collision Code_A (toutes tables confondues)
      if (any(codes_off %in% setdiff(names(A_zi_raw), "idmesh"))) {
        dup <- intersect(codes_off, setdiff(names(A_zi_raw), "idmesh"))
        stop("ETAPE 6: collision Code_A (déjà présent dans A_zi): ",
             paste(dup, collapse = ", "),
             ". Corrigez act_col_mapping (codes uniques).",
             call. = FALSE)
      }
      
      act_src <- sf::st_drop_geometry(list_sauv[[obj_name_act]])
      
      miss_db <- setdiff(c(champ_id_data_A, intensity_cols), names(act_src))
      if (length(miss_db) > 0) {
        stop(sprintf("ETAPE 6: colonnes absentes dans la table (%s): %s",
                     obj_name_act, paste(miss_db, collapse = ", ")),
             call. = FALSE)
      }
      
      sub <- act_src |>
        dplyr::select(dplyr::all_of(c(champ_id_data_A, intensity_cols))) |>
        dplyr::rename(idmesh = dplyr::all_of(champ_id_data_A)) |>
        dplyr::mutate(idmesh = as.character(.data$idmesh)) |>
        dplyr::filter(.data$idmesh %in% as.character(grid_z$idmesh))
      
      # Renommer intensity_cols -> codes officiels
      for (k in seq_along(intensity_cols)) {
        names(sub)[names(sub) == intensity_cols[k]] <- codes_off[k]
      }
      
      # masque "missing réel": la ligne existe ET la valeur est NA
      sub_missing <- sub |>
        dplyr::transmute(
          idmesh = .data$idmesh,
          dplyr::across(-idmesh, ~ {
            v <- suppressWarnings(as.numeric(.x))
            is.na(v)
          })
        )
      
      A_zi_raw <- A_zi_raw |>
        dplyr::left_join(sub, by = "idmesh")
      
      A_missing <- A_missing |>
        dplyr::left_join(sub_missing, by = "idmesh")
      
      act_table_all[[length(act_table_all) + 1]] <- data.frame(
        Table_code = table_code,
        Field      = intensity_cols,
        Code1      = codes_off,                 # code activité officiel (ex: A_1_1_1)
        codes      = codes_off,                 # colonne dans A_zi (même nom)
        IQ_col     = paste0(intensity_cols, "_iq"),  # colonne IQ en DB
        stringsAsFactors = FALSE
      )
    }
    
    act_table <- do.call(rbind, act_table_all)
    
    act_cols <- setdiff(names(A_zi_raw), "idmesh")
    
    # absent logique (pas de ligne) => NA après join => missing doit être FALSE
    A_missing[act_cols] <- lapply(A_missing[act_cols], function(x) {
      x <- as.logical(x)
      x[is.na(x)] <- FALSE
      x
    })
    
    # version calcul: NA (absent ou missing) -> 0
    A_zi <- A_zi_raw
    A_zi[act_cols] <- lapply(A_zi[act_cols], function(x) {
      x <- suppressWarnings(as.numeric(x))
      x[!is.finite(x)] <- 0
      x
    })
    
    # --- Normalisation log
    A_zi_lognorm <- A_zi |>
      dplyr::mutate(dplyr::across(-idmesh, ~ f_lognorm(.x))) |>
      dplyr::rename_with(~ paste0(.x, "_norm"), -idmesh)
    
    # --- correspondance Code_A (ODS) -> data_col (colonnes A_zi)
    act_data_corresp <- act_table |>
      dplyr::transmute(Code = .data$Code1, data_col = .data$codes)
    
    # --- IMA
    IMA <- A_zi |>
      dplyr::mutate(IMA_option1 = rowSums(dplyr::pick(-idmesh) > 0, na.rm = TRUE)) |>
      dplyr::select(idmesh, IMA_option1) |>
      dplyr::left_join(A_zi_lognorm, by = "idmesh") |>
      dplyr::mutate(IMA_option2 = rowSums(dplyr::pick(-idmesh, -IMA_option1), na.rm = TRUE)) |>
      dplyr::select(idmesh, IMA_option1, IMA_option2)
    
    denom <- max(IMA$IMA_option2, na.rm = TRUE)
    if (is.finite(denom) && denom > 0) IMA$IMA_option2 <- IMA$IMA_option2 / denom
    
    # --- Stockage
    ctx$act_table         <- act_table
    ctx$A_zi_raw          <- A_zi_raw         # conserve NA originaux
    ctx$A_zi_missing      <- A_missing        # masque NA pour IC
    ctx$A_zi              <- A_zi             # NA -> 0 pour calcul pressions
    ctx$A_zi_lognorm       <- A_zi_lognorm
    ctx$act_data_corresp  <- act_data_corresp
    ctx$IMA               <- IMA
    
    message("ETAPE 6 OK: ", ncol(A_zi) - 1, " activités (multi-tables)")
  }
  
  # ---- ETAPE 7: Cartographie des pressions -------------------------------
  {
    stopifnot(
      "ETAPE 7: A_zi_lognorm manquant"    = !is.null(ctx$A_zi_lognorm),
      "ETAPE 7: act_data_corresp manquant" = !is.null(ctx$act_data_corresp),
      "ETAPE 7: P_code_ods manquant"      = !is.null(ctx$P_code_ods),
      "ETAPE 7: P_evalDEM manquant"       = !is.null(ctx$P_evalDEM),
      "ETAPE 7: typo_press manquant"      = !is.null(ctx$typo_press) && nrow(ctx$typo_press) > 0,
      "ETAPE 7: act_press_path manquant"  = !is.null(ctx$act_press_path) && file.exists(ctx$act_press_path)
    )
    
    stopifnot(
      "ETAPE 7: colonne mode_calcul manquante dans ctx$typo_press" =
        "mode_calcul" %in% names(ctx$typo_press)
    )
    
    # --- Helpers -----------------------------------------------------------
    safe_max <- function(x, default = 0) {
      x <- suppressWarnings(as.numeric(x))
      x <- x[is.finite(x)]
      if (length(x) == 0) default else max(x)
    }
    
    # --- Pressions retenues (Eval_DEM == 1) --------------------------------
    P_code_ods <- ctx$P_code_ods
    P_evalDEM  <- ctx$P_evalDEM
    
    if (!is.null(names(P_evalDEM)) && all(P_code_ods %in% names(P_evalDEM))) {
      P_evalDEM <- P_evalDEM[P_code_ods]
    } else {
      P_evalDEM <- P_evalDEM[seq_along(P_code_ods)]
      names(P_evalDEM) <- P_code_ods
    }
    keep_press <- names(P_evalDEM)[P_evalDEM == 1L]
    stopifnot("ETAPE 7: aucune pression retenue (Eval_DEM=1)" = length(keep_press) > 0)
    
    # --- Exclusion des pressions mode_calcul == 3 --------------------------
    # Ces pressions sont reconstruites de manière récursive en étape 9
    # à partir de leurs sous-composantes. Elles ne sont pas alimentées
    # directement par les activités.
    tp <- ctx$typo_press
    codes_mode3 <- as.character(
      tp$Code_P[!is.na(tp$mode_calcul) & as.integer(tp$mode_calcul) == 3L]
    )
    
    press_exclues_mode3 <- intersect(keep_press, codes_mode3)
    if (length(press_exclues_mode3) > 0) {
      message(
        "ETAPE 7: ", length(press_exclues_mode3),
        " pression(s) mode_calcul=3 exclues du calcul direct (seront reconstruites en étape 9) : ",
        paste(press_exclues_mode3, collapse = ", ")
      )
    }
    
    press_target <- setdiff(keep_press, codes_mode3)
    stopifnot(
      "ETAPE 7: aucune pression exploitable après exclusion mode_calcul=3" =
        length(press_target) > 0
    )
    
    # --- Lire matrice A-P --------------------------------------------------
    ap_raw <- ctx$MatAP_active
    names(ap_raw) <- norm_name(names(ap_raw))
    stopifnot("ETAPE 7: matrice A-P invalide" = is.data.frame(ap_raw) && nrow(ap_raw) > 0)
    
    stopifnot(
      "ETAPE 7: colonne type introuvable"   = "type"   %in% names(ap_raw),
      "ETAPE 7: colonne code_a introuvable" = "code_a" %in% names(ap_raw)
    )
    
    ap_raw$type   <- norm_type(ap_raw$type)
    ap_raw$code_a <- trimws(as.character(ap_raw$code_a))
    ctx$MatAP_df  <- ap_raw
    
    MatAP <- ap_raw |>
      dplyr::filter(.data$type == "lien") |>
      dplyr::mutate(code_a = as.character(.data$code_a)) |>
      as.data.frame()
    
    stopifnot(
      "ETAPE 7: aucune ligne Type=='lien' dans matrice A-P" = nrow(MatAP) > 0
    )
    
    rownames(MatAP) <- MatAP$code_a
    MatAP <- MatAP |>
      dplyr::select(-dplyr::any_of(c("type", "code_a", "nom_activite")))
    
    MatAP <- as.matrix(
      dplyr::mutate(MatAP, dplyr::across(dplyr::everything(),
                                         ~ suppressWarnings(as.numeric(.x))))
    )
    
    press_cols_ap <- colnames(MatAP)[grep("^pr_", colnames(MatAP))]
    press_target  <- intersect(intersect(press_cols_ap, P_code_ods), press_target)
    stopifnot("ETAPE 7: aucune pression exploitable après croisement avec matrice A-P" =
                length(press_target) > 0)
    
    # --- Activités normalisées (matrice) -----------------------------------
    A_df <- ctx$A_zi_lognorm
    stopifnot("ETAPE 7: A_zi_lognorm doit contenir 'idmesh'" = "idmesh" %in% names(A_df))
    
    act_corresp  <- ctx$act_data_corresp
    stopifnot(all(c("Code", "data_col") %in% names(act_corresp)))
    
    act_cols_norm <- setdiff(names(A_df), "idmesh")
    stopifnot("ETAPE 7: aucune colonne activité dans A_zi_lognorm" =
                length(act_cols_norm) > 0)
    
    A_mat <- as.matrix(A_df[, act_cols_norm, drop = FALSE])
    rownames(A_mat) <- as.character(A_df$idmesh)
    
    # --- Calcul pressions --------------------------------------------------
    Mat_Pj_z  <- data.frame(idmesh = grid_z$idmesh, stringsAsFactors = FALSE)
    ampli_max <- data.frame(Code = press_target, ampli_max = 0, stringsAsFactors = FALSE)
    
    for (j in seq_along(press_target)) {
      p <- press_target[j]
      
      MiniMatAP <- data.frame(
        code_a = rownames(MatAP),
        amp    = MatAP[, p, drop = TRUE],
        stringsAsFactors = FALSE
      )
      
      amps <- act_corresp |>
        dplyr::left_join(MiniMatAP, by = c("Code" = "code_a")) |>
        dplyr::group_by(.data$data_col) |>
        dplyr::summarise(
          amp = safe_max(.data$amp, default = 0),
          .groups = "drop"
        )
      
      amp_vec <- setNames(amps$amp, paste0(amps$data_col, "_norm"))
      amp_vec <- amp_vec[is.finite(amp_vec)]
      
      ampli_max$ampli_max[j] <- if (length(amp_vec) == 0) 0 else max(amp_vec)
      
      cols_amp <- intersect(names(amp_vec), colnames(A_mat))
      
      if (length(cols_amp) == 0) {
        Mat_Pj_z[[p]] <- 0
        next
      }
      
      subA <- A_mat[, cols_amp, drop = FALSE]
      subA[is.na(subA)] <- 0
      pv <- as.numeric(subA %*% amp_vec[cols_amp])
      
      pv_by_idmesh  <- setNames(pv, rownames(subA))
      Mat_Pj_z[[p]] <- as.numeric(pv_by_idmesh[as.character(Mat_Pj_z$idmesh)])
    }
    
    # --- Normalisation log -------------------------------------------------
    Mat_Pj_norm_z <- Mat_Pj_z |>
      dplyr::mutate(dplyr::across(-idmesh, ~ f_lognorm(.x))) |>
      dplyr::rename_with(~ paste0(.x, "_norm"), -idmesh)
    
    # --- IPC ---------------------------------------------------------------
    IPC_z <- Mat_Pj_z |>
      dplyr::mutate(IPC_option1 = rowSums(dplyr::pick(-idmesh) > 0, na.rm = TRUE)) |>
      dplyr::transmute(idmesh = .data$idmesh, IPC_option1) |>
      dplyr::left_join(
        Mat_Pj_norm_z |>
          dplyr::mutate(IPC_option2 = rowSums(dplyr::pick(-idmesh), na.rm = TRUE)) |>
          dplyr::select(idmesh, IPC_option2),
        by = "idmesh"
      )
    
    # --- Stockage ----------------------------------------------------------
    ctx$Mat_Pj_z      <- Mat_Pj_z
    ctx$Mat_Pj_norm_z <- Mat_Pj_norm_z
    ctx$IPC_z         <- IPC_z
    ctx$press_target  <- press_target
    ctx$P_code_sel    <- press_target
    
    # P_code_sel : pressions feuilles effectivement calculées
    # (consommé par étapes 9, 10, 11)
    ctx$P_code_sel <- press_target
    
    message("ETAPE 7 OK: ", length(press_target), " pressions feuilles calculées",
            if (length(press_exclues_mode3) > 0)
              paste0(" (", length(press_exclues_mode3), " mode_calcul=3 exclues)")
            else "")
  }
  # ---- ETAPE 8: Risque d'exposition --------------------------------------
  {
    surfhab_z <- ctx$lHabBenth_z$surfhab_z
    Mat_Pj_z <- ctx$Mat_Pj_z
    press_cols <- setdiff(names(Mat_Pj_z), "idmesh")
    
    Mat_REX_PjE1_z <- data.frame(idmesh = grid_z$idmesh, surfhab = surfhab_z) |>
      dplyr::left_join(Mat_Pj_z, by = "idmesh") |>
      dplyr::mutate(dplyr::across(dplyr::all_of(press_cols), ~ .x * .data$surfhab)) |>
      dplyr::rename_with(~ paste0("REX_E1_", .x), dplyr::all_of(press_cols)) |>
      dplyr::select(-surfhab)
    
    REXC_E1 <- data.frame(
      idmesh = Mat_REX_PjE1_z$idmesh,
      REXC_E1 = rowSums(Mat_REX_PjE1_z[, -1], na.rm = TRUE)
    )
    
    if (exists("Plot_VARz_grid2D", mode = "function")) {
      Plot_VARz_grid2D(REXC_E1,grid_z,config,"REXC_E1","REX pressions concomitantes sur E1",outputsdir = ctx$maps_dir)
    }
    
    ctx$Mat_REX_PjE1_z <- Mat_REX_PjE1_z
    ctx$REXC_E1 <- REXC_E1
    
    message("ETAPE 8 OK: REX E1 calculé pour ", length(press_cols), " pressions")
  }
  
  # ---- ETAPE 9: Risque d'effets cumulés (avec récursion hiérarchique) ----
  {
    stopifnot(
      "ETAPE 9: Mat_Sensib_Pj manquant"  = !is.null(ctx$Mat_Sensib_Pj),
      "ETAPE 9: press_target manquant"   = !is.null(ctx$press_target) && length(ctx$press_target) > 0,
      "ETAPE 9: typo_press manquant"     = !is.null(ctx$typo_press) && nrow(ctx$typo_press) > 0,
      "ETAPE 9: f_norm manquant"         = exists("f_norm",    mode = "function"),
      "ETAPE 9: f_lognorm manquant"      = exists("f_lognorm", mode = "function")
    )
    
    stopifnot(
      "ETAPE 9: colonnes manquantes dans typo_press" =
        all(c("Code_P", "parent", "mode_calcul", "demonstrateur") %in% names(ctx$typo_press))
    )
    
    # ------------------------------------------------------------------ #
    #  9.1  Scores feuilles : f_lognorm(P_j) × S_j                       #
    # ------------------------------------------------------------------ #
    
    Sens_in <- ctx$Mat_Sensib_Pj |>
      sf::st_drop_geometry() |>
      dplyr::select(-dplyr::any_of("E_surfmer")) |>
      dplyr::mutate(dplyr::across(-idmesh, ~ ifelse(is.na(.x), 0, .x)))
    
    Mat_Sensib_Pj_in <- f_norm(Sens_in)
    
    press_feuilles <- ctx$press_target   # mode_calcul 1/2, demonstrateur=1
    
    # Vérification : toutes les feuilles doivent être dans la matrice de sensibilité
    feuilles_sans_sensib <- setdiff(press_feuilles,
                                    setdiff(names(Mat_Sensib_Pj_in), "idmesh"))
    if (length(feuilles_sans_sensib) > 0) {
      stop(
        "ETAPE 9: ", length(feuilles_sans_sensib),
        " pression(s) feuille(s) absente(s) de la matrice de sensibilité : ",
        paste(feuilles_sans_sensib, collapse = ", ")
      )
    }
    
    # Colonnes pression pondérées (Mat_Pj_pondere utilise le suffixe _norm)
    press_feuilles_norm <- paste0(press_feuilles, "_norm")
    press_feuilles_norm <- intersect(press_feuilles_norm, names(ctx$Mat_Pj_norm_z))
    
    if (length(press_feuilles_norm) == 0) {
      stop("ETAPE 9: aucune colonne pression disponible dans Mat_Pj_norm_z")
    }
    
    # Base commune idmesh
    idmesh_vec <- as.character(grid_z$idmesh)
    
    # Matrice sensibilités feuilles alignée sur la grille
    sens_feuilles <- Mat_Sensib_Pj_in |>
      dplyr::mutate(idmesh = as.character(.data$idmesh)) |>
      dplyr::select(idmesh, dplyr::all_of(press_feuilles))
    
    # Matrice pressions pondérées feuilles alignée sur la grille
    pres_feuilles <- ctx$Mat_Pj_norm_z |>
      dplyr::mutate(idmesh = as.character(.data$idmesh)) |>
      dplyr::select(idmesh, dplyr::all_of(press_feuilles_norm))
    
    # score_tbl : une colonne par pression (feuilles + nœuds composés)
    # colonnes nommées par Code_P (sans suffixe _norm)
    score_tbl <- data.frame(idmesh = idmesh_vec, stringsAsFactors = FALSE)
    
    for (p in press_feuilles) {
      p_norm <- paste0(p, "_norm")
      
      s_vec <- sens_feuilles[[p]][match(idmesh_vec, sens_feuilles$idmesh)]
      p_vec <- pres_feuilles[[p_norm]][match(idmesh_vec, pres_feuilles$idmesh)]
      
      s_vec[!is.finite(s_vec)] <- NA_real_
      p_vec[!is.finite(p_vec)] <- 0
      
      score_tbl[[p]] <- p_vec * s_vec
    }
    
    # ------------------------------------------------------------------ #
    #  9.2  Récursion : nœuds composés (mode_calcul == 3)                 #
    # ------------------------------------------------------------------ #
    
    tp <- ctx$typo_press
    tp$Code_P <- tolower(as.character(tp$Code_P))
    tp$parent <- tolower(as.character(tp$parent))
    
    tp_dem <- tp[!is.na(tp$demonstrateur) & as.integer(tp$demonstrateur) == 1L, ]
    
    # Nœuds composés demonstrateur=1
    noeuds_composes <- tp_dem[
      !is.na(tp_dem$mode_calcul) & as.integer(tp_dem$mode_calcul) == 3L,
    ]
    
    if (nrow(noeuds_composes) > 0) {
      
      # Trier par profondeur décroissante : les enfants avant les parents
      # La profondeur est inférée depuis le Code_P (nombre de segments séparés par _)
      # ex: Pr_P1_3_1 -> 4 segments -> profondeur 4
      get_depth <- function(code) {
        lengths(regmatches(code, gregexpr("_", code))) + 1L
      }
      
      noeuds_composes <- noeuds_composes[
        order(get_depth(as.character(noeuds_composes$Code_P)), decreasing = TRUE),
      ]
      
      for (i in seq_len(nrow(noeuds_composes))) {
        
        code_parent <- as.character(noeuds_composes$Code_P[i])
        
        # Enfants directs : pressions dont parent == code_parent, demonstrateur=1
        enfants <- as.character(
          tp_dem$Code_P[
            !is.na(tp_dem$parent) &
              as.character(tp_dem$parent) == code_parent
          ]
        )
        
        # Seuls les enfants déjà calculés (présents dans score_tbl) sont retenus
        enfants_ok      <- intersect(enfants, names(score_tbl))
        enfants_exclus  <- setdiff(enfants, enfants_ok)
        
        if (length(enfants_exclus) > 0) {
          message(
            "ETAPE 9: nœud ", code_parent, " — ",
            length(enfants_exclus), " enfant(s) absent(s) de score_tbl (exclus par demonstrateur=0 ?) : ",
            paste(enfants_exclus, collapse = ", ")
          )
        }
        
        if (length(enfants_ok) == 0) {
          message(
            "ETAPE 9: nœud composé ", code_parent,
            " sans enfants calculés — score fixé à NA."
          )
          score_tbl[[code_parent]] <- NA_real_
          next
        }
        
        # Somme des scores enfants puis normalisation spatiale
        mat_enfants <- as.matrix(score_tbl[, enfants_ok, drop = FALSE])
        mat_enfants[!is.finite(mat_enfants)] <- NA_real_
        
        somme_enfants <- rowSums(mat_enfants, na.rm = TRUE)
        # Maille où TOUS les enfants sont NA -> NA (pas de données)
        tout_na <- apply(mat_enfants, 1, function(r) all(is.na(r)))
        somme_enfants[tout_na] <- NA_real_
        
        score_tbl[[code_parent]] <- f_lognorm(somme_enfants)
      }
    }
    
    # ------------------------------------------------------------------ #
    #  9.3  REFC : somme des racines demonstrateur=1, normalisée          #
    # ------------------------------------------------------------------ #
    
    # Racines = nœuds demonstrateur=1 sans parent (parent NA ou vide)
    racines <- as.character(
      tp_dem$Code_P[
        is.na(tp_dem$parent) | !nzchar(trimws(as.character(tp_dem$parent)))
      ]
    )
    
    racines_ok     <- intersect(racines, names(score_tbl))
    racines_exclus <- setdiff(racines, names(score_tbl))
    
    if (length(racines_exclus) > 0) {
      message(
        "ETAPE 9: ", length(racines_exclus),
        " racine(s) absente(s) de score_tbl (aucun enfant calculé ?) : ",
        paste(racines_exclus, collapse = ", ")
      )
    }
    
    stopifnot(
      "ETAPE 9: aucune racine disponible pour calculer le REFC" =
        length(racines_ok) > 0
    )
    
    mat_racines   <- as.matrix(score_tbl[, racines_ok, drop = FALSE])
    mat_racines[!is.finite(mat_racines)] <- NA_real_
    
    somme_racines <- rowSums(mat_racines, na.rm = TRUE)
    tout_na_r     <- apply(mat_racines, 1, function(r) all(is.na(r)))
    somme_racines[tout_na_r] <- NA_real_
    
    # Normalisation finale
    REFC_E1_vec <- f_lognorm(somme_racines)
    
    REFC_E1 <- data.frame(
      idmesh  = idmesh_vec,
      REFC_E1 = REFC_E1_vec,
      stringsAsFactors = FALSE
    )
    
    # ------------------------------------------------------------------ #
    #  9.4  Tableaux intermédiaires pour étape 10 / exports               #
    # ------------------------------------------------------------------ #
    
    # Mat_REF_PjE1_z : scores feuilles renommés ref_e1_* (compatibilité étape 10)
    ref_cols <- paste0("ref_e1_", press_feuilles)
    Mat_REF_PjE1_z <- score_tbl |>
      dplyr::select(idmesh, dplyr::all_of(press_feuilles)) |>
      dplyr::rename_with(~ paste0("ref_e1_", .x), -idmesh)
    names(Mat_REF_PjE1_z) <- c("idmesh", ref_cols)
    
    # Carte optionnelle
    if (exists("Plot_VARz_grid2D", mode = "function")) {
      Plot_VARz_grid2D(
        REFC_E1, grid_z, config,
        "REFC_E1", "REF pressions concomitantes sur E1",
        outputsdir = ctx$maps_dir
      )
    }
    
    # ------------------------------------------------------------------ #
    #  9.5  Stockage                                                       #
    # ------------------------------------------------------------------ #
    
    ctx$Mat_Sensib_Pj_in <- Mat_Sensib_Pj_in
    ctx$Mat_REF_PjE1_z   <- Mat_REF_PjE1_z
    ctx$REFC_E1          <- REFC_E1
    
    # score_tbl : feuilles + nœuds composés, toutes pressions demonstrateur=1
    # Utilisé pour traçabilité et futurs graphiques par niveau hiérarchique
    ctx$score_tbl        <- score_tbl
    
    message(
      "ETAPE 9 OK: REFC_E1 calculé — ",
      length(press_feuilles), " feuille(s), ",
      if (nrow(noeuds_composes) > 0) nrow(noeuds_composes) else 0,
      " nœud(s) composé(s), ",
      length(racines_ok), " racine(s)"
    )
  }
  
  # ---- ETAPE 10: Indices de confiance ------------------------------------
  {
    message("ETAPE 10: Calcul des indices de confiance...")
    
    # ---- Guards minimaux ---------------------------------------------------
    MatAP_df   <- ctx$MatAP_df
    press_cols <- ctx$P_code_sel
    
    if (is.null(MatAP_df) || !is.data.frame(MatAP_df) || nrow(MatAP_df) == 0 || length(press_cols) == 0) {
      warning("ETAPE 10: Données insuffisantes pour IC. Skipped.", call. = FALSE)
      ctx$IC_available <- FALSE
    } else {
      
      press_cols_ap <- intersect(press_cols, names(MatAP_df)[grep("^pr_", tolower(names(MatAP_df)))])
      if (length(press_cols_ap) == 0) {
        warning("ETAPE 10: Aucune colonne Pr_* commune. IC non calculé.", call. = FALSE)
        ctx$IC_available <- FALSE
      } else {
        
        # ---- 10.1 MatAP_lien + MatAP_IC --------------------------------------
        
        # MatAP_df est déjà normalisé en minuscules depuis l'étape 7
        stopifnot("ETAPE 10: colonne type introuvable dans MatAP_df"   = "type"   %in% names(MatAP_df))
        stopifnot("ETAPE 10: colonne code_a introuvable dans MatAP_df" = "code_a" %in% names(MatAP_df))
        
        MatAP_df$code_a <- trimws(as.character(MatAP_df$code_a))
        MatAP_df$type <- norm_type(MatAP_df$type)
        
        df_lien <- as.data.frame(MatAP_df[MatAP_df$type == "lien", , drop = FALSE])
        df_ic   <- as.data.frame(MatAP_df[MatAP_df$type == "ic",   , drop = FALSE])
        
        stopifnot(
          "ETAPE 10: Aucune ligne lien" = nrow(df_lien) > 0,
          "ETAPE 10: Aucune ligne IC"   = nrow(df_ic) > 0
        )
        
        df_lien$code_a <- as.character(df_lien$code_a)
        df_ic$code_a   <- as.character(df_ic$code_a)
        
        # Guard strict: mêmes ensembles de code_a (sinon erreur ODS)
        codes_lien <- sort(unique(df_lien$code_a))
        codes_ic   <- sort(unique(df_ic$code_a))
        
        stopifnot(
          "ETAPE 10: code_a manquant/vide dans df_lien" = !any(is.na(codes_lien) | !nzchar(codes_lien)),
          "ETAPE 10: code_a manquant/vide dans df_ic"   = !any(is.na(codes_ic)   | !nzchar(codes_ic)),
          "ETAPE 10: Ensembles code_a differents entre lien et IC (ODS incoherent)" = identical(codes_lien, codes_ic)
        )
        
        # Ordre stable
        A_codes <- codes_lien
        
        # Indexation par code_a
        rownames(df_lien) <- df_lien$code_a
        rownames(df_ic)   <- df_ic$code_a
        
        # Colonnes pressions (déjà calculées ailleurs)
        stopifnot(
          "ETAPE 10: press_cols_ap manquant/vide" = exists("press_cols_ap") && length(press_cols_ap) > 0
        )
        
        # MatAP_lien : valeurs [0,1], NA conservés (ND/NA/vide/null -> NA)
        MatAP_lien <- df_lien[A_codes, press_cols_ap, drop = FALSE]
        MatAP_lien <- as.data.frame(lapply(MatAP_lien, function(x) {
          x <- trimws(as.character(x))
          x <- gsub("\u00A0", "", x, fixed = TRUE)
          x <- gsub(",", ".", x, fixed = TRUE)
          x <- tolower(x)
          x[x %in% c("", "nd", "na", "null")] <- NA_character_
          out <- suppressWarnings(as.numeric(x))
          out
        }))
        rownames(MatAP_lien) <- A_codes
        
        # MatAP_IC : conversion robuste 0..1 (ND/NA/vide/null/non-num -> 0)
        MatAP_IC <- df_ic[A_codes, press_cols_ap, drop = FALSE]
        MatAP_IC <- as.data.frame(lapply(MatAP_IC, function(x) {
          x <- trimws(as.character(x))
          x <- gsub("\u00A0", "", x, fixed = TRUE)
          x <- gsub(",", ".", x, fixed = TRUE)
          x <- tolower(x)
          x[x %in% c("nd", "na", "", "null")] <- "0"
          out <- suppressWarnings(as.numeric(x))
          out[!is.finite(out)] <- 0
          out <- pmin(1, pmax(0, out))
          out
        }))
        rownames(MatAP_IC) <- A_codes
        
        # Lien manquant => IC = 0
        MatAP_IC[is.na(MatAP_lien)] <- 0
        
        # Contrôles
        x  <- as.matrix(MatAP_lien)
        ic <- as.matrix(MatAP_IC)
        
        stopifnot(
          "ETAPE 10: MatAP_lien invalide (vide)" = nrow(x) > 0 && ncol(x) > 0,
          "ETAPE 10: MatAP_lien contient des valeurs hors [0,1]" =
            all(is.na(x) | (is.finite(x) & x >= 0 & x <= 1)),
          "ETAPE 10: MatAP_IC contient NA" = !anyNA(ic),
          "ETAPE 10: MatAP_IC hors bornes [0,1]" = all(ic >= 0 & ic <= 1)
        )
        
        ctx$MatAP_lien <- MatAP_lien
        ctx$MatAP_IC   <- MatAP_IC
        
        # ---- 10.2 IQ_A_mean + IC_AP_mean ----
        
        stopifnot(
          "ETAPE 10.2: A_zi manquant" = !is.null(ctx$A_zi),
          "ETAPE 10.2: A_zi_missing manquant" = !is.null(ctx$A_zi_missing),
          "ETAPE 10.2: act_table manquant" = !is.null(ctx$act_table),
          "ETAPE 10.2: MatAP_IC manquant" = !is.null(ctx$MatAP_IC)
        )
        
        A_zi <- ctx$A_zi; A_zi$idmesh <- as.character(A_zi$idmesh)
        A_miss <- ctx$A_zi_missing; A_miss$idmesh <- as.character(A_miss$idmesh)
        
        A_aligned <- data.frame(idmesh = as.character(grid_z$idmesh), stringsAsFactors = FALSE) |>
          dplyr::left_join(A_zi, by = "idmesh")
        
        A_miss_aligned <- data.frame(idmesh = as.character(grid_z$idmesh), stringsAsFactors = FALSE) |>
          dplyr::left_join(A_miss, by = "idmesh")
        
        act_cols <- setdiff(names(A_aligned), "idmesh")
        stopifnot("ETAPE 10.2: aucune activité dans A_zi" = length(act_cols) > 0)
        
        # absent logique => missing FALSE
        A_miss_aligned[act_cols] <- lapply(A_miss_aligned[act_cols], function(x) {
          x <- as.logical(x); x[is.na(x)] <- FALSE; x
        })
        
        # construire A_iq_z (multi-tables) puis aligner
        A_iq_z <- NULL
        for (spec in ctx$activity_specs) {
          obj_name_act <- paste0("A_", spec$table_code)
          if (is.null(ctx$list_sauv[[obj_name_act]])) next
          
          act_pg <- sf::st_drop_geometry(ctx$list_sauv[[obj_name_act]])
          iq_cols <- paste0(spec$intensity_cols, "_iq")
          iq_cols_present <- intersect(iq_cols, names(act_pg))
          if (length(iq_cols_present) == 0) next
          
          tmp <- act_pg |>
            dplyr::select(dplyr::all_of(c(spec$id_col, iq_cols_present))) |>
            dplyr::rename(idmesh = dplyr::all_of(spec$id_col)) |>
            dplyr::mutate(idmesh = as.character(.data$idmesh))
          
          if (is.null(A_iq_z)) A_iq_z <- tmp else A_iq_z <- dplyr::full_join(A_iq_z, tmp, by = "idmesh")
        }
        
        A_iq_aligned <- data.frame(idmesh = as.character(grid_z$idmesh), stringsAsFactors = FALSE)
        if (!is.null(A_iq_z)) {
          A_iq_z$idmesh <- as.character(A_iq_z$idmesh)
          A_iq_aligned <- A_iq_aligned |> dplyr::left_join(A_iq_z, by = "idmesh")
        }
        
        # mapping Code_A (col A_zi) -> IQ_col (col DB)
        map_iq <- ctx$act_table |>
          dplyr::distinct(Code1, IQ_col) |>
          dplyr::filter(.data$Code1 %in% act_cols)
        
        iq_col_by_code <- stats::setNames(as.character(map_iq$IQ_col), as.character(map_iq$Code1))
        
        # IQ_A_mean : moyenne des IQ de toutes les activités du cas d'étude
        IQ_A_mean <- vapply(seq_len(nrow(A_aligned)), function(i) {
          
          miss_row <- as.logical(A_miss_aligned[i, act_cols, drop = TRUE])
          miss_row[is.na(miss_row)] <- FALSE
          
          iq_vec <- rep(1, length(act_cols))
          names(iq_vec) <- act_cols
          
          # missing (NA) réel => 0
          iq_vec[miss_row] <- 0
          
          # si non-missing et IQ dispo => iq/5
          for (a in act_cols[!miss_row]) {
            iqc <- iq_col_by_code[[a]]
            if (!is.null(iqc) && iqc %in% names(A_iq_aligned)) {
              v <- suppressWarnings(as.numeric(A_iq_aligned[i, iqc, drop = TRUE]))
              if (!is.finite(v)) v <- 5
              v <- pmin(5, pmax(0, v))
              iq_vec[a] <- v / 5
            }
          }
          
          mean(iq_vec)
          
        }, numeric(1))
        
        ctx$IQ_A_mean <- data.frame(idmesh = grid_z$idmesh, IQ_A_mean = IQ_A_mean)
        
        # IC_AP_mean (uniquement selon activités présentes; si aucune -> 1)
        MatAP_IC <- ctx$MatAP_IC
        press_cols_ap <- intersect(colnames(MatAP_IC), ctx$P_code_sel)
        
        IC_AP_mean <- vapply(seq_len(nrow(A_aligned)), function(i) {
          
          vals <- suppressWarnings(as.numeric(A_aligned[i, act_cols, drop = TRUE]))
          vals[!is.finite(vals)] <- 0
          acts_present <- act_cols[vals > 0]
          if (length(acts_present) == 0) return(1)
          
          per_act <- vapply(acts_present, function(a) {
            if (!(a %in% rownames(MatAP_IC))) return(NA_real_)
            x <- suppressWarnings(as.numeric(MatAP_IC[a, press_cols_ap, drop = TRUE]))
            x <- x[is.finite(x)]
            if (length(x) == 0) NA_real_ else mean(x)
          }, numeric(1))
          
          per_act <- per_act[is.finite(per_act)]
          if (length(per_act) == 0) 1 else mean(per_act)
          
        }, numeric(1))
        
        ctx$IC_AP_mean <- data.frame(idmesh = grid_z$idmesh, IC_AP_mean = IC_AP_mean)
        ctx$A_iq_aligned   <- A_iq_aligned
        ctx$iq_col_by_code <- iq_col_by_code
        ctx$A_miss_aligned <- A_miss_aligned
        
        # ---- 10.3 IQ_H_mean (qualité données habitats) : hab impliqués + pondération surfacique ----
        stopifnot(
          "IQ_H_mean: HabBenth_z manquant" = !is.null(ctx$HabBenth_z),
          "IQ_H_mean: hab_iq manquant"     = "hab_iq" %in% names(ctx$HabBenth_z),
          "IQ_H_mean: E_surfhab manquant"  = "E_surfhab" %in% names(ctx$HabBenth_z),
          "IQ_H_mean: idmesh manquant"     = "idmesh" %in% names(ctx$HabBenth_z)
        )
        
        hab_df <- ctx$HabBenth_z |>
          sf::st_drop_geometry() |>
          dplyr::transmute(
            idmesh   = as.character(.data$idmesh),
            surfhab  = suppressWarnings(as.numeric(.data$E_surfhab)),
            hab_iq   = suppressWarnings(as.numeric(.data$hab_iq))
          )
        
        IQ_H_mean <- hab_df |>
          dplyr::group_by(.data$idmesh) |>
          dplyr::summarise(
            IQ_H_mean = {
              s <- .data$surfhab
              q <- .data$hab_iq
              
              # garder seulement habitats réellement présents
              keep <- is.finite(s) & s > 0
              
              if (!any(keep)) {
                0  # aucun habitat présent => pas de données habitats
              } else {
                
                s <- s[keep]
                q <- q[keep]
                
                # hab_iq manquant/non-num => 0 (donnée manquante réelle)
                q[!is.finite(q)] <- 0
                q <- pmin(5, pmax(0, q)) / 5  # 0..1
                
                w <- s / sum(s)
                sum(w * q)
              }
            },
            .groups = "drop"
          )
        
        # réaligner sur toutes les mailles (absent => 0)
        ctx$IQ_H_mean <- data.frame(idmesh = as.character(grid_z$idmesh), stringsAsFactors = FALSE) |>
          dplyr::left_join(IQ_H_mean, by = "idmesh") |>
          dplyr::mutate(IQ_H_mean = dplyr::if_else(is.na(.data$IQ_H_mean), 0, .data$IQ_H_mean))
        
        # ---- 10.4 IC_HP_mean (liens hab/press) : pressions impliquées + pondération surfacique ----
        stopifnot(
          "IC_HP_mean: HabBenth_z manquant"  = !is.null(ctx$HabBenth_z),
          "IC_HP_mean: E_surfhab manquant"   = "E_surfhab" %in% names(ctx$HabBenth_z),
          "IC_HP_mean: E_habcode manquant"   = "E_habcode" %in% names(ctx$HabBenth_z),
          "IC_HP_mean: idmesh manquant"      = "idmesh" %in% names(ctx$HabBenth_z),
          "IC_HP_mean: sensi_path invalide"  = !is.null(ctx$sensi_path) && file.exists(ctx$sensi_path),
          "IC_HP_mean: sensi_sheet invalide" = !is.null(ctx$sensi_sheet) && nzchar(ctx$sensi_sheet),
          "IC_HP_mean: P_code_sel manquant"  = !is.null(ctx$P_code_sel) && length(ctx$P_code_sel) > 0
        )
        
        # Habitats présents par maille (avec surface)
        hab_cells <- ctx$HabBenth_z |>
          sf::st_drop_geometry() |>
          dplyr::transmute(
            idmesh   = as.character(.data$idmesh),
            code_hab = as.character(.data$E_habcode),
            surfhab  = suppressWarnings(as.numeric(.data$E_surfhab))
          )
        
        # lecture ODS sensibilités (même sheet que sensibilités)
        ods_sensi <- readODS::read_ods(ctx$sensi_path, sheet = ctx$sensi_sheet)
        stopifnot("IC_HP_mean: lecture ODS sensi échouée" = is.data.frame(ods_sensi) && nrow(ods_sensi) > 0)
        
        names(ods_sensi) <- norm_name(names(ods_sensi))
        
        stopifnot("IC_HP_mean: colonne code_h introuvable"  = "code_h" %in% names(ods_sensi))
        stopifnot("IC_HP_mean: colonne type introuvable"    = "type"   %in% names(ods_sensi))
        
        ods_sensi$type <- norm_type(ods_sensi$type)
        ods_sensi$code_h <- trimws(as.character(ods_sensi$code_h))
        
        # Pressions "utilisées" (Eval_DEM==1) = ctx$P_code_sel, et présentes dans l'ODS
        pcols <- intersect(names(ods_sensi)[grep("^pr_", names(ods_sensi))], ctx$P_code_sel)
        stopifnot("IC_HP_mean: aucune colonne pression commune (ODS vs P_code_sel)" = length(pcols) > 0)
        
        # Matrice IC habitat-pression
        ods_ic <- ods_sensi |>
          dplyr::filter(.data$type == "ic") |>
          dplyr::select(dplyr::all_of(c("code_h", pcols))) |>
          dplyr::mutate(
            code_h = as.character(.data$code_h),
            dplyr::across(dplyr::all_of(pcols), ~ suppressWarnings(as.numeric(.x)))
          )
        
        stopifnot("IC_HP_mean: aucune ligne Type=='IC' dans ODS" = nrow(ods_ic) > 0)
        
        # IC moyen par habitat sur les pressions utilisées
        ic_by_hab <- ods_ic |>
          dplyr::rowwise() |>
          dplyr::mutate(
            IC_hab = {
              x <- c_across(dplyr::all_of(pcols))
              # manquant/non-num = 0 (conservateur)
              x[!is.finite(x)] <- 0
              x <- pmin(5, pmax(0, x))
              mean(x / 5)
            }
          ) |>
          dplyr::ungroup() |>
          dplyr::select(code_h, IC_hab)
        
        # Join habitats de maille -> IC_hab, puis pondération par surfhab
        IC_HP_mean <- hab_cells |>
          dplyr::left_join(ic_by_hab, by = c("code_hab" = "code_h")) |>
          dplyr::group_by(.data$idmesh) |>
          dplyr::summarise(
            IC_HP_mean = {
              s <- .data$surfhab
              keep <- is.finite(s) & s > 0
              if (!any(keep)) {
                0
              } else {
                s <- s[keep]
                x <- .data$IC_hab[keep]
                # si IC_hab NA (habitat absent de la matrice) => 0
                x[!is.finite(x)] <- 0
                w <- s / sum(s)
                sum(w * x)
              }
            },
            .groups = "drop"
          )
        
        # réaligner sur la grille complète (absent => 0)
        ctx$IC_HP_mean <- data.frame(idmesh = as.character(grid_z$idmesh), stringsAsFactors = FALSE) |>
          dplyr::left_join(IC_HP_mean, by = "idmesh") |>
          dplyr::mutate(IC_HP_mean = dplyr::if_else(is.na(.data$IC_HP_mean), 0, .data$IC_HP_mean))
        
        # ---- 10.5 IC global = moyenne pondérée (Shapley) des 4 indicateurs ----
        stopifnot(
          "STUB IC: IQ_A_mean manquant"  = !is.null(ctx$IQ_A_mean)  && all(c("idmesh","IQ_A_mean")  %in% names(ctx$IQ_A_mean)),
          "STUB IC: IC_AP_mean manquant" = !is.null(ctx$IC_AP_mean) && all(c("idmesh","IC_AP_mean") %in% names(ctx$IC_AP_mean)),
          "STUB IC: IQ_H_mean manquant"  = !is.null(ctx$IQ_H_mean)  && all(c("idmesh","IQ_H_mean")  %in% names(ctx$IQ_H_mean)),
          "STUB IC: IC_HP_mean manquant" = !is.null(ctx$IC_HP_mean) && all(c("idmesh","IC_HP_mean") %in% names(ctx$IC_HP_mean))
        )
        
        tmp4 <- data.frame(idmesh = as.character(grid_z$idmesh), stringsAsFactors = FALSE) |>
          dplyr::left_join(dplyr::mutate(ctx$IQ_A_mean,  idmesh = as.character(.data$idmesh)), by = "idmesh") |>
          dplyr::left_join(dplyr::mutate(ctx$IC_AP_mean, idmesh = as.character(.data$idmesh)), by = "idmesh") |>
          dplyr::left_join(dplyr::mutate(ctx$IQ_H_mean,  idmesh = as.character(.data$idmesh)), by = "idmesh") |>
          dplyr::left_join(dplyr::mutate(ctx$IC_HP_mean, idmesh = as.character(.data$idmesh)), by = "idmesh")
        
        # --- Poids Shapley phi_IC ------------------------------------------------
        # Valeurs issues du sweep Shapley (60 permutations, 10 répétitions) renormalisées à somme=1
        # Si besoin : poids égaux (0.25 chacun) = équivalent à la moyenne simple.
        phi_IC <- c(
          IQ_A  = 0.46,  # contribution X5_var_act - shapley 
          IC_AP = 0.03,  # contribution X4_var_ccr_AP - shapley 
          IQ_H  = 0.16,  # contribution X8_var_hab - shapley 
          IC_HP = 0.35   # contribution X3_var_sensi - shapley 
        )
        stopifnot(
          "ETAPE 10.5: phi_IC doit sommer a 1" = abs(sum(phi_IC) - 1) < 1e-6,
          "ETAPE 10.5: phi_IC doit etre >= 0"   = all(phi_IC >= 0)
        )
        ctx$phi_IC <- phi_IC
        
        ctx$IC_global <- tmp4 |>
          dplyr::rowwise() |>
          dplyr::mutate(
            IC = {
              v <- c(IQ_A = .data$IQ_A_mean, IC_AP = .data$IC_AP_mean,
                     IQ_H = .data$IQ_H_mean, IC_HP = .data$IC_HP_mean)
              ok <- is.finite(v)
              if (!any(ok)) NA_real_ else stats::weighted.mean(v[ok], phi_IC[ok])
            }
          ) |>
          dplyr::ungroup() |>
          dplyr::select(idmesh, IC)
        
        ctx$IC_global$IC <- pmin(1, pmax(0, ctx$IC_global$IC))
        ctx$IC_available <- TRUE
        
        # ---- Export carte IC_global (optionnel) -----------------------------------
        if (isTRUE(config$Outputs$write_maps) &&
            isTRUE(ctx$IC_available) &&
            !is.null(ctx$IC_global) &&
            exists("Plot_VARz_grid2D", mode = "function")) {
          
          stopifnot(
            "ETAPE 10: ctx$IC_global doit contenir 'idmesh'" = "idmesh" %in% names(ctx$IC_global),
            "ETAPE 10: ctx$IC_global doit contenir 'IC'"   = "IC"   %in% names(ctx$IC_global),
            "ETAPE 10: grid_z doit contenir 'idmesh'"      = "idmesh" %in% names(grid_z)
          )
          
          Plot_VARz_grid2D(
            VARval    = ctx$IC_global,
            grid_plot = grid_z,
            config    = config,
            VARname   = "IC",
            VARattr   = "Indice de confiance global",
            outputsdir = ctx$maps_dir
          )
          message("ETAPE 10: carte IC_global exportée dans ", ctx$outputsdir)
        }
        
      }
    }
  }
  # ---- ETAPE 11: Export / Consolidation ------------------------------------
  {
    message("ETAPE 11: Consolidation des résultats...")
    
    # ---- Guards minimaux ----------------------------------------------------
    stopifnot(
      "ETAPE 11: grid_z manquant"        = exists("grid_z") && inherits(grid_z, "sf"),
      "ETAPE 11: ctx$IMA manquant"       = !is.null(ctx$IMA),
      "ETAPE 11: ctx$REXC_E1 manquant"   = !is.null(ctx$REXC_E1),
      "ETAPE 11: ctx$REFC_E1 manquant"   = !is.null(ctx$REFC_E1),
      "ETAPE 11: ctx$IPC_z manquant"     = !is.null(ctx$IPC_z),
      "ETAPE 11: ctx$IQ_A_mean manquant"  = !is.null(ctx$IQ_A_mean),
      "ETAPE 11: ctx$IC_AP_mean manquant" = !is.null(ctx$IC_AP_mean),
      "ETAPE 11: ctx$IQ_H_mean manquant"  = !is.null(ctx$IQ_H_mean),
      "ETAPE 11: ctx$IC_HP_mean manquant" = !is.null(ctx$IC_HP_mean),
      "ETAPE 11: ctx$outputsdir manquant"  = !is.null(ctx$outputsdir) && nzchar(ctx$outputsdir),
      "ETAPE 11: ctx$datesauv manquant"  = !is.null(ctx$datesauv) && nzchar(ctx$datesauv)
    )
    
    # créer dossier outputs si absent
    if (!dir.exists(ctx$outputsdir)) dir.create(ctx$outputsdir, recursive = TRUE, showWarnings = FALSE)
    
    # standardiser types de clés (évite joins silencieux)
    grid_z <- grid_z |>
      dplyr::mutate(idmesh = as.character(.data$idmesh))
    
    ctx$IMA <- ctx$IMA |>
      dplyr::mutate(idmesh = as.character(.data$idmesh))
    
    ctx$IPC_z <- ctx$IPC_z |>
      dplyr::mutate(idmesh = as.character(.data$idmesh))
    
    ctx$REXC_E1 <- ctx$REXC_E1 |>
      dplyr::mutate(idmesh = as.character(.data$idmesh))
    
    ctx$REFC_E1 <- ctx$REFC_E1 |>
      dplyr::mutate(idmesh = as.character(.data$idmesh))
    
    # Sensibilités export (réutilisé)
    stopifnot("ETAPE 11: ctx$Mat_Sensib_Pj manquant" = !is.null(ctx$Mat_Sensib_Pj))
    Mat_Sensib_export <- sf::st_drop_geometry(ctx$Mat_Sensib_Pj)
    if ("idmesh" %in% names(Mat_Sensib_export)) {
      Mat_Sensib_export$idmesh <- as.character(Mat_Sensib_export$idmesh)
    }
    
    # ---- 11.1 Grille finale -------------------------------------------------
    grid_final <- grid_z |>
      dplyr::left_join(ctx$IMA,     by = "idmesh") |>
      dplyr::left_join(ctx$IPC_z,   by = c("idmesh" = "idmesh")) |>
      dplyr::left_join(ctx$REXC_E1, by = c("idmesh" = "idmesh")) |>
      dplyr::left_join(ctx$REFC_E1, by = c("idmesh" = "idmesh"))
    
    # REXC_E1_norm : normalisation a posteriori du risque d'exposition brut,
    # pour obtenir un score relatif [0,1] comparable à REFC_E1. N'affecte
    # pas REXC_E1 (conservé brut, en m², pour les exports/cartes existants).
    stopifnot("ETAPE 11: f_lognorm non chargée" = exists("f_lognorm", mode = "function"))
    grid_final <- grid_final |>
      dplyr::mutate(REXC_E1_norm = f_lognorm(.data$REXC_E1))
    
    if (isTRUE(ctx$IC_available) && !is.null(ctx$IC_global)) {
      ctx$IC_global <- ctx$IC_global |>
        dplyr::mutate(idmesh = as.character(.data$idmesh))
      grid_final <- grid_final |>
        dplyr::left_join(ctx$IC_global, by = c("idmesh" = "idmesh"))
    } else {
      grid_final$IC <- NA_real_
    }
    
    # ---- Ajout des 4 indicateurs (QA, CAP, QH, CHP) ----------------------------
    ctx$IQ_A_mean  <- ctx$IQ_A_mean  |> dplyr::mutate(idmesh = as.character(.data$idmesh))
    ctx$IC_AP_mean <- ctx$IC_AP_mean |> dplyr::mutate(idmesh = as.character(.data$idmesh))
    ctx$IQ_H_mean  <- ctx$IQ_H_mean  |> dplyr::mutate(idmesh = as.character(.data$idmesh))
    ctx$IC_HP_mean <- ctx$IC_HP_mean |> dplyr::mutate(idmesh = as.character(.data$idmesh))
    
    grid_final <- grid_final |>
      dplyr::left_join(ctx$IQ_A_mean,  by = "idmesh") |>
      dplyr::left_join(ctx$IC_AP_mean, by = "idmesh") |>
      dplyr::left_join(ctx$IQ_H_mean,  by = "idmesh") |>
      dplyr::left_join(ctx$IC_HP_mean, by = "idmesh")
    
    # ---- 11.2 Table indices (export DB) -------------------------------------
    required_index_cols <- c(
      "idmesh",
      "IMA_option1", "IMA_option2",
      "IPC_option1", "IPC_option2",
      "REXC_E1", "REXC_E1_norm", "REFC_E1",
      "IQ_A_mean", "IC_AP_mean", "IQ_H_mean", "IC_HP_mean",
      "IC"
    )
    
    grid_final_nogeo <- sf::st_drop_geometry(grid_final)
    missing_index_cols <- setdiff(required_index_cols, names(grid_final_nogeo))
    if (length(missing_index_cols) > 0) {
      stop("ETAPE 11: colonnes manquantes dans grid_final pour data_index: ",
           paste(missing_index_cols, collapse = ", "))
    }
    
    data_index <- grid_final_nogeo |>
      dplyr::select(dplyr::all_of(required_index_cols))
    
    # ---- 11.3 Export GeoPackage principal -----------------------------------
    if (isTRUE(config$Outputs$write_maps)) {
      output_gpkg <- file.path(ctx$outputsdir, paste0("resultats_analyse_", ctx$datesauv, ".gpkg"))
      
      sf::st_write(
        grid_final,
        dsn = output_gpkg,
        layer = "grille_indices",
        driver = "GPKG",
        delete_dsn = TRUE,
        quiet = TRUE
      )
      
      message("  -> GeoPackage principal : ", output_gpkg)
      ctx$output_gpkg <- output_gpkg
    } else {
      ctx$output_gpkg <- NULL
    }
    
    # ---- 11.3bis Export GeoJSON (Leaflet) ------------------------------------
    if (requireNamespace("sf", quietly = TRUE)) {
      
      maps_dir <- file.path(ctx$outputsdir, "maps")
      dir.create(maps_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Données web: 1 couche IC (avec tous les attributs pour le popup)
      grid_web <- grid_final |>
        dplyr::select(idmesh, REFC_E1, IC, IQ_A_mean, IC_AP_mean, IQ_H_mean, IC_HP_mean)
      
      # Reprojection obligatoire pour Leaflet (WGS84)
      grid_web <- sf::st_transform(grid_web, 4326)
      
      # Carte REFC
      refc_geojson <- file.path(maps_dir, "refc_e1.geojson")
      sf::st_write(
        grid_web[, c("idmesh", "REFC_E1")],
        refc_geojson,
        delete_dsn = TRUE,
        quiet = TRUE
      )
      
      # Carte IC (UNE SEULE couche) : contient IC + les 4 indicateurs -> popup possible
      ic_geojson <- file.path(maps_dir, "ic.geojson")
      sf::st_write(
        grid_web,
        ic_geojson,
        delete_dsn = TRUE,
        quiet = TRUE
      )
      
      ctx$geojson_refc <- refc_geojson
      ctx$geojson_ic   <- ic_geojson
      
      message("  -> Exports GeoJSON Leaflet : ", maps_dir)
    }
    # ---- 11.4 Exports détails (GPKG) ----------------------------------------
    if (isTRUE(config$Outputs$export_details)) {
      details_gpkg <- file.path(ctx$outputsdir, paste0("details_analyse_", ctx$datesauv, ".gpkg"))
      
      # On repart d'un fichier propre pour éviter des couches résiduelles
      if (file.exists(details_gpkg)) file.remove(details_gpkg)
      
      # Activités
      stopifnot("ETAPE 11: ctx$A_zi manquant (export_details)" = !is.null(ctx$A_zi))
      grid_act <- grid_z |>
        dplyr::left_join(dplyr::mutate(ctx$A_zi, idmesh = as.character(.data$idmesh)), by = "idmesh")
      sf::st_write(grid_act, dsn = details_gpkg, layer = "activites", quiet = TRUE)
      
      # Pressions normalisées
      stopifnot("ETAPE 11: ctx$Mat_Pj_norm_z manquant (export_details)" = !is.null(ctx$Mat_Pj_norm_z))
      grid_press <- grid_z |>
        dplyr::left_join(dplyr::mutate(ctx$Mat_Pj_norm_z, idmesh = as.character(.data$idmesh)), by = c("idmesh" = "idmesh"))
      sf::st_write(grid_press, dsn = details_gpkg, layer = "pressions_norm", quiet = TRUE)
      
      # Sensibilités
      stopifnot("ETAPE 11: Mat_Sensib_export doit contenir idmesh" = "idmesh" %in% names(Mat_Sensib_export))
      grid_sensib <- grid_z |>
        dplyr::left_join(Mat_Sensib_export, by = c("idmesh" = "idmesh"))
      sf::st_write(grid_sensib, dsn = details_gpkg, layer = "sensibilites", quiet = TRUE)
      
      # Effets
      stopifnot("ETAPE 11: ctx$Mat_REF_PjE1_z manquant (export_details)" = !is.null(ctx$Mat_REF_PjE1_z))
      grid_effets <- grid_z |>
        dplyr::left_join(dplyr::mutate(ctx$Mat_REF_PjE1_z, idmesh = as.character(.data$idmesh)), by = c("idmesh" = "idmesh"))
      sf::st_write(grid_effets, dsn = details_gpkg, layer = "effets", quiet = TRUE)
      
      message("  -> GeoPackage détails : ", details_gpkg)
      ctx$details_gpkg <- details_gpkg
    } else {
      ctx$details_gpkg <- NULL
    }
    
    # ---- 11.5 Stats récap ---------------------------------------------------
    stats_recap <- list(
      n_mailles     = nrow(grid_z),
      n_activites   = if (!is.null(ctx$A_zi)) max(0, ncol(ctx$A_zi) - 1) else NA_integer_,
      n_pressions   = if (!is.null(ctx$P_code_sel)) length(ctx$P_code_sel) else NA_integer_,
      srm           = ctx$lcond_SRM,
      date_execution= ctx$datesauv,
      
      ima_opt1_moy  = mean(data_index$IMA_option1, na.rm = TRUE),
      ima_opt1_max  = suppressWarnings(max(data_index$IMA_option1, na.rm = TRUE)),
      ima_opt2_moy  = mean(data_index$IMA_option2, na.rm = TRUE),
      
      ipc_opt1_moy  = mean(data_index$IPC_option1, na.rm = TRUE),
      ipc_opt1_max  = suppressWarnings(max(data_index$IPC_option1, na.rm = TRUE)),
      ipc_opt2_moy  = mean(data_index$IPC_option2, na.rm = TRUE),
      
      rexc_moy      = mean(data_index$REXC_E1_norm, na.rm = TRUE),
      rexc_max      = suppressWarnings(max(data_index$REXC_E1_norm, na.rm = TRUE)),
      rexc_q75      = as.numeric(stats::quantile(data_index$REXC_E1_norm, 0.75, na.rm = TRUE)),
      rexc_brut_moy = mean(data_index$REXC_E1, na.rm = TRUE),
      
      refc_moy      = mean(data_index$REFC_E1, na.rm = TRUE),
      refc_max      = suppressWarnings(max(data_index$REFC_E1, na.rm = TRUE)),
      refc_q75      = as.numeric(stats::quantile(data_index$REFC_E1, 0.75, na.rm = TRUE)),
      
      ic_moy        = if (isTRUE(ctx$IC_available)) mean(data_index$IC, na.rm = TRUE) else NA_real_,
      ic_min        = if (isTRUE(ctx$IC_available)) suppressWarnings(min(data_index$IC, na.rm = TRUE)) else NA_real_
    )
    
    stats_file <- file.path(ctx$outputsdir, paste0("stats_recap_", ctx$datesauv, ".json"))
    jsonlite::write_json(stats_recap, stats_file, auto_unbox = TRUE, pretty = TRUE)
    message("  -> Statistiques : ", stats_file)
    
    # Stockages ctx + objets de sortie pour ETAPE 12
    ctx$stats_recap <- stats_recap
    ctx$grid_final  <- grid_final
    ctx$data_index  <- data_index
    ctx$stats_file  <- stats_file
    
    message("ETAPE 11 OK: Consolidation terminée.")
  }
  
  # ---- ETAPE 12: Finalisation / Return -------------------------------------
  {
    # ---- Guards: ETAPE 11 doit avoir rempli ctx -----------------------------
    stopifnot(
      "ETAPE 12: ctx$grid_final manquant" = !is.null(ctx$grid_final),
      "ETAPE 12: ctx$data_index manquant" = !is.null(ctx$data_index),
      "ETAPE 12: ctx$stats_recap manquant"= !is.null(ctx$stats_recap),
      "ETAPE 12: ctx$score_tbl manquant"   = !is.null(ctx$score_tbl)
    )
    
    grid_final  <- ctx$grid_final
    data_index  <- ctx$data_index
    stats_recap <- ctx$stats_recap
    
    n_act <- if (!is.null(ctx$A_zi)) max(0, ncol(ctx$A_zi) - 1) else 0
    n_pr  <- if (!is.null(ctx$P_code_sel)) length(ctx$P_code_sel) else 0
    
    cat("\n")
    cat("========================================\n")
    cat("   ANALYSE TERMINÉE AVEC SUCCÈS\n")
    cat("========================================\n")
    cat("Zone d'étude    :", paste(ctx$lcond_SRM, collapse = ", "), "\n")
    cat("Mailles         :", nrow(grid_z), "\n")
    cat("Activités       :", n_act, "\n")
    cat("Pressions       :", n_pr, "\n")
    cat("\nIndicateurs moyens :\n")
    cat("  IMA (opt 1)   :", round(stats_recap$ima_opt1_moy, 2), "\n")
    cat("  IPC (opt 1)   :", round(stats_recap$ipc_opt1_moy, 2), "\n")
    cat("  REXC (E1)     :", round(stats_recap$rexc_moy, 3), "\n")
    cat("  REFC (E1)     :", round(stats_recap$refc_moy, 3), "\n")
    if (isTRUE(ctx$IC_available)) {
      cat("  IC global     :", round(stats_recap$ic_moy, 3), "\n")
    }
    cat("\nFichiers générés :\n")
    if (!is.null(ctx$output_gpkg))  cat("  ->", ctx$output_gpkg, "\n")
    if (!is.null(ctx$details_gpkg)) cat("  ->", ctx$details_gpkg, "\n")
    cat("========================================\n\n")
    
    data_source <- tolower(trimws(config$db$data_source %||% "postgis"))
    if (identical(data_source, "postgis")) {
      suppressMessages({export_gpkgs_to_postgres(config, outputs_dir = ctx$outputsdir)})
    } else {
      message("  [flat_files] Export PostgreSQL ignoré — résultats disponibles dans : ", ctx$outputsdir)
    }
    
    return(list(
      grid_final      = grid_final,
      data_index      = data_index,

      activites       = ctx$A_zi,
      pressions_norm  = ctx$Mat_Pj_norm_z,
      sensibilites    = ctx$Mat_Sensib_Pj,
      effets          = ctx$Mat_REF_PjE1_z,
      score_tbl       = ctx$score_tbl,  
      
      IMA             = ctx$IMA,
      IPC             = ctx$IPC_z,
      REXC_E1         = ctx$REXC_E1,
      REFC_E1         = ctx$REFC_E1,
      IC_global       = if (isTRUE(ctx$IC_available)) ctx$IC_global else NULL,
      
      stats           = stats_recap,
      paths           = list(
        gpkg_principal = ctx$output_gpkg,
        gpkg_details   = ctx$details_gpkg,
        outputsdir       = ctx$outputsdir,
        stats_json     = ctx$stats_file
      ),
      
      ctx             = ctx
    ))
  }
  
}
