#engine/functions/step1_init.R

step1_init <- function(config, paths, con, data) {
  
  # Vérifier dépendances installées
  pkgs <- c("sf","raster","DBI","RPostgres","ggplot2","ggmap","readODS","tidyverse")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) stop("Package manquant: ", p)
  }
  
  # Guards
  if (is.null(config) || !is.list(config)) stop("ETAPE 1: 'config' manquant ou invalide.")
  if (is.null(paths)  || !is.list(paths))  stop("ETAPE 1: 'paths' manquant ou invalide.")
  data_source <- tolower(trimws(config$db$data_source %||% "postgis"))
  mode_flat   <- identical(data_source, "flat_files")
  if (!mode_flat && (is.null(con) || !DBI::dbIsValid(con))) {
    stop("ETAPE 1: connexion 'con' manquante ou invalide.")
  }
  if (is.null(data) || !is.list(data)) stop("ETAPE 1: argument 'data' manquant (res0$list_sauv).")
  
  # Charger les fonctions legacy
  fun_files <- c(
    "Plot_VARz_grid2D.R",
    "f_lognorm.R",
    "f_norm.R",
    "f_Int_to_Log.R"
  )
  
  fun_dir <- paths$functions_dir
  if (is.null(fun_dir) || !nzchar(as.character(fun_dir)) || !dir.exists(fun_dir)) {
    stop("ETAPE 1: paths$functions_dir manquant ou dossier inexistant: ", as.character(fun_dir))
  }
  
  missing_fun <- fun_files[!file.exists(file.path(fun_dir, fun_files))]
  if (length(missing_fun) > 0) {
    stop("ETAPE 1: fichiers fonctions manquants dans ", fun_dir, ": ", paste(missing_fun, collapse = ", "))
  }
  for (f in fun_files) source(file.path(fun_dir, f))
  
  # Matrice de Sensibilité: chemin résolu
  filesensi <- config$inputs$ods$sensi_hab_press
  if (is.null(filesensi) || !nzchar(trimws(as.character(filesensi)))) {
    stop("ETAPE 1: config$inputs$ods$sensi_hab_press manquant.")
  }
  sensi_path <- file.path(paths$study_dir, as.character(filesensi))
  if (!file.exists(sensi_path)) stop("ETAPE 1: fichier sensibilités introuvable: ", sensi_path)
  
  # Matrice Activites-Pressions: chemin résolu
  fileap <- config$inputs$ods$act_press
  if (is.null(fileap) || !nzchar(trimws(as.character(fileap)))) {
    stop("ETAPE 1: config$inputs$ods$act_press manquant.")
  }
  
  act_press_path <- file.path(paths$study_dir, as.character(fileap))
  if (!file.exists(act_press_path)) stop("ETAPE 1: fichier act_press introuvable: ", act_press_path)
  
  # SRM
  srm_raw <- config$project$srm
  if (is.null(srm_raw)) srm_raw <- ""
  lcond_SRM <- trimws(unlist(strsplit(as.character(srm_raw), "[;,\\s]+")))
  lcond_SRM <- lcond_SRM[nzchar(lcond_SRM)]
  if (length(lcond_SRM) == 0) stop("ETAPE 1: aucune SRM fournie dans config$project$srm.")
  
  # Outputs
  datesauv <- format(Sys.time(), "%Y-%m-%d_%H-%M")
  run_dir <- file.path(paths$outputs_dir, paste0("results_", datesauv))
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  maps_dir   <- file.path(run_dir, "maps")
  graphs_dir <- file.path(run_dir, "graphs")
  dir.create(maps_dir,   recursive = TRUE, showWarnings = FALSE)
  dir.create(graphs_dir, recursive = TRUE, showWarnings = FALSE)
  outputsdir <- run_dir
  
  message("SRM retenues: ", paste(lcond_SRM, collapse = ", "))
  
  # --- Chargement et validation du fichier ODS matrice A-P ------------------
  # Onglets attendus :
  #   - CCR_mediane        : CCR médians + IC  (scénario actif dans le pipeline principal)
  #   - CCR_precaution     : CCR précaution + IC (réservé Monte Carlo)
  #   - CCR_binaire        : CCR binaires V1 + IC (réservé Monte Carlo / analyse sensibilité)
  #   - typologie_activites : référentiel des activités (chargé pour info, non utilisé en 7-9)
  #   - typologie_pressions  : hiérarchie des pressions (Code_P, parent, mode_calcul)
  
  sheets_requis <- c("CCR_mediane", "CCR_precaution", "CCR_binaire",
                     "typologie_activites", "typologie_pressions")
  
  # Lister les onglets disponibles
  sheets_dispo <- tryCatch(
    readODS::list_ods_sheets(act_press_path),
    error = function(e) stop(
      "ETAPE 1: impossible de lire le fichier ODS matrice A-P: ", act_press_path,
      "\n  Erreur: ", conditionMessage(e)
    )
  )
  
  sheets_manquants <- setdiff(sheets_requis, sheets_dispo)
  if (length(sheets_manquants) > 0) {
    stop(
      "ETAPE 1: onglet(s) manquant(s) dans ", basename(act_press_path), ": ",
      paste(sheets_manquants, collapse = ", "),
      "\n  Onglets disponibles: ", paste(sheets_dispo, collapse = ", ")
    )
  }
  
  # --- Fonction interne : lecture + validation d'un onglet CCR ---------------
  # Structure attendue : colonnes Niveau (opt), Code_A, Nom_activite (opt),
  #                      Type (lien|ic), Pr_* (valeurs dans [0,1])
  # L'ordre des colonnes n'a pas d'importance.
  
  load_ccr_sheet <- function(path, sheet) {
    
    df <- tryCatch(
      readODS::read_ods(path, sheet = sheet),
      error = function(e) stop(
        "ETAPE 1: échec lecture onglet '", sheet, "' dans ", basename(path),
        "\n  Erreur: ", conditionMessage(e)
      )
    )
    
    if (!is.data.frame(df) || nrow(df) == 0)
      stop("ETAPE 1 [", sheet, "]: data.frame invalide ou vide")
    
    names_lc <- tolower(trimws(names(df)))
    
    # Colonnes obligatoires
    idx_type  <- which(names_lc == "type")
    idx_codea <- which(names_lc == "code_a")
    
    if (length(idx_type) == 0)
      stop("ETAPE 1 [", sheet, "]: colonne 'Type' introuvable")
    if (length(idx_codea) == 0)
      stop("ETAPE 1 [", sheet, "]: colonne 'Code_A' introuvable")
    
    names(df)[idx_type[1]]  <- "Type"
    names(df)[idx_codea[1]] <- "Code_A"
    
    # Au moins une colonne Pr_*
    pr_cols <- names(df)[grep("^pr_", tolower(names(df)))]
    if (length(pr_cols) == 0)
      stop("ETAPE 1 [", sheet, "]: aucune colonne Pr_* trouvée")
    
    # Lignes lien et ic présentes
    types_presents <- tolower(trimws(unique(as.character(df$Type))))
    types_presents <- types_presents[!is.na(types_presents) & nzchar(types_presents)]
    
    if (!"lien" %in% types_presents)
      stop("ETAPE 1 [", sheet, "]: aucune ligne Type=='lien'")
    if (!"ic" %in% types_presents)
      stop("ETAPE 1 [", sheet, "]: aucune ligne Type=='ic'")
    
    # Valeurs lien dans [0,1]
    vals_lien <- suppressWarnings(
      as.numeric(unlist(df[tolower(trimws(df$Type)) == "lien", pr_cols, drop = FALSE]))
    )
    vals_finies <- vals_lien[is.finite(vals_lien)]
    if (length(vals_finies) > 0 && (any(vals_finies < 0) || any(vals_finies > 1))) {
      warning(
        "ETAPE 1 [", sheet, "]: valeurs CCR hors [0,1] détectées ",
        "(min=", round(min(vals_finies), 4), ", max=", round(max(vals_finies), 4), ")"
      )
    }
    
    n_act <- sum(tolower(trimws(df$Type)) == "lien", na.rm = TRUE)
    message("ETAPE 1: onglet '", sheet, "' OK — ",
            n_act, " activité(s), ", length(pr_cols), " pression(s)")
    df
  }
  
  # --- Chargement des trois matrices CCR ------------------------------------
  MatAP_mediane    <- load_ccr_sheet(act_press_path, "CCR_mediane")
  MatAP_precaution <- load_ccr_sheet(act_press_path, "CCR_precaution")
  MatAP_binaire    <- load_ccr_sheet(act_press_path, "CCR_binaire")
  
  # --- Cohérence inter-matrices (warning non bloquant) ----------------------
  get_codea  <- function(df) sort(unique(as.character(
    df$Code_A[tolower(trimws(df$Type)) == "lien"])))
  get_prcols <- function(df) sort(names(df)[grep("^pr_", tolower(names(df)))])
  
  codes_med  <- get_codea(MatAP_mediane)
  codes_prec <- get_codea(MatAP_precaution)
  codes_bin  <- get_codea(MatAP_binaire)
  pr_med     <- get_prcols(MatAP_mediane)
  pr_prec    <- get_prcols(MatAP_precaution)
  pr_bin     <- get_prcols(MatAP_binaire)
  
  if (!identical(codes_med, codes_prec) || !identical(codes_med, codes_bin)) {
    diff_prec <- setdiff(codes_med, codes_prec)
    diff_bin  <- setdiff(codes_med, codes_bin)
    warning(
      "ETAPE 1: les onglets CCR n'ont pas les mêmes Code_A.\n",
      if (length(diff_prec) > 0) paste0("  Absent de CCR_precaution: ",
                                        paste(diff_prec, collapse = ", "), "\n") else "",
      if (length(diff_bin)  > 0) paste0("  Absent de CCR_binaire: ",
                                        paste(diff_bin,  collapse = ", "), "\n") else ""
    )
  }
  if (!identical(pr_med, pr_prec) || !identical(pr_med, pr_bin)) {
    warning(
      "ETAPE 1: les onglets CCR n'ont pas les mêmes colonnes Pr_*. ",
      "Vérifiez la cohérence entre CCR_mediane, CCR_precaution et CCR_binaire."
    )
  }
  
  # Toujours CCR_mediane comme scénario actif.
  # CCR_precaution et CCR_binaire sont réservés au Monte Carlo (step2)
  MatAP_active <- MatAP_mediane
  message("ETAPE 1: scénario CCR actif = médiane (pipeline principal)")
  
  # --- Chargement de la typologie_pressions ----------------------------------
  # Colonnes attendues : Code_P, Niveau, Nom_pression, parent, mode_calcul, demonstrateur
  # Niveau         : profondeur hiérarchique PressRef (1 = racine, 4 = feuille fine)
  # mode_calcul 1  : estimée depuis les activités via Σ(γᵢⱼ · Aᵢ_norm)
  # mode_calcul 2  : alimentation depuis données externes (à développer)
  # mode_calcul 3  : reconstruite récursivement depuis ses enfants (étape 9)
  # Note : Niveau et mode_calcul sont indépendants. Une feuille (mode 1 ou 2)
  # peut être à n'importe quel niveau.
  
  typo_press_raw <- tryCatch(
    readODS::read_ods(act_press_path, sheet = "typologie_pressions"),
    error = function(e) stop(
      "ETAPE 1: échec lecture onglet 'typologie_pressions': ", conditionMessage(e)
    )
  )
  
  if (!is.data.frame(typo_press_raw) || nrow(typo_press_raw) == 0)
    stop("ETAPE 1 [typologie_pressions]: data.frame invalide ou vide")
  
  names_tp    <- tolower(trimws(names(typo_press_raw)))
  cols_requis <- c("code_p", "niveau", "nom_pression", "parent", "mode_calcul", "demonstrateur")
  manquants   <- setdiff(cols_requis, names_tp)
  if (length(manquants) > 0)
    stop("ETAPE 1 [typologie_pressions]: colonne(s) manquante(s): ",
         paste(manquants, collapse = ", "))
  
  cols_target <- c("Code_P", "Niveau", "Nom_pression", "parent", "mode_calcul", "demonstrateur")
  names(typo_press_raw)[match(cols_requis, names_tp)] <- cols_target
  
  typo_press <- typo_press_raw |>
    dplyr::transmute(
      Code_P        = as.character(Code_P),
      Niveau        = suppressWarnings(as.integer(Niveau)),
      Nom_pression  = trimws(as.character(Nom_pression)),
      parent        = dplyr::na_if(trimws(as.character(parent)), ""),
      mode_calcul   = suppressWarnings(as.integer(mode_calcul)),
      demonstrateur = suppressWarnings(as.integer(demonstrateur))
    ) |>
    dplyr::filter(!is.na(.data$Code_P) & nzchar(.data$Code_P))
  
  # Validation : Niveau dans 1..4
  niveaux_invalides <- unique(typo_press$Niveau[!typo_press$Niveau %in% 1L:4L])
  if (length(niveaux_invalides) > 0)
    stop("ETAPE 1 [typologie_pressions]: valeurs Niveau invalides (attendu 1..4): ",
         paste(niveaux_invalides, collapse = ", "))
  
  # Validation : Nom_pression renseigné
  nom_manquants <- typo_press$Code_P[
    is.na(typo_press$Nom_pression) | !nzchar(typo_press$Nom_pression)
  ]
  if (length(nom_manquants) > 0) {
    stop("ETAPE 1 [typologie_pressions]: Nom_pression vide pour le(s) code(s): ",
         paste(nom_manquants, collapse = ", "))
  }
  
  modes_invalides <- unique(typo_press$mode_calcul[!typo_press$mode_calcul %in% c(1L, 2L, 3L)])
  if (length(modes_invalides) > 0)
    stop("ETAPE 1 [typologie_pressions]: valeurs mode_calcul invalides (attendu 1, 2 ou 3): ",
         paste(modes_invalides, collapse = ", "))
  
  n_feuilles <- sum(typo_press$mode_calcul %in% c(1L, 2L), na.rm = TRUE)
  n_composes <- sum(typo_press$mode_calcul == 3L,           na.rm = TRUE)
  n_racines  <- sum(is.na(typo_press$parent))
  
  message("ETAPE 1: typologie_pressions OK — ",
          nrow(typo_press), " pressions (",
          n_feuilles, " feuille(s), ",
          n_composes, " composée(s), ",
          n_racines,  " racine(s), niveaux ",
          paste(sort(unique(typo_press$Niveau)), collapse = "/"), ")")
  
  list(
    sensi_path = sensi_path,
    act_press_path = act_press_path,
    lcond_SRM  = lcond_SRM,
    datesauv   = datesauv,
    outputsdir   = outputsdir,
    run_dir    = run_dir,
    maps_dir   = maps_dir,
    graphs_dir = graphs_dir,
    MatAP_mediane    = MatAP_mediane,
    MatAP_precaution = MatAP_precaution,
    MatAP_binaire    = MatAP_binaire,
    MatAP_active     = MatAP_active,   # = MatAP_mediane (pipeline principal)
    typo_press       = typo_press
  )
}
