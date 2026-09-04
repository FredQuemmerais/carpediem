# engine/steps/step2b_robustesse_det.R
# version du 24.07.2026
#
# ============================================================
# RÔLE DE CE SCRIPT
# ============================================================
# Analyse de robustesse déterministe : évalue l'effet marginal
# de chaque facteur déterministe sur les résultats du modèle.
# Un facteur varie à la fois, les autres restent à REF_VALUES.
#
# Facteurs déterministes couverts :
#   X1_mat_AP      : matrice A-P (médiane / précaution / binaire)
#   X2_relation_AP : forme de la relation A-P (lin / opti / pess / logist)
#   X6_typo_act    : granularité de la typologie activité
#   X7_mat_sensi   : matrice de sensibilité (précaution / médiane)
#   X9_norm_form   : forme de normalisation (log / linéaire / sigmoïde)
#   X10_agreg_press: mode d'agrégation (additif / synergique / antagoniste)
#
# Facteurs stochastiques hors périmètre (X3, X4, X5, X8) :
#   Leur effet est caractérisé par step2c (robustesse stochastique).
#
# Dépendances :
#   engine_step2()        -> engine/functions/engine_step2.R
#   calc_refc()           -> engine/functions/f_REFC_ref_step2.R
#   REF_VALUES            -> engine/functions/f_aux_monte_carlo.R
#
# ============================================================
# INDICATEURS PRODUITS PAR FACTEUR ET PAR SCÉNARIO
# ============================================================
#   REFC_<scenario>       : vecteur REFC par maille
#   ecart_<s>_ref         : écart absolu vs scénario de référence
#   rang_<scenario>       : rang par maille (décroissant)
#   delta_rang_<s>_ref    : changement de rang relatif vs référence (% de nz)
#   top25_<scenario>      : booléen — maille dans le top 25%
#   top10_<scenario>      : booléen — maille dans le top 10%
#   stable25_<s>_ref      : booléen — stable dans le top 25% vs référence
#   stable10_<s>_ref      : booléen — stable dans le top 10% vs référence
#   Spearman, Jaccard top25/top10 stockés en attributs.

step2b_robustesse_det <- function(config, paths, con, res1, engine = NULL) {
  
  # ---- Guards -----------------------------------------------------------
  stopifnot(
    "Step2b: res1 doit être une liste"                         = is.list(res1),
    "Step2b: res1$ctx manquant"                                = !is.null(res1$ctx),
    "Step2b: config$analyses manquant"                         = !is.null(config$analyses),
    "Step2b: config$analyses$robustesse_deterministe manquant" =
      !is.null(config$analyses$robustesse_deterministe),
    "Step2b: res1$ctx$Mat_Sensib_Pj manquant"                  = !is.null(res1$ctx$Mat_Sensib_Pj),
    "Step2b: res1$ctx$P_code_sel manquant"                     = !is.null(res1$ctx$P_code_sel),
    "Step2b: res1$ctx$typo_press manquant"                     = !is.null(res1$ctx$typo_press),
    "Step2b: calc_refc non chargée"                            = exists("calc_refc",    mode = "function"),
    "Step2b: engine_step2 non chargée"                         = exists("engine_step2", mode = "function"),
    "Step2b: REF_VALUES introuvable"                           = exists("REF_VALUES")
  )
  
  message("\n========================================")
  message("   STEP2B - ROBUSTESSE DÉTERMINISTE")
  message("========================================\n")
  
  ctx        <- res1$ctx
  grid_z     <- ctx$grid_z
  nz         <- ctx$nz
  col_idz    <- ctx$col_idz
  P_code_sel <- ctx$P_code_sel
  outputsdir <- ctx$outputsdir
  
  rob_dir <- file.path(outputsdir, "robustesse_det")
  dir.create(rob_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- Construction / réutilisation du bundle engine --------------------
  # Si engine est fourni par l'appelant (cas où step2 a déjà tourné),
  # on le réutilise directement. Sinon on le construit ici.
  if (is.null(engine)) {
    message("Step2b: construction du bundle engine (non fourni)...")
    engine <- engine_step2(ctx)
  } else {
    message("Step2b: réutilisation du bundle engine fourni.")
  }

  scenarios_cfg <- config$analyses$robustesse_deterministe$scenarios
  
  # -----------------------------------------------------------------------
  # Constructeur de params complets pour un scénario déterministe.
  # Tous les facteurs hors périmètre step2b restent à REF_VALUES.
  # Les facteurs stochastiques sont toujours à leur valeur de référence
  # (0 pour les binaires), ce qui désactive leur perturbation dans calc_refc.
  # -----------------------------------------------------------------------
  make_params <- function(...) {
    overrides <- list(...)
    params    <- REF_VALUES
    for (nm in names(overrides)) {
      if (!nm %in% names(params))
        stop(sprintf("Step2b make_params: facteur inconnu '%s'", nm))
      params[[nm]] <- as.integer(overrides[[nm]])
    }
    params
  }
  
  # -----------------------------------------------------------------------
  # Fonction de comparaison d'un ensemble de scénarios pour un facteur.
  # scenarios_list : liste nommée (nom = label scénario, valeur = vecteur REFC).
  # Le premier élément est le scénario de référence.
  # -----------------------------------------------------------------------
  compare_scenarios <- function(scenarios_list, facteur_label) {
    
    nms          <- names(scenarios_list)
    ref_nm       <- nms[1]
    REFC_ref_vec <- scenarios_list[[ref_nm]]
    
    df <- data.frame(idmesh = as.character(col_idz), stringsAsFactors = FALSE)
    
    spearman_list        <- list()
    pct_rang_stable_list <- list()
    ecart_moyen_list     <- list()
    stab_top25_list      <- list()
    stab_top10_list      <- list()
    
    seuil_stable_pct <- 5   # % de nz, cohérent avec step2c
    
    q75_ref  <- quantile(REFC_ref_vec, 0.75, na.rm = TRUE)
    q90_ref  <- quantile(REFC_ref_vec, 0.90, na.rm = TRUE)
    top25_ref <- REFC_ref_vec >= q75_ref
    top10_ref <- REFC_ref_vec >= q90_ref
    
    for (nm in nms) {
      rv <- scenarios_list[[nm]]
      df[[paste0("REFC_", nm)]] <- rv
      df[[paste0("rang_", nm)]] <- rank(-rv, ties.method = "average")
      
      q75_s <- quantile(rv, 0.75, na.rm = TRUE)
      q90_s <- quantile(rv, 0.90, na.rm = TRUE)
      df[[paste0("top25_", nm)]] <- rv >= q75_s
      df[[paste0("top10_", nm)]] <- rv >= q90_s
      
      if (nm != ref_nm) {
        df[[paste0("ecart_", nm, "_ref")]]      <- abs(rv - REFC_ref_vec)
        # delta_rang_<nm>_ref : écart de rang relatif, exprimé en fraction de nz
        df[[paste0("delta_rang_", nm, "_ref")]] <-
          abs(df[[paste0("rang_", nm)]] - df[[paste0("rang_", ref_nm)]]) / nz
        
        df[[paste0("stable25_", nm, "_ref")]] <-
          top25_ref & df[[paste0("top25_", nm)]]
        df[[paste0("stable10_", nm, "_ref")]] <-
          top10_ref & df[[paste0("top10_", nm)]]
        
        sp <- cor(REFC_ref_vec, rv, method = "spearman", use = "complete.obs")
        
        # % de mailles dont le rang varie de moins de seuil_stable_pct % de l'effectif
        # -> robustesse du classement spatial (cf. step2c$pct_rang_stable)
        delta_rang_frac <- df[[paste0("delta_rang_", nm, "_ref")]]
        pct_rang_stable <- mean(delta_rang_frac <= (seuil_stable_pct / 100),
                                na.rm = TRUE) * 100
        
        # écart absolu moyen REFC comparé vs référence
        ecart_moyen <- mean(df[[paste0("ecart_", nm, "_ref")]], na.rm = TRUE)
        
        inter25 <- sum(top25_ref & df[[paste0("top25_", nm)]], na.rm = TRUE)
        union25 <- sum(top25_ref | df[[paste0("top25_", nm)]], na.rm = TRUE)
        jacc25  <- if (union25 > 0) inter25 / union25 else NA_real_
        
        inter10 <- sum(top10_ref & df[[paste0("top10_", nm)]], na.rm = TRUE)
        union10 <- sum(top10_ref | df[[paste0("top10_", nm)]], na.rm = TRUE)
        jacc10  <- if (union10 > 0) inter10 / union10 else NA_real_
        
        pct25 <- mean(df[[paste0("top25_", nm)]][top25_ref], na.rm = TRUE) * 100
        pct10 <- mean(df[[paste0("top10_", nm)]][top10_ref], na.rm = TRUE) * 100
        
        spearman_list[[nm]]        <- sp
        pct_rang_stable_list[[nm]] <- pct_rang_stable
        ecart_moyen_list[[nm]]     <- ecart_moyen
        stab_top25_list[[nm]]      <- list(jaccard = jacc25, pct_retenu = pct25)
        stab_top10_list[[nm]]      <- list(jaccard = jacc10, pct_retenu = pct10)
        
        message(sprintf(
          "  %s | %-14s vs %-14s : Spearman=%.3f | Stable rang(+-%d%%)=%.1f%% | ecart_moy=%.4f | Top25%% pct=%.1f%%",
          facteur_label, ref_nm, nm, sp, seuil_stable_pct, pct_rang_stable, ecart_moyen, pct25
        ))
      }
    }
    
    attr(df, "spearman")         <- spearman_list
    attr(df, "pct_rang_stable")  <- pct_rang_stable_list
    attr(df, "ecart_moyen")      <- ecart_moyen_list
    attr(df, "stab_top25")       <- stab_top25_list
    attr(df, "stab_top10")       <- stab_top10_list
    attr(df, "seuil_stable_pct") <- seuil_stable_pct
    attr(df, "ref_scenario")     <- ref_nm
    attr(df, "facteur")          <- facteur_label
    df
  }
  
  # -----------------------------------------------------------------------
  # Vérification de la config scénarios
  # -----------------------------------------------------------------------
  if (is.null(scenarios_cfg) || length(scenarios_cfg) == 0) {
    warning("Step2b: aucun scénario défini dans config$analyses$robustesse_deterministe$scenarios")
    return(invisible(NULL))
  }
  
  comp_scenarios <- list()
  run_ts <- format(Sys.time(), "%Y%m%d_%H%M")
  
  # -----------------------------------------------------------------------
  # X1_mat_AP — choix de la matrice A-P
  # Référence : 2 (médiane), conforme à REF_VALUES
  # -----------------------------------------------------------------------
  if (!is.null(scenarios_cfg$X1_mat_AP) && length(scenarios_cfg$X1_mat_AP) > 1) {
    
    message("\nStep2b: X1_mat_AP — comparaison des matrices A-P...")
    
    modal_map <- list(
      "2" = "mediane",
      "3" = "precaution",
      "4" = "binaire"
    )
    
    sc_list <- list()
    for (m in as.integer(scenarios_cfg$X1_mat_AP)) {
      key <- as.character(m)
      if (!key %in% names(modal_map)) {
        warning(sprintf("Step2b X1_mat_AP: modalité %d non reconnue, ignorée.", m)); next
      }
      lbl          <- modal_map[[key]]
      sc_list[[lbl]] <- calc_refc(make_params(X1_mat_AP = m), engine, ctx)
    }
    
    if (length(sc_list) > 1) {
      # Référence en premier (médiane = modalité 2)
      ref_lbl  <- modal_map[[as.character(REF_VALUES$X1_mat_AP)]]
      sc_list  <- c(sc_list[ref_lbl], sc_list[setdiff(names(sc_list), ref_lbl)])
      comp_X1  <- compare_scenarios(sc_list, "X1_mat_AP")
      comp_scenarios[["X1_mat_AP"]] <- comp_X1
      .export_robustesse_det(comp_X1, "X1_mat_AP", rob_dir, grid_z, run_ts)
      
      fact_dir_x1 <- file.path(rob_dir, "X1_mat_AP")
      if (exists("write_leaflet_robustesse_det_html", mode = "function")) {
        tryCatch(
          write_leaflet_robustesse_det_html(comp_X1, grid_z, fact_dir_x1),
          error = function(e)
            warning(sprintf("Step2b: carte HTML X1_mat_AP échouée : %s", e$message))
        )
      }
    }
  }
  
  # -----------------------------------------------------------------------
  # X2_relation_AP — forme de la relation A-P
  # Référence : 2 (linéaire), conforme à REF_VALUES
  # -----------------------------------------------------------------------
  if (!is.null(scenarios_cfg$X2_relation_AP) && length(scenarios_cfg$X2_relation_AP) > 1) {
    
    message("\nStep2b: X2_relation_AP — comparaison des formes A-P...")
    
    modal_map <- list(
      "2" = "lineaire",
      "3" = "optimiste",
      "4" = "pessimiste",
      "5" = "logistique"
    )
    
    sc_list <- list()
    for (m in as.integer(scenarios_cfg$X2_relation_AP)) {
      key <- as.character(m)
      if (!key %in% names(modal_map)) {
        warning(sprintf("Step2b X2_relation_AP: modalité %d non reconnue, ignorée.", m)); next
      }
      lbl          <- modal_map[[key]]
      sc_list[[lbl]] <- calc_refc(make_params(X2_relation_AP = m), engine, ctx)
    }
    
    if (length(sc_list) > 1) {
      ref_lbl <- modal_map[[as.character(REF_VALUES$X2_relation_AP)]]
      sc_list <- c(sc_list[ref_lbl], sc_list[setdiff(names(sc_list), ref_lbl)])
      comp_X2 <- compare_scenarios(sc_list, "X2_relation_AP")
      comp_scenarios[["X2_relation_AP"]] <- comp_X2
      .export_robustesse_det(comp_X2, "X2_relation_AP", rob_dir, grid_z, run_ts)
    }
  }
  
  # -----------------------------------------------------------------------
  # X6_typo_act — granularité de la typologie activité
  # Référence : 2 (natif), conforme à REF_VALUES
  # -----------------------------------------------------------------------
  if (!is.null(scenarios_cfg$X6_typo_act) && length(scenarios_cfg$X6_typo_act) > 1) {
    
    message("\nStep2b: X6_typo_act — comparaison des granularités d'activité...")
    
    modal_map <- list("2" = "natif", "3" = "aggrege_N2", "4" = "aggrege_N1")
    
    sc_list <- list()
    for (m in as.integer(scenarios_cfg$X6_typo_act)) {
      key <- as.character(m)
      if (!key %in% names(modal_map)) {
        warning(sprintf("Step2b X6_typo_act: modalité %d non reconnue, ignorée.", m)); next
      }
      lbl          <- modal_map[[key]]
      sc_list[[lbl]] <- calc_refc(make_params(X6_typo_act = m), engine, ctx)
    }
    
    if (length(sc_list) > 1) {
      ref_lbl <- modal_map[[as.character(REF_VALUES$X6_typo_act)]]
      sc_list <- c(sc_list[ref_lbl], sc_list[setdiff(names(sc_list), ref_lbl)])
      comp_X6 <- compare_scenarios(sc_list, "X6_typo_act")
      comp_scenarios[["X6_typo_act"]] <- comp_X6
      .export_robustesse_det(comp_X6, "X6_typo_act", rob_dir, grid_z, run_ts)
    }
  }
  
  # -----------------------------------------------------------------------
  # X7_mat_sensi — choix de la matrice de sensibilité
  # Référence déduite de config$Calc$agreg_sensib pour cohérence avec step1 :
  #   1 = précaution (modalité 2 dans calc_refc), 2 = médiane (modalité 3)
  # Attention : le mapping X7 est inversé par rapport à X1
  #   (X7=2 -> précaution, X7=3 -> médiane)
  # -----------------------------------------------------------------------
  if (!is.null(scenarios_cfg$X7_mat_sensi) && length(scenarios_cfg$X7_mat_sensi) > 1) {
    
    message("\nStep2b: X7_mat_sensi — comparaison des matrices de sensibilité...")
    
    modal_map <- list("2" = "precaution", "3" = "mediane", "4" = "binaire")
    
    # Référence cohérente avec step1
    agreg    <- as.integer(config$Calc$agreg_sensib %||% 1L)
    ref_x7   <- switch(as.character(agreg), "1" = 2L, "2" = 3L, 2L)  # défaut = précaution
    
    sc_list <- list()
    for (m in as.integer(scenarios_cfg$X7_mat_sensi)) {
      key <- as.character(m)
      if (!key %in% names(modal_map)) {
        warning(sprintf("Step2b X7_mat_sensi: modalité %d non reconnue, ignorée.", m)); next
      }
      lbl          <- modal_map[[key]]
      sc_list[[lbl]] <- calc_refc(make_params(X7_mat_sensi = m), engine, ctx)
    }
    
    if (length(sc_list) > 1) {
      ref_lbl <- modal_map[[as.character(ref_x7)]]
      if (ref_lbl %in% names(sc_list) && names(sc_list)[1] != ref_lbl)
        sc_list <- c(sc_list[ref_lbl], sc_list[setdiff(names(sc_list), ref_lbl)])
      comp_X7 <- compare_scenarios(sc_list, "X7_mat_sensi")
      comp_scenarios[["X7_mat_sensi"]] <- comp_X7
      .export_robustesse_det(comp_X7, "X7_mat_sensi", rob_dir, grid_z, run_ts)
    }
  }
  
  # -----------------------------------------------------------------------
  # X9_norm_form — forme de normalisation (NOUVEAU)
  # Référence : 2 (log), conforme à REF_VALUES
  # -----------------------------------------------------------------------
  if (!is.null(scenarios_cfg$X9_norm_form) && length(scenarios_cfg$X9_norm_form) > 1) {
    
    message("\nStep2b: X9_norm_form — comparaison des formes de normalisation...")
    
    modal_map <- list("2" = "log", "3" = "lineaire", "4" = "sigmoide")
    
    sc_list <- list()
    for (m in as.integer(scenarios_cfg$X9_norm_form)) {
      key <- as.character(m)
      if (!key %in% names(modal_map)) {
        warning(sprintf("Step2b X9_norm_form: modalité %d non reconnue, ignorée.", m)); next
      }
      lbl          <- modal_map[[key]]
      sc_list[[lbl]] <- calc_refc(make_params(X9_norm_form = m), engine, ctx)
    }
    
    if (length(sc_list) > 1) {
      ref_lbl <- modal_map[[as.character(REF_VALUES$X9_norm_form)]]
      sc_list <- c(sc_list[ref_lbl], sc_list[setdiff(names(sc_list), ref_lbl)])
      comp_X9 <- compare_scenarios(sc_list, "X9_norm_form")
      comp_scenarios[["X9_norm_form"]] <- comp_X9
      .export_robustesse_det(comp_X9, "X9_norm_form", rob_dir, grid_z, run_ts)
    }
  }
  
  # -----------------------------------------------------------------------
  # X10_agreg_press — mode d'agrégation des pressions (NOUVEAU)
  # Référence : 2 (additif), conforme à REF_VALUES
  # -----------------------------------------------------------------------
  if (!is.null(scenarios_cfg$X10_agreg_press) && length(scenarios_cfg$X10_agreg_press) > 1) {
    
    message("\nStep2b: X10_agreg_press — comparaison des modes d'agrégation...")
    
    modal_map <- list("2" = "additif", "3" = "synergique", "4" = "antagoniste")
    
    sc_list <- list()
    for (m in as.integer(scenarios_cfg$X10_agreg_press)) {
      key <- as.character(m)
      if (!key %in% names(modal_map)) {
        warning(sprintf("Step2b X10_agreg_press: modalité %d non reconnue, ignorée.", m)); next
      }
      lbl          <- modal_map[[key]]
      sc_list[[lbl]] <- calc_refc(make_params(X10_agreg_press = m), engine, ctx)
    }
    
    if (length(sc_list) > 1) {
      ref_lbl <- modal_map[[as.character(REF_VALUES$X10_agreg_press)]]
      sc_list <- c(sc_list[ref_lbl], sc_list[setdiff(names(sc_list), ref_lbl)])
      comp_X10 <- compare_scenarios(sc_list, "X10_agreg_press")
      comp_scenarios[["X10_agreg_press"]] <- comp_X10
      .export_robustesse_det(comp_X10, "X10_agreg_press", rob_dir, grid_z, run_ts)
    }
  }
  
  message("\n========================================")
  message("   STEP2B TERMINÉ")
  message("========================================\n")
  
  return(comp_scenarios)
}

# -----------------------------------------------------------------------
# Fonction interne : export GeoPackage + cartes pour un facteur
# Inchangée fonctionnellement, conservée telle quelle.
# -----------------------------------------------------------------------
.export_robustesse_det <- function(comp_df, facteur_name, rob_dir,
                                   grid_z, run_ts) {
  
  fact_dir <- file.path(rob_dir, facteur_name)
  dir.create(fact_dir, recursive = TRUE, showWarnings = FALSE)
  
  gpkg_path <- file.path(fact_dir,
                         paste0("robustesse_det_", facteur_name, "_", run_ts, ".gpkg"))
  
  grid_export <- grid_z |>
    dplyr::left_join(
      dplyr::mutate(comp_df, idmesh = as.character(.data$idmesh)),
      by = "idmesh"
    )
  
  tryCatch(
    sf::st_write(grid_export, dsn = gpkg_path,
                 layer = facteur_name, delete_dsn = TRUE, quiet = TRUE),
    error = function(e)
      warning(sprintf("Step2b: export GeoPackage échoué pour %s : %s",
                      facteur_name, e$message))
  )
  message(sprintf("  -> GeoPackage : %s", gpkg_path))
  
  if (exists("Plot_VARz_grid2D", mode = "function")) {
    
    ecart_cols <- grep("^ecart_", names(comp_df), value = TRUE)
    for (ec in ecart_cols) {
      tryCatch(
        Plot_VARz_grid2D(
          VARval     = comp_df[, c("idmesh", ec)],
          grid_plot  = grid_z,
          config     = NULL,
          VARname    = ec,
          VARattr    = paste("Écart absolu REFC —", facteur_name),
          outputsdir = fact_dir
        ),
        error = function(e)
          warning(sprintf("Step2b: carte %s échouée : %s", ec, e$message))
      )
    }
    
    delta_cols <- grep("^delta_rang_", names(comp_df), value = TRUE)
    for (dc in delta_cols) {
      tryCatch(
        Plot_VARz_grid2D(
          VARval     = comp_df[, c("idmesh", dc)],
          grid_plot  = grid_z,
          config     = NULL,
          VARname    = dc,
          VARattr    = paste("Delta rang relatif —", facteur_name),
          outputsdir = fact_dir
        ),
        error = function(e)
          warning(sprintf("Step2b: carte %s échouée : %s", dc, e$message))
      )
    }
  }
  
  invisible(gpkg_path)
}
