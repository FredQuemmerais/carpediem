# engine/steps/step2c_robustesse_stoch.R
# version du 06.05.2026
#
# ============================================================
# RÔLE DE CE SCRIPT
# ============================================================
# Analyse de robustesse stochastique : caractérise la dispersion
# des résultats Monte Carlo (res2) par rapport au REFC de
# référence (res1) et évalue la stabilité spatiale des zones
# à enjeu face aux facteurs d'incertitude stochastiques.
#
# Ce script consomme res2$simul_results (produit par step2) et
# res1$REFC_E1 (REFC de référence déterministe de step1).
#
# ============================================================
# INDICATEURS PRODUITS
# ============================================================
#
# Par maille (sensi_stat) :
#   cv_REFC      : coefficient de variation des REFC simulés
#                  Interprétation : dispersion relative autour de
#                  la moyenne simulée. Élevé = maille sensible aux
#                  facteurs d'incertitude activés.
#
#   IQR_REFC     : étendue interquantile (Q75 - Q25) des REFC simulés
#                  Interprétation : mesure robuste de la dispersion,
#                  moins sensible aux valeurs extrêmes que le CV.
#
#   REFC_ref     : REFC de référence issu de step1 (déterministe)
#
#   REFC_moy     : moyenne des REFC simulés
#
#   ecart_moy_ref: écart absolu entre REFC_moy et REFC_ref
#                  Interprétation : biais moyen introduit par les
#                  facteurs d'incertitude par rapport au résultat
#                  déterministe.
#
# Globaux (synthese) :
#   spearman_ref_vs_moy : corrélation de Spearman entre REFC_ref
#                  et REFC_moy sur toutes les mailles.
#                  Proche de 1 = classement stable malgré les
#                  perturbations (signe de robustesse globale).
#
#   cv_moyen     : CV moyen sur toutes les mailles
#   IQR_moyen    : IQR moyen sur toutes les mailles
#   pct_top25_stable : % de mailles du top 25% (ref) restant
#                  dans le top 25% de la distribution simulée
#                  moyenne — stabilité spatiale des zones à enjeu.
#
# ============================================================
# SORTIES CARTOGRAPHIQUES
# ============================================================
# GeoPackage dans outputs/robustesse_stoch/
# Cartes via Plot_VARz_grid2D si disponible :
#   - carte du CV par maille
#   - carte de l'IQR par maille
#   - carte de l'écart absolu ref vs moy

step2c_robustesse_stoch <- function(config, res1, res2) {
  
  # ---- Guards -----------------------------------------------------------
  stopifnot(
    "Step2c: res1 doit être une liste"           = is.list(res1),
    "Step2c: res2 doit être une liste"           = is.list(res2),
    "Step2c: res1$ctx manquant"                  = !is.null(res1$ctx),
    "Step2c: res1$REFC_E1 manquant"              = !is.null(res1$REFC_E1),
    "Step2c: res2$simul_results manquant"        = !is.null(res2$simul_results),
    "Step2c: res2$stats_simul manquant"          = !is.null(res2$stats_simul)
  )
  
  message("\n========================================")
  message("   STEP2C - ROBUSTESSE STOCHASTIQUE")
  message("========================================\n")
  
  ctx        <- res1$ctx
  grid_z     <- ctx$grid_z
  col_idz    <- ctx$col_idz
  nz         <- ctx$nz
  outputsdir <- ctx$outputsdir
  
  rob_dir <- file.path(outputsdir, "robustesse_stoch")
  dir.create(rob_dir, recursive = TRUE, showWarnings = FALSE)
  
  run_ts <- format(Sys.time(), "%Y%m%d_%H%M")
  
  # -----------------------------------------------------------------------
  # Extraction des simulations
  # -----------------------------------------------------------------------
  simul_results <- res2$simul_results
  stats_simul   <- res2$stats_simul
  nsim          <- res2$nsim
  
  sim_cols <- grep("^simul\\d+$", names(simul_results), value = TRUE)
  stopifnot(
    "Step2c: aucune colonne simul* dans simul_results" = length(sim_cols) > 0
  )
  
  sim_mat <- as.matrix(simul_results[, sim_cols, drop = FALSE])
  
  # -----------------------------------------------------------------------
  # REFC de référence (step1) aligné sur col_idz
  # -----------------------------------------------------------------------
  REFC_ref <- res1$REFC_E1$REFC_E1[
    match(as.character(col_idz), as.character(res1$REFC_E1$idmesh))
  ]
  
  if (all(is.na(REFC_ref))) {
    warning("Step2c: REFC_ref entièrement NA — vérifier res1$REFC_E1$idmesh vs col_idz.")
  }
  
  # -----------------------------------------------------------------------
  # Indicateurs par maille
  # -----------------------------------------------------------------------
  message("Step2c: calcul des indicateurs par maille...")
  
  q25 <- apply(sim_mat, 1, quantile, probs = 0.25, na.rm = TRUE)
  q75 <- apply(sim_mat, 1, quantile, probs = 0.75, na.rm = TRUE)
  
  # CV et IQR
  cv_vec  <- if ("cv_REFC" %in% names(stats_simul)) {
    stats_simul$cv_REFC[match(as.character(col_idz),
                              as.character(stats_simul$idmesh))]
  } else {
    # Recalcul de sécurité
    sd_vec  <- apply(sim_mat, 1, sd,   na.rm = TRUE)
    moy_vec <- apply(sim_mat, 1, mean, na.rm = TRUE)
    ifelse(moy_vec > 0, sd_vec / moy_vec, 0)
  }
  
  IQR_vec <- q75 - q25
  
  REFC_moy <- if ("REFC_moy" %in% names(stats_simul)) {
    stats_simul$REFC_moy[match(as.character(col_idz),
                               as.character(stats_simul$idmesh))]
  } else {
    rowMeans(sim_mat, na.rm = TRUE)
  }
  
  ecart_moy_ref <- abs(REFC_moy - REFC_ref)
  
  # Delta de rang : robustesse du classement spatial du risque
  # rang 1 = maille la plus à risque (REFC le plus élevé)
  rang_ref <- rank(-REFC_ref, ties.method = "min", na.last = "keep")
  rang_moy <- rank(-REFC_moy, ties.method = "min", na.last = "keep")
  
  n_rang <- sum(!is.na(REFC_ref) & !is.na(REFC_moy))
  
  # delta_rang > 0 : la maille RECULE (rang moyen simulé > rang réf) 
  #                  -> risque relatif moindre en simulation qu'en référence
  # delta_rang < 0 : la maille AVANCE (rang moyen simulé < rang réf)
  #                  -> risque relatif plus fort en simulation qu'en référence
  delta_rang     <- rang_moy - rang_ref
  delta_rang_abs <- abs(delta_rang)
  delta_rang_pct <- if (n_rang > 0) delta_rang / n_rang * 100 else NA_real_
  
  sensi_stat <- data.frame(
    idmesh         = as.character(col_idz),
    cv_REFC        = cv_vec,
    IQR_REFC       = IQR_vec,
    REFC_ref       = REFC_ref,
    REFC_moy       = REFC_moy,
    ecart_moy_ref  = ecart_moy_ref,
    rang_ref       = rang_ref,
    rang_moy       = rang_moy,
    delta_rang     = delta_rang,
    delta_rang_abs = delta_rang_abs,
    delta_rang_pct = delta_rang_pct,
    stringsAsFactors = FALSE
  )
  
  # -----------------------------------------------------------------------
  # Indicateurs globaux de synthèse
  # -----------------------------------------------------------------------
  message("Step2c: calcul des indicateurs globaux...")
  
  spearman_ref_vs_moy <- cor(
    REFC_ref, REFC_moy,
    method = "spearman",
    use    = "complete.obs"
  )
  
  cv_moyen  <- mean(cv_vec,  na.rm = TRUE)
  IQR_moyen <- mean(IQR_vec, na.rm = TRUE)
  
  # Stabilité top 25% / top 10% : mailles dans le top N% du REFC_ref
  # qui restent dans le top N% du REFC_moy
  q75_ref <- quantile(REFC_ref, 0.75, na.rm = TRUE)
  q75_moy <- quantile(REFC_moy, 0.75, na.rm = TRUE)
  top25_ref <- REFC_ref >= q75_ref
  top25_moy <- REFC_moy >= q75_moy
  pct_top25_stable <- sum(top25_ref & top25_moy, na.rm = TRUE) / sum(top25_ref, na.rm = TRUE) * 100
  
  q90_ref <- quantile(REFC_ref, 0.90, na.rm = TRUE)
  q90_moy <- quantile(REFC_moy, 0.90, na.rm = TRUE)
  top10_ref <- REFC_ref >= q90_ref
  top10_moy <- REFC_moy >= q90_moy
  pct_top10_stable <- sum(top10_ref & top10_moy, na.rm = TRUE) / sum(top10_ref, na.rm = TRUE) * 100
  
  sensi_stat$top25_ref <- top25_ref
  sensi_stat$top25_moy <- top25_moy
  sensi_stat$stable_top25 <- top25_ref & top25_moy
  sensi_stat$top10_ref <- top10_ref
  sensi_stat$top10_moy <- top10_moy
  sensi_stat$stable_top10 <- top10_ref & top10_moy
  
  ecart_moy_ref_moyen <- mean(ecart_moy_ref, na.rm = TRUE)
  
  delta_rang_abs_moyen  <- mean(delta_rang_abs,   na.rm = TRUE)
  delta_rang_abs_median <- median(delta_rang_abs, na.rm = TRUE)
  
  # part des mailles dont le rang varie de moins de X % de l'effectif total
  seuil_stable_pct <- 5
  pct_rang_stable  <- sum(abs(delta_rang_pct) <= seuil_stable_pct, na.rm = TRUE) /
    sum(!is.na(delta_rang_pct)) * 100
  
  synthese <- list(
    nsim                  = nsim,
    spearman_ref_vs_moy   = spearman_ref_vs_moy,
    cv_moyen              = cv_moyen,
    IQR_moyen             = IQR_moyen,
    ecart_moy_ref_moyen   = ecart_moy_ref_moyen,
    delta_rang_abs_moyen  = delta_rang_abs_moyen,
    delta_rang_abs_median = delta_rang_abs_median,
    pct_rang_stable       = pct_rang_stable,
    seuil_stable_pct      = seuil_stable_pct,
    pct_top25_stable      = pct_top25_stable,
    pct_top10_stable      = pct_top10_stable,
    facteurs_actifs       = res2$factors
  )
  
  message(sprintf(
    "Step2c synthèse :\n  Spearman REFC_ref vs REFC_moy = %.3f\n  Delta de rang moyen (abs)     = %.1f\n  CV moyen                      = %.3f\n  IQR moyen                     = %.3f\n  Stabilité top 25%%             = %.1f%%",
    spearman_ref_vs_moy, delta_rang_abs_moyen, cv_moyen, IQR_moyen, pct_top25_stable
  ))
  
  # -----------------------------------------------------------------------
  # Export GeoPackage
  # -----------------------------------------------------------------------
  gpkg_path <- file.path(rob_dir,
                         paste0("robustesse_stoch_", run_ts, ".gpkg"))
  
  grid_export <- grid_z |>
    dplyr::left_join(sensi_stat, by = "idmesh")
  
  tryCatch(
    sf::st_write(grid_export, dsn = gpkg_path,
                 layer = "robustesse_stoch",
                 delete_dsn = TRUE, quiet = TRUE),
    error = function(e)
      warning(sprintf("Step2c: export GeoPackage échoué : %s", e$message))
  )
  message(sprintf("  -> GeoPackage : %s", gpkg_path))
  
  # -----------------------------------------------------------------------
  # Export GeoJSON pour carte Leaflet dans le rapport HTML
  # Reprojection en WGS84 obligatoire pour Leaflet.
  # Seules les colonnes nécessaires à la carte sont exportées.
  # -----------------------------------------------------------------------
  maps_dir <- file.path(rob_dir, "maps")
  dir.create(maps_dir, recursive = TRUE, showWarnings = FALSE)
  
  geojson_path <- file.path(maps_dir, "delta_rang.geojson")
  
  grid_geojson <- grid_z |>
    dplyr::left_join(
      sensi_stat |>
        dplyr::select(idmesh, delta_rang, delta_rang_abs, delta_rang_pct,
                      rang_ref, rang_moy, ecart_moy_ref, cv_REFC, IQR_REFC,
                      REFC_ref, REFC_moy),
      by = "idmesh"
    ) |>
    sf::st_transform(4326)
  
  tryCatch(
    sf::st_write(grid_geojson, dsn = geojson_path,
                 delete_dsn = TRUE, quiet = TRUE),
    error = function(e)
      warning(sprintf("Step2c: export GeoJSON échoué : %s", e$message))
  )
  message(sprintf("  -> GeoJSON carte : %s", geojson_path))
  
  # -----------------------------------------------------------------------
  # Cartes optionnelles via Plot_VARz_grid2D
  # -----------------------------------------------------------------------
  if (exists("Plot_VARz_grid2D", mode = "function")) {
    
    cartes <- list(
      list(col = "cv_REFC",        attr = "Coefficient de variation du REFC (Monte Carlo)"),
      list(col = "IQR_REFC",       attr = "Étendue interquantile du REFC (Monte Carlo)"),
      list(col = "ecart_moy_ref",  attr = "Écart absolu REFC_moy vs REFC_ref"),
      list(col = "delta_rang_abs", attr = "Delta de rang absolu (défaut vs simulations)")
    )
    
    for (carte in cartes) {
      tryCatch(
        Plot_VARz_grid2D(
          VARval     = sensi_stat[, c("idmesh", carte$col)],
          grid_plot  = grid_z,
          config     = NULL,
          VARname    = carte$col,
          VARattr    = carte$attr,
          outputsdir = rob_dir
        ),
        error = function(e)
          warning(sprintf("Step2c: carte %s échouée : %s", carte$col, e$message))
      )
    }
  }
  
  # -----------------------------------------------------------------------
  # Export JSON synthèse
  # -----------------------------------------------------------------------
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    json_path <- file.path(rob_dir,
                           paste0("synthese_robustesse_stoch_", run_ts, ".json"))
    tryCatch(
      jsonlite::write_json(synthese, json_path,
                           auto_unbox = TRUE, pretty = TRUE),
      error = function(e)
        warning(sprintf("Step2c: export JSON échoué : %s", e$message))
    )
    message(sprintf("  -> JSON synthèse : %s", json_path))
  }
  
  message("\n========================================")
  message("   STEP2C TERMINÉ")
  message("========================================\n")
  
  return(list(
    sensi_stat   = sensi_stat,
    synthese     = synthese,
    gpkg_path    = gpkg_path,
    geojson_path = geojson_path
  ))
}
