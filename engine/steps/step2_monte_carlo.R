# engine/steps/step2_monte_carlo.R
# Boucle de tirages Monte Carlo
# version parallélisée du 24.07.2026
#
# Dépendances :
#   engine_step2()        -> engine/functions/engine_step2.R
#   draw_params()         -> engine/functions/f_REFC_ref_step2.R
#   calc_refc()           -> engine/functions/f_REFC_ref_step2.R
#   FACTOR_REGISTRY, REF_VALUES -> engine/functions/f_aux_monte_carlo.R
#
# Facteurs d'incertitudes :
#
# X1_mat_AP      : 1=aléa, 2=médiane (défaut), 3=précaution, 4=binaire
# X2_relation_AP : 1=aléa, 2=linéaire (défaut), 3=optimiste, 4=pessimiste, 5=logistique
# X3_var_sensi   : 0=sans IC (défaut), 1=avec IC
# X4_var_ccr_AP  : 0=sans IC (défaut), 1=avec IC
# X5_var_act     : 0=sans IQ (défaut), 1=avec IQ
# X6_typo_act    : 1=aléa, 2=natif (défaut), 3=agrégé N2, 4=agrégé N1
# X7_mat_sensi   : 1=aléa, 2=précaution (défaut), 3=médiane
# X8_var_hab     : 0=sans (défaut), 1=avec perturbation des surfaces habitats
# X9_norm_form   : 1=aléa, 2=log (défaut), 3=linéaire, 4=sigmoïde
# X10_agreg_press: 1=aléa, 2=additif (défaut), 3=synergique, 4=antagoniste

library(future)
library(future.apply)
library(progressr)

step2_monte_carlo <- function(config, paths, con, res1, engine = NULL) {
  
  # ---- ETAPE 1 : Guards et contexte ------------------------------------
  {
    stopifnot(
      "Step2: res1 doit être une liste"                 = is.list(res1),
      "Step2: res1$ctx manquant"                        = !is.null(res1$ctx),
      "Step2: config$analyses$monte_carlo manquant"     = !is.null(config$analyses$monte_carlo),
      "Step2: res1$ctx$P_code_sel manquant"             = !is.null(res1$ctx$P_code_sel),
      "Step2: res1$ctx$typo_press manquant"             = !is.null(res1$ctx$typo_press),
      "Step2: res1$ctx$Mat_Sensib_Pj manquant"          = !is.null(res1$ctx$Mat_Sensib_Pj),
      "Step2: res1$ctx$seuil_nodata manquant"           = !is.null(res1$ctx$seuil_nodata),
      "Step2: calc_refc non chargée"                    = exists("calc_refc",        mode = "function"),
      "Step2: draw_params non chargée"                  = exists("draw_params",      mode = "function"),
      "Step2: engine_step2 non chargée"                 = exists("engine_step2",     mode = "function"),
      "Step2: FACTOR_REGISTRY introuvable"              = exists("FACTOR_REGISTRY"),
      "Step2: REF_VALUES introuvable"                   = exists("REF_VALUES")
    )
    
    message("\n========================================")
    message("   STEP2 - SIMULATION MONTE-CARLO")
    message("========================================\n")
    
    ctx         <- res1$ctx
    col_idz     <- ctx$col_idz
    nz          <- ctx$nz
    grid_z      <- ctx$grid_z
    lcond_SRM   <- ctx$lcond_SRM
    P_code_sel  <- ctx$P_code_sel
    seuil_nodata <- ctx$seuil_nodata
    
    mc_cfg     <- config$analyses$monte_carlo
    nsim       <- mc_cfg$nbsimul
    thres_stat <- mc_cfg$thres_stat
    factors    <- mc_cfg$factors
    
    stopifnot(
      "Step2: nbsimul introuvable"  = !is.null(nsim)  && is.numeric(nsim) && nsim > 0,
      "Step2: thres_stat introuvable" = !is.null(thres_stat) && length(thres_stat) > 0,
      "Step2: factors introuvable"  = !is.null(factors) && is.list(factors)
    )
    
    # Validation sommaire des facteurs (draw_params gère les défauts et warnings)
    message(sprintf(
      "Step2: %d simulations | facteurs : %s",
      nsim,
      paste(names(factors), unlist(factors), sep = "=", collapse = ", ")
    ))
  }
  
  # ---- ETAPE 2 : Construction / réutilisation du bundle engine ----------
  {
    if (is.null(engine)) {
      message("\nStep2: construction du bundle engine...")
      engine <- engine_step2(ctx)
      
      message("Step2: bundle engine prêt.\n")
    } else {
      message("\nStep2: réutilisation du bundle engine fourni.")
    }
  }
  
  # ---- ETAPE 3 : Boucle de simulation (PARALLELISEE) -------------------
  
  # draw_params() résout les modalités "aléa" (valeur 1) en valeurs concrètes.
  # calc_refc() exécute un tirage complet pour un jeu de paramètres résolu.
  # Les deux fonctions sont définies dans f_REFC_ref_step2.R et capturées
  # par la closure via l'environnement exporté vers les workers future.
  
  run_one_simul <- function(k) {
    params <- draw_params(factors)
    calc_refc(params, engine, ctx)
  }
  
  message(sprintf("Lancement de %d simulations Monte-Carlo (parallèle)...\n", nsim))
  
  n_workers <- max(1L, parallel::detectCores() - 1L)
  old_plan  <- future::plan()
  plan(multisession, workers = n_workers)
  on.exit(future::plan(old_plan), add = TRUE)
  
  handlers(global = TRUE)
  handlers("txtprogressbar")
  
  list_REFC <- with_progress({
    p <- progressor(steps = nsim)
    future_lapply(
      seq_len(nsim),
      function(k) {
        res <- run_one_simul(k)
        p()
        res
      },
      future.seed     = TRUE,
      future.packages = c("progressr", "dplyr", "sf")
    )
  })
  
  message("\n\nSimulations terminées !")
  
  # Reconstruction de simul_results
  simul_results <- data.frame(idmesh = col_idz)
  for (k in seq_len(nsim)) {
    simul_results[[paste0("simul", k)]] <- list_REFC[[k]]
  }
  
  # ---- ETAPE 4 : Statistiques ------------------------------------------
  message("\nCalcul des statistiques...")
  
  stats_simul    <- stat_tab(simul_results)
  simul_calc_res <- simul_calc(simul_results, thres_stat)
  
  simul_stat <- data.frame(idmesh = col_idz)
  for (i in seq_along(thres_stat)) {
    m <- simul_calc_res[[i]][, -1, drop = FALSE]
    simul_stat[[paste0("top",    thres_stat[i])]] <- rowSums(m ==  1) / nsim * 100
    simul_stat[[paste0("bottom", thres_stat[i])]] <- rowSums(m == -1) / nsim * 100
  }
  simul_stat <- cbind(simul_stat, stats_simul[, -1])
  
  # ---- ETAPE 5 : Export ------------------------------------------------
  if (isTRUE(mc_cfg$export_simul) || isTRUE(mc_cfg$export_result)) {
    message("\nExport vers PostgreSQL...")
    export_con <- get_db_conn(config, mc_cfg$db_choice %||% "local")
    on.exit(db_disconnect_safe(export_con), add = TRUE)
    
    schema <- mc_cfg$export_scheme
    if (is.null(schema) || !nzchar(schema)) schema <- "results_monte_carlo"
    DBI::dbExecute(export_con,
                   sprintf("CREATE SCHEMA IF NOT EXISTS %s", schema))
    run_ts <- format(Sys.time(), "%Y%m%d_%H%M")
    
    if (isTRUE(mc_cfg$export_simul)) {
      tn       <- paste("simul", nsim,
                        tolower(paste(lcond_SRM, collapse = "_")),
                        run_ts, sep = "_")
      sim_cols <- grep("^simul\\d+$", names(simul_results), value = TRUE)
      simul_results[sim_cols] <- lapply(simul_results[sim_cols], as.numeric)
      stopifnot(!anyNA(simul_results[sim_cols]))
      export_table(export_con, schema, grid_z, simul_results, tn)
      message(sprintf("  -> %s.%s", schema, tn))
    }
    if (isTRUE(mc_cfg$export_result)) {
      tn <- paste("stat_simul", nsim,
                  tolower(paste(lcond_SRM, collapse = "_")),
                  run_ts, sep = "_")
      export_table(export_con, schema, grid_z, simul_stat, tn)
      message(sprintf("  -> %s.%s", schema, tn))
    }
  }
  
  message("\n========================================")
  message("   STEP2 TERMINÉ")
  message("========================================\n")
  
  return(list(
    simul_results = simul_results,
    simul_stat    = simul_stat,
    stats_simul   = stats_simul,
    nsim          = nsim,
    thres_stat    = thres_stat,
    factors       = factors,
    engine        = engine   # transmis pour réutilisation par step2b/step2d
  ))
}
