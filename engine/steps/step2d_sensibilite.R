# engine/steps/step2d_sensibilite.R
# version du 28.07.2026
#
# Dépendances :
#   engine_step2()        -> engine/functions/engine_step2.R
#   calc_refc()           -> engine/functions/f_REFC_ref_step2.R
#   REF_VALUES            -> engine/functions/f_aux_monte_carlo.R

library(future)
library(future.apply)
library(progressr)

step2d_sensibilite <- function(config, paths, res1, res2b, engine = NULL) {
  
  # ---- BLOC 0 : Guards --------------------------------------------------
  stopifnot(
    "Step2d: res1 doit être une liste"                      = is.list(res1),
    "Step2d: res1$ctx manquant"                             = !is.null(res1$ctx),
    "Step2d: res1$REFC_E1 manquant"                         = !is.null(res1$REFC_E1),
    "Step2d: config$analyses$sensibilite_formelle manquant" =
      !is.null(config$analyses$sensibilite_formelle),
    "Step2d: calc_refc non chargée"                         = exists("calc_refc",    mode = "function"),
    "Step2d: engine_step2 non chargée"                      = exists("engine_step2", mode = "function"),
    "Step2d: REF_VALUES introuvable"                        = exists("REF_VALUES")
  )
  
  message("\n========================================")
  message("   STEP2D - SENSIBILITÉ GLOBALE SHAPLEY")
  message("========================================\n")
  
  # ---- BLOC 1 : Contexte ------------------------------------------------
  ctx        <- res1$ctx
  col_idz    <- ctx$col_idz
  nz         <- ctx$nz
  P_code_sel <- ctx$P_code_sel
  outputsdir <- ctx$outputsdir
  
  sf_cfg <- config$analyses$sensibilite_formelle
  m      <- as.integer(sf_cfg$m    %||% 500L)
  nrep   <- as.integer(sf_cfg$nrep %||% 10L)
  
  stopifnot(
    "Step2d: m doit être un entier positif"    = m > 0,
    "Step2d: nrep doit être un entier positif" = nrep > 0
  )
  
  facteurs_cfg <- sf_cfg$facteurs
  stopifnot(
    "Step2d: sf_cfg$facteurs manquant ou vide" =
      !is.null(facteurs_cfg) && length(facteurs_cfg) > 0
  )
  
  factor_names <- names(facteurs_cfg)
  k            <- length(factor_names)
  stopifnot("Step2d: au moins 2 facteurs requis" = k >= 2)
  
  message(sprintf("Step2d: %d facteurs actifs, m=%d permutations, nrep=%d",
                  k, m, nrep))
  message(sprintf("Step2d: facteurs : %s", paste(factor_names, collapse = ", ")))
  
  # ---- BLOC 2 : Construction / réutilisation du bundle engine -----------
  if (is.null(engine)) {
    message("Step2d: construction du bundle engine (non fourni)...")
    engine <- engine_step2(ctx)
  } else {
    message("Step2d: réutilisation du bundle engine fourni.")
  }
  
  message("Step2d: engine prêt.")
  
  # ---- BLOC 3 : Utilitaires ---------------------------------------------
  
  # complete_params : complète un jeu de paramètres partiels avec REF_VALUES.
  # Utilisé dans run_one_permutation pour garantir que calc_refc reçoit
  # toujours les 10 facteurs, même si seuls certains varient dans la permutation.
  complete_params <- function(partial) {
    params <- REF_VALUES
    for (nm in names(partial)) {
      if (!nm %in% names(params))
        stop(sprintf("Step2d complete_params: facteur inconnu '%s'", nm))
      params[[nm]] <- as.integer(partial[[nm]])
    }
    params
  }
  
  # REFC de référence (step1) aligné sur col_idz
  REFC_ref <- res1$REFC_E1$REFC_E1[
    match(as.character(col_idz), as.character(res1$REFC_E1$idmesh))
  ]
  stopifnot(
    "Step2d: REFC_ref entièrement NA — vérifier res1$REFC_E1" =
      !all(is.na(REFC_ref))
  )
  
  # ---- BLOC 4 : Fonction boîte noire ------------------------------------
  # eval_model reçoit un jeu de params COMPLETS (10 facteurs, valeurs résolues).
  # Pour les facteurs stochastiques actifs (X3/X4/X5/X8 = 1), calc_refc
  # introduit une perturbation aléatoire à chaque appel. On répète nrep fois
  # et on moyenne les Spearman pour obtenir une estimation stable.
  STOCH_FACTORS <- names(FACTOR_REGISTRY)[FACTOR_REGISTRY == "stochastique"]
  
  eval_model <- function(params) {
    
    is_stoch <- any(vapply(STOCH_FACTORS, function(fn) params[[fn]] == 1L, logical(1)))
    
    n_eval        <- if (is_stoch) nrep else 1L
    spearman_vals <- numeric(n_eval)
    
    for (rep_i in seq_len(n_eval)) {
      refc_sim <- calc_refc(params, engine, ctx)
      spearman_vals[rep_i] <- cor(
        REFC_ref, refc_sim,
        method = "spearman",
        use    = "complete.obs"
      )
    }
    
    mean(spearman_vals, na.rm = TRUE)
  }
  
  # Vérification de la combinaison de référence : doit donner Spearman ≈ 1
  message("Step2d: vérification de la combinaison de référence...")
  spearman_ref <- eval_model(REF_VALUES)
  message(sprintf("  Spearman(ref, ref) = %.6f  [attendu ≈ 1.000]", spearman_ref))
  if (abs(spearman_ref - 1) > 0.01) {
    warning(sprintf(
      paste("Step2d: Spearman de référence = %.4f (attendu ≈ 1).",
            "Vérifier que REF_VALUES reproduit exactement le REFC de step1."),
      spearman_ref
    ))
  }
  
  # ---- BLOC 5 : Modalités des facteurs actifs ---------------------------
  # Deux conventions coexistent selon les facteurs :
  #   - X1, X2, X6, X7, X9, X10 : 1 = "aléa" (placeholder à exclure ici, le
  #     tirage aléatoire étant de la responsabilité de run_one_permutation)
  #   - X3, X4, X5, X8   : convention binaire 0/1 ("sans"/"avec"), où 1
  #     est une Vraie modalité et ne doit jamais être filtrée.
  ALEA_FACTORS <- c("X1_mat_AP", "X2_relation_AP", "X6_typo_act", "X7_mat_sensi",
                    "X9_norm_form", "X10_agreg_press")
  
  modalities <- lapply(factor_names, function(fn) {
    vals <- as.integer(facteurs_cfg[[fn]])
    if (fn %in% ALEA_FACTORS) {
      vals <- vals[vals != 1L]
    }
    if (length(vals) == 0)
      stop(sprintf("Step2d: facteur '%s' n'a aucune modalité concrète.", fn))
    vals
  })
  names(modalities) <- factor_names
  
  # ---- BLOC 6 : Algorithme Shapley par permutations (PARALLELISE) -------
  message(sprintf("\nStep2d: lancement de %d permutations Shapley (parallèle)...", m))
  
  v_ref <- spearman_ref
  
  # run_one_permutation :
  #   1. Tire un ordre aléatoire des k facteurs (une permutation).
  #   2. Pour chaque position dans la permutation, échantillonne une modalité
  #      concrète pour le facteur entrant.
  #   3. Calcule la contribution marginale de chaque facteur comme la
  #      différence de valeur (Spearman) avant/après son entrée.
  #
  # La boucle interne sur les positions est SEQUENTIELLE (v_prev dépend
  # de la position précédente). Seule la boucle EXTERNE sur les m
  # permutations est parallélisée.
  
  run_one_permutation <- function(perm_i) {
    
    perm         <- sample(factor_names)
    sampled_vals <- lapply(modalities, function(mods) sample(mods, 1L))
    
    contrib_vec <- setNames(numeric(k), factor_names)
    current     <- list()   # facteurs déjà entrés avec leur valeur tirée
    v_prev      <- v_ref
    
    for (pos in seq_along(perm)) {
      fi <- perm[pos]
      current[[fi]] <- sampled_vals[[fi]]
      
      # Paramètres complets : facteurs entrés à leur valeur tirée,
      # facteurs non encore entrés à REF_VALUES
      params_with <- complete_params(current)
      v_with      <- eval_model(params_with)
      
      contrib_vec[[fi]] <- v_prev - v_with
      v_prev            <- v_with
    }
    
    contrib_vec
  }
  
  n_workers <- max(1L, parallel::detectCores() - 1L)
  old_plan  <- future::plan()
  plan(multisession, workers = n_workers)
  on.exit(future::plan(old_plan), add = TRUE)
  
  handlers(global = TRUE)
  handlers("txtprogressbar")
  
  list_contribs <- with_progress({
    p <- progressor(steps = m)
    future_lapply(
      seq_len(m),
      function(perm_i) {
        res <- run_one_permutation(perm_i)
        p(sprintf("permutation %d/%d", perm_i, m))
        res
      },
      future.seed = TRUE
    )
  })
  
  message("\n\nPermutations terminées !")
  
  # ---- BLOC 7 : Agrégation et normalisation -----------------------------
  shapley_sum  <- Reduce(`+`, list_contribs)
  shapley_vals <- shapley_sum / m
  
  total_abs    <- sum(abs(shapley_vals))
  shapley_norm <- if (total_abs > 0) abs(shapley_vals) / total_abs else
    setNames(rep(1 / k, k), factor_names)
  
  results_df <- data.frame(
    facteur      = factor_names,
    shapley      = shapley_vals[factor_names],
    shapley_abs  = abs(shapley_vals[factor_names]),
    shapley_norm = shapley_norm[factor_names],
    rang         = rank(-abs(shapley_vals[factor_names]), ties.method = "min"),
    stringsAsFactors = FALSE
  )
  results_df <- results_df[order(results_df$rang), ]
  rownames(results_df) <- NULL
  
  message("\nStep2d — Valeurs de Shapley (contribution à la variabilité du classement) :")
  message(sprintf("  %-20s  %8s  %8s  %5s",
                  "Facteur", "Shapley", "Abs.norm", "Rang"))
  for (i in seq_len(nrow(results_df))) {
    message(sprintf("  %-20s  %+8.5f  %8.4f  %5d",
                    results_df$facteur[i],
                    results_df$shapley[i],
                    results_df$shapley_norm[i],
                    results_df$rang[i]))
  }
  
  # ---- BLOC 8 : Export --------------------------------------------------
  sens_dir <- file.path(outputsdir, "sensibilite_shapley")
  dir.create(sens_dir, recursive = TRUE, showWarnings = FALSE)
  run_ts <- format(Sys.time(), "%Y%m%d_%H%M")
  
  csv_path <- file.path(sens_dir,
                        paste0("shapley_results_", run_ts, ".csv"))
  tryCatch(
    write.csv(results_df, csv_path, row.names = FALSE),
    error = function(e)
      warning(sprintf("Step2d: export CSV échoué : %s", e$message))
  )
  message(sprintf("  -> CSV : %s", csv_path))
  
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    synthese <- list(
      run_ts          = run_ts,
      m               = m,
      nrep            = nrep,
      k               = k,
      facteurs_actifs = factor_names,
      spearman_ref    = spearman_ref,
      shapley         = as.list(shapley_vals[factor_names]),
      shapley_norm    = as.list(shapley_norm[factor_names]),
      rang            = as.list(setNames(results_df$rang, results_df$facteur))
    )
    json_path <- file.path(sens_dir,
                           paste0("shapley_synthese_", run_ts, ".json"))
    tryCatch(
      jsonlite::write_json(synthese, json_path,
                           auto_unbox = TRUE, pretty = TRUE),
      error = function(e)
        warning(sprintf("Step2d: export JSON échoué : %s", e$message))
    )
    message(sprintf("  -> JSON : %s", json_path))
  }
  
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    
    plot_df         <- results_df
    plot_df$facteur <- factor(plot_df$facteur,
                              levels = rev(plot_df$facteur[order(plot_df$rang)]))
    
    p <- ggplot2::ggplot(plot_df,
                         ggplot2::aes(x = facteur, y = shapley_norm)) +
      ggplot2::geom_col(fill = "#96d1de", alpha = 0.4, width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = ggplot2::expansion(mult = c(0, 0.05))
      ) +
      ggplot2::labs(
        title    = "Contribution de chaque facteur à la variabilité du classement spatial REFC",
        subtitle = sprintf(
          "(m=%d permutations)",
          m
        ),
        x = NULL,
        y = "Contribution normalisée"
      ) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        plot.title         = ggplot2::element_text(face = "bold"),
        panel.grid.major.y = ggplot2::element_blank()
      )
    
    png_path <- file.path(sens_dir,
                          paste0("shapley_barplot_", run_ts, ".png"))
    tryCatch({
      ggplot2::ggsave(png_path, p, width = 8, height = 5, dpi = 150)
      message(sprintf("  -> Graphique : %s", png_path))
    },
    error = function(e)
      warning(sprintf("Step2d: export graphique échoué : %s", e$message))
    )
  }
  
  message("\n========================================")
  message("   STEP2D TERMINÉ")
  message("========================================\n")
  
  return(list(
    results      = results_df,
    shapley_vals = shapley_vals,
    shapley_norm = shapley_norm,
    spearman_ref = spearman_ref,
    m            = m,
    nrep         = nrep,
    k            = k,
    csv_path     = csv_path,
    json_path    = if (exists("json_path")) json_path else NULL,
    png_path     = if (exists("png_path"))  png_path  else NULL
  ))
}
