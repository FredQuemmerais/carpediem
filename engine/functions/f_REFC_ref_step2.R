# engine/functions/f_REFC_ref_step2.R
# Calcul du REFC, utilisée par step2 (Monte Carlo), step2b (robustesse déterministe) et step2d (sensibilité Shapley)
# Consomme le bundle `engine` (engine_step2.R)
#
# IMPORTANT — Ce que calc_refc() NE fait PAS :
#   - Il ne tire AUCUNE modalité "aléa" (X*=1). `params` doit contenir des
#     valeurs résolues (ex: X1_mat_AP = 2, jamais 1). La résolution des
#     "aléa" est la responsabilité de l'appelant (draw_params() dans step2,
#     boucle de scénarios dans step2b, échantillonnage Shapley dans step2d).
#   - Il ne répète PAS les appels pour les facteurs stochastiques (X3/X4/X5/
#     X8, cf FACTOR_REGISTRY). Un appel = un tirage. Moyenner sur `nrep`
#     appels reste la responsabilité de l'appelant (step2d).

# -----------------------------------------------------------------------
# Fonction moteur
# -----------------------------------------------------------------------
# params : liste nommée avec les 10 facteurs, valeurs RÉSOLUES (jamais 1) :
#   X1_mat_AP       in {2,3,4}          (2=médiane, 3=précaution, 4=binaire)
#   X2_relation_AP  in {2,3,4,5}        (2=lin, 3=optimiste, 4=pessimiste, 5=logistique)
#   X3_var_sensi    in {0,1}
#   X4_var_ccr_AP   in {0,1}
#   X5_var_act      in {0,1}
#   X6_typo_act     in {2,4}            (2=fin, 3= intermédiaire, 4=grossier)
#   X7_mat_sensi    in {2,3}            (2=précaution, 3=médiane -- attention, inverse de X1)
#   X8_var_hab      in {0,1}
#   X9_norm_form    in {2,3,4}          (2=log, 3=linear, 4=sigmoid)
#   X10_agreg_press in {2,3,4}          (2=additif, 3=synergique, 4=antagoniste)
#
# engine : bundle retourné par engine_step2(ctx)
# ctx    : contexte de step1 (grid_z, col_idz, nz, P_code_sel, A_zi_lognorm,
#          typo_press, HabBenth_z, ...)
# Retour : vecteur numérique REFC, aligné sur ctx$col_idz.

calc_refc <- function(params, engine, ctx) {
  
  # ---- Guards -------------------------------------------------------------
  stopifnot(
    "calc_refc: params doit être une liste"    = is.list(params),
    "calc_refc: engine doit être une liste"    = is.list(engine),
    "calc_refc: ctx doit être une liste"       = is.list(ctx)
  )
  
  req_params <- c("X1_mat_AP", "X2_relation_AP", "X3_var_sensi", "X4_var_ccr_AP",
                  "X5_var_act", "X6_typo_act", "X7_mat_sensi", "X8_var_hab",
                  "X9_norm_form", "X10_agreg_press")
  missing_p <- setdiff(req_params, names(params))
  if (length(missing_p) > 0) {
    stop("calc_refc: paramètre(s) manquant(s) dans params : ",
         paste(missing_p, collapse = ", "))
  }
  
  X1  <- as.integer(params$X1_mat_AP)
  X2  <- as.integer(params$X2_relation_AP)
  X3  <- as.integer(params$X3_var_sensi)
  X4  <- as.integer(params$X4_var_ccr_AP)
  X5  <- as.integer(params$X5_var_act)
  X6  <- as.integer(params$X6_typo_act)
  X7  <- as.integer(params$X7_mat_sensi)
  X8  <- as.integer(params$X8_var_hab)
  X9  <- as.integer(params$X9_norm_form)
  X10 <- as.integer(params$X10_agreg_press)
  
  stopifnot(
    "calc_refc: X1_mat_AP doit être dans {2,3,4} (résolu, pas 'aléa')"      = X1  %in% 2:4,
    "calc_refc: X2_relation_AP doit être dans {2,3,4,5}"                    = X2  %in% 2:5,
    "calc_refc: X3_var_sensi doit être 0 ou 1"                              = X3  %in% 0:1,
    "calc_refc: X4_var_ccr_AP doit être 0 ou 1"                             = X4  %in% 0:1,
    "calc_refc: X5_var_act doit être 0 ou 1"                                = X5  %in% 0:1,
    "calc_refc: X6_typo_act doit être dans {2,3,4} (résolu, pas 'aléa')"    = X6 %in% 2:4,
    "calc_refc: X7_mat_sensi doit être dans {2,3,4} (résolu, pas 'aléa')"   = X7 %in% 2:4,
    "calc_refc: X8_var_hab doit être 0 ou 1"                                = X8  %in% 0:1,
    "calc_refc: X9_norm_form doit être dans {2,3,4} (résolu, pas 'aléa')"    = X9  %in% 2:4,
    "calc_refc: X10_agreg_press doit être dans {2,3,4} (résolu, pas 'aléa')" = X10 %in% 2:4
  )
  
  if (X5 == 1L && !isTRUE(engine$has_X5)) {
    stop("calc_refc: X5_var_act=1 demandé mais engine$has_X5=FALSE ",
         "(ctx$A_iq_aligned / ctx$iq_col_by_code indisponibles lors de engine_step2()).")
  }
  
  col_idz    <- ctx$col_idz
  nz         <- ctx$nz
  P_code_sel <- ctx$P_code_sel
  
  norm_mode  <- switch(as.character(X9),  "2" = "log", "3" = "linear",  "4" = "sigmoid")
  agreg_mode <- switch(as.character(X10), "2" = "additif", "3" = "synergique", "4" = "antagoniste")
  
  # ---- X1 : sélection de la matrice A-P (lien + IC) ------------------------
  mat_sel <- switch(as.character(X1),
                    "2" = engine$mat_ap$mediane,
                    "3" = engine$mat_ap$precaution,
                    "4" = engine$mat_ap$binaire)
  MatAP_lien <- mat_sel$lien
  MatAP_IC   <- mat_sel$ic
  
  # ---- X4 : perturbation des CCR ------------------------------------------
  if (X4 == 1L) {
    for (i in seq_len(nrow(MatAP_lien))) {
      for (j in seq_len(ncol(MatAP_lien))) {
        if (is.finite(MatAP_lien[i, j]))
          MatAP_lien[i, j] <- f_AP(MatAP_lien[i, j], MatAP_IC[i, j])
      }
    }
  }
  
  # ---- X2 : forme de la relation A-P --------------------------------------
  transform_ap <- switch(as.character(X2),
                         "2" = identity, "3" = optimiste,
                         "4" = pessimiste, "5" = logistique)
  MatAP_t <- MatAP_lien
  MatAP_t[is.finite(MatAP_t)] <- transform_ap(MatAP_t[is.finite(MatAP_t)])
  MatAP_t[!is.finite(MatAP_t)] <- 0
  
  # ---- X6 : sélection du niveau de granularité de la typologie activité ---
  level_key    <- switch(as.character(X6), "2" = "natif", "3" = "N2", "4" = "N1")
  A_sim        <- engine$A_lognorm_by_level[[level_key]]
  A_sim$idmesh <- as.character(A_sim$idmesh)
  
  # ---- X5 : perturbation IQ des activités simulées -------------------------
  # L'IQ appliquée est celle précalculée pour le niveau choisi par X6 (moyenne
  # pondérée par l'intensité brute des enfants regroupés) : X5 et X6 sont
  # ainsi réellement combinables, y compris sur les activités agrégées.
  if (X5 == 1L) {
    iq_info      <- engine$iq_by_level[[level_key]]
    act_cols_sim <- setdiff(names(A_sim), "idmesh")
    for (j in seq_along(act_cols_sim)) {
      col_j  <- act_cols_sim[j]
      code_a <- gsub("_norm$", "", col_j)
      
      iq_vec   <- iq_info$iq_by_code[[code_a]]   %||% rep(5, nz)
      miss_vec <- iq_info$miss_by_code[[code_a]] %||% rep(FALSE, nz)
      iq_vec[miss_vec] <- 0
      
      A_sim[[col_j]] <- vapply(seq_len(nz), function(ii)
        f_sensib(A_sim[[col_j]][ii], iq_vec[ii], 1), numeric(1))
    }
  }
  
  # ---- Pressions simulées (feuilles) --------------------------------------
  act_cols_norm <- setdiff(names(A_sim), "idmesh")
  A_mat         <- as.matrix(A_sim[, act_cols_norm, drop = FALSE])
  rownames(A_mat) <- as.character(A_sim$idmesh)
  act_codes     <- tolower(gsub("_norm$", "", act_cols_norm))
  idx_act       <- match(act_codes, rownames(MatAP_t))
  
  Mat_Pj_sim <- data.frame(idmesh = col_idz)
  for (p in P_code_sel) {
    if (!p %in% colnames(MatAP_t)) { Mat_Pj_sim[[p]] <- 0; next }
    ap_vec <- MatAP_t[idx_act, p]
    ap_vec[!is.finite(ap_vec)] <- 0
    Mat_Pj_sim[[p]] <- as.numeric(A_mat %*% ap_vec)
  }
  
  Mat_Pj_norm_z <- Mat_Pj_sim |>
    dplyr::mutate(dplyr::across(-idmesh, ~ f_mode_norm(.x, norm_mode)))
  
  # ---- X7 : choix de la base de sensibilité (attention : mapping inversé
  #           par rapport à X1 -- préservé tel quel depuis run_one_simul) --
  S_brut_base   <- switch(as.character(X7),
                          "2" = engine$S_brut$precaution,
                          "3" = engine$S_brut$mediane,
                          "4" = engine$S_brut$binaire)
  hab_rows_base <- switch(as.character(X7),
                          "2" = engine$hab_rows$precaution,
                          "3" = engine$hab_rows$mediane,
                          "4" = engine$hab_rows$binaire)
  
  # ---- X8 : perturbation des surfaces habitats ----------------------------
  if (X8 == 1L) {
    w_surf <- vapply(seq_len(nrow(hab_rows_base)), function(ii) {
      maxval_ii <- hab_rows_base$E_surfmer[ii]
      if (!is.finite(maxval_ii) || maxval_ii <= 0) return(hab_rows_base$E_surfhab[ii])
      f_sensib(hab_rows_base$E_surfhab[ii], engine$hab_iq_vec[ii], maxval_ii)
    }, numeric(1))
    S_brut_base <- f_agreg_surf_hab(hab_rows_base, P_code_sel, col_idz,
                                    ctx$seuil_nodata, w_surf = w_surf)
  }
  
  # ---- X3 : dispersion IC --------------------------------------------------
  if (X3 == 1L) {
    S_brut_sim <- S_brut_base
    for (p in P_code_sel) {
      s_col <- S_brut_base[[p]]
      s_col[!is.finite(s_col)] <- 0
      S_brut_sim[[p]] <- vapply(seq_len(nz), function(ii)
        f_sensib(s_col[ii], engine$disp_mat[ii, p], 5), numeric(1))
    }
    S_base_sim <- f_norm(S_brut_sim)
  } else {
    S_base_sim <- f_norm(S_brut_base)
  }
  
  Mat_Sensib_sim <- data.frame(idmesh = col_idz)
  for (p in P_code_sel) {
    s0 <- if (p %in% names(S_base_sim)) S_base_sim[[p]] else rep(0, nz)
    s0[!is.finite(s0)] <- 0
    Mat_Sensib_sim[[p]] <- s0
  }
  
  # ---- Scores feuilles ------------------------------------------------------
  score_tbl <- data.frame(idmesh = col_idz, stringsAsFactors = FALSE)
  for (p in P_code_sel) {
    p_vec <- Mat_Pj_norm_z[[p]]; p_vec[!is.finite(p_vec)] <- 0
    s_vec <- Mat_Sensib_sim[[p]]; s_vec[!is.finite(s_vec)] <- NA_real_
    score_tbl[[p]] <- p_vec * s_vec
  }
  
  # ---- Récursion hiérarchique (X9 x X10) ------------------------------------
  noeuds_composes <- engine$hierarchie$noeuds_composes
  tp_dem          <- engine$hierarchie$tp_dem
  racines         <- engine$hierarchie$racines
  
  if (nrow(noeuds_composes) > 0) {
    for (i in seq_len(nrow(noeuds_composes))) {
      code_parent <- as.character(noeuds_composes$Code_P[i])
      enfants <- as.character(
        tp_dem$Code_P[!is.na(tp_dem$parent) & as.character(tp_dem$parent) == code_parent]
      )
      enfants_ok <- intersect(enfants, names(score_tbl))
      if (length(enfants_ok) == 0) {
        score_tbl[[code_parent]] <- NA_real_
        next
      }
      mat_enf <- as.matrix(score_tbl[, enfants_ok, drop = FALSE])
      somme   <- f_mode_agreg_press(mat_enf, agreg_mode)
      score_tbl[[code_parent]] <- f_mode_norm(somme, norm_mode)
    }
  }
  
  # ---- REFC (X9 x X10) --------------------------------------------------------
  racines_ok <- intersect(racines, names(score_tbl))
  if (length(racines_ok) == 0) {
    warning("calc_refc: aucune racine disponible, fallback somme des feuilles")
    mat_fallback <- as.matrix(score_tbl[, P_code_sel, drop = FALSE])
    return(f_mode_norm(f_mode_agreg_press(mat_fallback, agreg_mode), norm_mode))
  }
  
  mat_rac <- as.matrix(score_tbl[, racines_ok, drop = FALSE])
  f_mode_norm(f_mode_agreg_press(mat_rac, agreg_mode), norm_mode)
}

# -----------------------------------------------------------------------
# draw_params() — résolution des modalités "aléa" en un jeu de paramètres
# concret, consommable par calc_refc().
# -----------------------------------------------------------------------
# Seuls X1, X2, X7, X9, X10 supportent une modalité "aléa" (valeur 1) qui
# déclenche un tirage uniforme parmi leurs modalités concrètes. X3, X4, X5,
# X6, X8 sont de simples interrupteurs 0/1 (pas de tirage de modalité :
# leur caractère stochastique vient de calc_refc() lui-même via f_sensib/
# f_AP quand ils valent 1, pas d'un choix de modalité en amont).
#
# factors_cfg : config$analyses$monte_carlo$factors (valeurs brutes,
#               éventuellement 1 = aléa pour X1/X2/X7/X9/X10)
#
# Retour : liste de paramètres résolus, prête pour calc_refc(params, ...).
draw_params <- function(factors_cfg) {
  
  stopifnot("draw_params: factors_cfg doit être une liste" = is.list(factors_cfg))
  
  # ---- Résolution d'un facteur à modalité "aléa" (valeur 1) ---------------
  resolve_modal <- function(val, choices, label, default) {
    val <- suppressWarnings(as.integer(val))
    if (is.na(val)) {
      warning(sprintf("draw_params: %s manquant/invalide, défaut = %d.", label, default))
      return(default)
    }
    if (val == 1L) return(sample(choices, 1))
    if (!val %in% choices) {
      warning(sprintf(
        "draw_params: %s=%d hors domaine {%s}, défaut = %d.",
        label, val, paste(choices, collapse = ","), default
      ))
      return(default)
    }
    val
  }
  
  # ---- Validation d'un facteur binaire simple (0/1, pas de tirage) -------
  validate_binary <- function(val, label, default = 0L) {
    val <- suppressWarnings(as.integer(val %||% default))
    if (is.na(val) || !val %in% 0:1) {
      warning(sprintf("draw_params: %s invalide, défaut = %d.", label, default))
      return(default)
    }
    val
  }
  
  list(
    X1_mat_AP       = resolve_modal(factors_cfg$X1_mat_AP,      2:4, "X1_mat_AP",      REF_VALUES$X1_mat_AP),
    X2_relation_AP  = resolve_modal(factors_cfg$X2_relation_AP, 2:5, "X2_relation_AP", REF_VALUES$X2_relation_AP),
    X3_var_sensi    = validate_binary(factors_cfg$X3_var_sensi,  "X3_var_sensi",  REF_VALUES$X3_var_sensi),
    X4_var_ccr_AP   = validate_binary(factors_cfg$X4_var_ccr_AP, "X4_var_ccr_AP", REF_VALUES$X4_var_ccr_AP),
    X5_var_act      = validate_binary(factors_cfg$X5_var_act,    "X5_var_act",    REF_VALUES$X5_var_act),
    X6_typo_act     = resolve_modal(factors_cfg$X6_typo_act, 2:4, "X6_typo_act", REF_VALUES$X6_typo_act),
    X7_mat_sensi    = resolve_modal(factors_cfg$X7_mat_sensi, 2:4, "X7_mat_sensi", REF_VALUES$X7_mat_sensi),
    X8_var_hab      = validate_binary(factors_cfg$X8_var_hab,    "X8_var_hab",    REF_VALUES$X8_var_hab),
    X9_norm_form    = resolve_modal(factors_cfg$X9_norm_form,    2:4, "X9_norm_form",    REF_VALUES$X9_norm_form),
    X10_agreg_press = resolve_modal(factors_cfg$X10_agreg_press, 2:4, "X10_agreg_press", REF_VALUES$X10_agreg_press)
  )
}
