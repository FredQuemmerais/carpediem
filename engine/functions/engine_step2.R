# engine/functions/engine_step2.R
# Moteur calcul refc mutualisé pour step2 (Monte Carlo), step2b (robustesse déterministe) et step2d (sensibilité Shapley)

engine_step2 <- function(ctx) {
  
  # ---- Guards d'entrée ----------------------------------------------------
  stopifnot(
    "engine_step2: ctx doit être une liste"           = is.list(ctx),
    "engine_step2: ctx$grid_z manquant"               = !is.null(ctx$grid_z),
    "engine_step2: ctx$col_idz manquant"              = !is.null(ctx$col_idz),
    "engine_step2: ctx$nz manquant"                   = !is.null(ctx$nz),
    "engine_step2: ctx$P_code_sel manquant"           = !is.null(ctx$P_code_sel) && length(ctx$P_code_sel) > 0,
    "engine_step2: ctx$typo_press manquant"           = !is.null(ctx$typo_press) && nrow(ctx$typo_press) > 0,
    "engine_step2: ctx$HabBenth_z manquant"           = !is.null(ctx$HabBenth_z),
    "engine_step2: ctx$Mat_Sensib_Pj manquant"        = !is.null(ctx$Mat_Sensib_Pj),
    "engine_step2: ctx$seuil_nodata manquant"         = !is.null(ctx$seuil_nodata),
    "engine_step2: ctx$sensi_path manquant"           = !is.null(ctx$sensi_path) && file.exists(ctx$sensi_path),
    "engine_step2: ctx$sensi_sheet manquant"          = !is.null(ctx$sensi_sheet) && nzchar(ctx$sensi_sheet),
    "engine_step2: ctx$ods_ic_hab manquant"           = !is.null(ctx$ods_ic_hab),
    "engine_step2: ctx$A_zi_lognorm manquant"         = !is.null(ctx$A_zi_lognorm),
    "engine_step2: ctx$MatAP_mediane manquant"        = !is.null(ctx$MatAP_mediane),
    "engine_step2: ctx$MatAP_precaution manquant"     = !is.null(ctx$MatAP_precaution),
    "engine_step2: ctx$MatAP_binaire manquant"        = !is.null(ctx$MatAP_binaire),
    "engine_step2: fonctions f_agreg_surf_hab/f_norm/f_lognorm requises" =
      exists("f_agreg_surf_hab", mode = "function") &&
      exists("f_norm",           mode = "function") &&
      exists("f_lognorm",        mode = "function"),
    "engine_step2: FACTOR_REGISTRY introuvable (charger f_aux_monte_carlo.R)" =
      exists("FACTOR_REGISTRY")
  )
  
  col_idz    <- ctx$col_idz
  nz         <- ctx$nz
  P_code_sel <- ctx$P_code_sel
  
  message("engine_step2: construction du bundle de préparation...")
  
  # -------------------------------------------------------------------------
  # X1 / X4 — Matrices A-P (médiane / précaution / binaire), lien + IC
  # -------------------------------------------------------------------------
  prep_matap <- function(mat_raw, label) {
    m        <- mat_raw
    names(m) <- tolower(trimws(names(m)))
    if (!"type"   %in% names(m)) names(m)[1] <- "type"
    if (!"code_a" %in% names(m))
      stop(sprintf("engine_step2: colonne code_a introuvable dans matrice A-P (%s)", label))
    m$type   <- tolower(trimws(as.character(m$type)))
    m$code_a <- tolower(trimws(as.character(m$code_a)))
    
    df_lien <- m[m$type == "lien", , drop = FALSE]
    df_ic   <- m[m$type == "ic",   , drop = FALSE]
    if (nrow(df_lien) == 0)
      stop(sprintf("engine_step2: aucune ligne 'lien' dans matrice A-P (%s)", label))
    if (nrow(df_ic) == 0)
      stop(sprintf("engine_step2: aucune ligne 'ic' dans matrice A-P (%s)", label))
    
    pr_cols <- names(m)[grep("^pr_", names(m))]
    if (length(pr_cols) == 0)
      stop(sprintf("engine_step2: aucune colonne pression (%s)", label))
    
    mat_l <- as.matrix(df_lien[, pr_cols, drop = FALSE])
    storage.mode(mat_l) <- "numeric"
    rownames(mat_l) <- df_lien$code_a
    mat_l[!is.finite(mat_l)] <- NA_real_
    
    mat_ic <- as.matrix(df_ic[, pr_cols, drop = FALSE])
    storage.mode(mat_ic) <- "numeric"
    rownames(mat_ic) <- df_ic$code_a
    mat_ic[!is.finite(mat_ic)] <- 0
    mat_ic[mat_ic < 0] <- 0
    mat_ic[mat_ic > 1] <- 1
    
    list(lien = mat_l, ic = mat_ic)
  }
  
  mat_ap <- list(
    mediane    = prep_matap(ctx$MatAP_mediane,    "mediane"),
    precaution = prep_matap(ctx$MatAP_precaution, "precaution"),
    binaire    = prep_matap(ctx$MatAP_binaire,    "binaire")
  )
  
  message("engine_step2: X1/X4 -- 3 matrices A-P préparées (médiane/précaution/binaire)")
  
  # -------------------------------------------------------------------------
  # X7 / X8 — Lignes habitat brutes (médiane + précaution) et S_brut agrégé
  # -------------------------------------------------------------------------
  hab_rows_ref <- sf::st_drop_geometry(ctx$HabBenth_z) |>
    dplyr::select(idmesh, E_surfhab, E_surfmer, dplyr::any_of(P_code_sel)) |>
    dplyr::mutate(idmesh = as.character(idmesh))
  
  load_hab_rows_autre_sheet <- function(sheet_cible) {
    ods_alt <- tryCatch(
      readODS::read_ods(ctx$sensi_path, sheet = sheet_cible),
      error = function(e) {
        warning(sprintf(
          "engine_step2: lecture onglet '%s' impossible (%s). Fallback sheet référence.",
          sheet_cible, e$message
        ))
        NULL
      }
    )
    if (is.null(ods_alt)) return(hab_rows_ref)
    
    names(ods_alt) <- tolower(trimws(names(ods_alt)))
    if (!"type" %in% names(ods_alt)) names(ods_alt)[1] <- "type"
    ods_alt$type <- tolower(trimws(as.character(ods_alt$type)))
    pcols_alt    <- names(ods_alt)[grep("^pr_", names(ods_alt))]
    ods_sens_alt <- ods_alt[ods_alt$type == "sensibilite",
                            c("code_h", pcols_alt), drop = FALSE]
    
    sf::st_drop_geometry(ctx$HabBenth_z) |>
      dplyr::select(idmesh, E_surfhab, E_surfmer, E_habcode) |>
      dplyr::mutate(idmesh = as.character(idmesh)) |>
      dplyr::left_join(ods_sens_alt, by = c("E_habcode" = "code_h")) |>
      dplyr::select(-E_habcode)
  }
  
  if (ctx$sensi_sheet == "precaution") {
    hab_rows_pre <- hab_rows_ref
    hab_rows_med <- load_hab_rows_autre_sheet("median")
  } else {
    hab_rows_med <- hab_rows_ref
    hab_rows_pre <- load_hab_rows_autre_sheet("precaution")
  }
  hab_rows_bin <- load_hab_rows_autre_sheet("binaire")
  
  zero_fill_na <- function(df) {
    cols <- setdiff(names(df), "idmesh")
    df[cols] <- lapply(df[cols], function(x) {
      x <- suppressWarnings(as.numeric(x))
      x[!is.finite(x)] <- 0
      x
    })
    df
  }
  
  hab_rows <- list(mediane = hab_rows_med, precaution = hab_rows_pre, binaire = hab_rows_bin)
  
  S_brut <- list(
    mediane    = zero_fill_na(f_agreg_surf_hab(hab_rows_med, P_code_sel, col_idz, ctx$seuil_nodata)),
    precaution = zero_fill_na(f_agreg_surf_hab(hab_rows_pre, P_code_sel, col_idz, ctx$seuil_nodata)),
    binaire    = zero_fill_na(f_agreg_surf_hab(hab_rows_bin, P_code_sel, col_idz, ctx$seuil_nodata))
  )
  
  hab_iq_vec <- suppressWarnings(as.numeric(sf::st_drop_geometry(ctx$HabBenth_z)$hab_iq))
  
  message("engine_step2: X7/X8 -- hab_rows + S_brut préparés (médiane/précaution)")
  
  # -------------------------------------------------------------------------
  # X3 — Dispersion IC habitat-pression par maille [0,5]
  # -------------------------------------------------------------------------
  ic_cols <- intersect(P_code_sel, names(ctx$ods_ic_hab))
  
  if (length(ic_cols) < length(P_code_sel)) {
    warning(sprintf(
      "engine_step2 X3: %d pression(s) absente(s) de ods_ic_hab, dispersion=0 : %s",
      length(P_code_sel) - length(ic_cols),
      paste(setdiff(P_code_sel, ic_cols), collapse = ", ")
    ))
  }
  
  hab_ic <- sf::st_drop_geometry(ctx$HabBenth_z) |>
    dplyr::select(idmesh, E_surfhab, E_habcode) |>
    dplyr::left_join(
      ctx$ods_ic_hab |>
        dplyr::select(code_h, dplyr::all_of(ic_cols)) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(ic_cols),
                                    ~ suppressWarnings(as.numeric(.x)))),
      by = c("E_habcode" = "code_h")
    ) |>
    dplyr::mutate(
      E_surfhab = suppressWarnings(as.numeric(.data$E_surfhab)),
      idmesh    = as.character(.data$idmesh)
    )
  
  disp_agg <- hab_ic |>
    dplyr::group_by(idmesh) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(ic_cols),
        ~ {
          s    <- .data$E_surfhab
          x    <- .x
          keep <- is.finite(s) & s > 0 & is.finite(x)
          if (!any(keep)) return(0)
          sum(s[keep] * x[keep]) / sum(s[keep])
        }
      ),
      .groups = "drop"
    )
  
  disp_agg <- data.frame(idmesh = as.character(col_idz)) |>
    dplyr::left_join(disp_agg, by = "idmesh") |>
    dplyr::mutate(dplyr::across(dplyr::all_of(ic_cols),
                                ~ dplyr::if_else(is.na(.x), 0, .x)))
  for (p in setdiff(P_code_sel, ic_cols)) disp_agg[[p]] <- 0
  
  disp_mat <- as.matrix(disp_agg[, P_code_sel, drop = FALSE])
  rownames(disp_mat) <- disp_agg$idmesh
  
  message("engine_step2: X3 -- matrice de dispersion IC construite")
  
  # -------------------------------------------------------------------------
  # X5 — IQ activités (optionnel : dégradation propre si absent de ctx)
  # -------------------------------------------------------------------------
  has_X5 <- !is.null(ctx$A_iq_aligned) && !is.null(ctx$iq_col_by_code)
  
  if (!has_X5) {
    warning("engine_step2: ctx$A_iq_aligned ou ctx$iq_col_by_code manquant -- X5_var_act indisponible.")
    A_iq_aligned   <- NULL
    iq_col_by_code <- NULL
    A_miss_aligned <- NULL
  } else {
    A_iq_aligned   <- ctx$A_iq_aligned
    iq_col_by_code <- ctx$iq_col_by_code
    A_miss_aligned <- ctx$A_miss_aligned
  }
  
  message(sprintf(
    "engine_step2: X5 -- %s",
    if (has_X5) "IQ activités disponible" else "IQ activités indisponible (X5 désactivé)"
  ))
  
  # -------------------------------------------------------------------------
  # Structure hiérarchique des pressions (feuilles / nœuds composés / racines)
  # Utilisée par la récursion X9/X10 dans calc_refc()
  # -------------------------------------------------------------------------
  tp        <- ctx$typo_press
  tp$Code_P <- tolower(as.character(tp$Code_P))
  tp$parent <- tolower(as.character(tp$parent))
  
  tp_dem <- tp[!is.na(tp$demonstrateur) & as.integer(tp$demonstrateur) == 1L, ]
  
  noeuds_composes <- tp_dem[
    !is.na(tp_dem$mode_calcul) & as.integer(tp_dem$mode_calcul) == 3L,
  ]
  
  if (nrow(noeuds_composes) > 0) {
    get_depth <- function(code)
      lengths(regmatches(code, gregexpr("_", code))) + 1L
    noeuds_composes <- noeuds_composes[
      order(get_depth(as.character(noeuds_composes$Code_P)), decreasing = TRUE),
    ]
  }
  
  racines <- as.character(
    tp_dem$Code_P[
      is.na(tp_dem$parent) | !nzchar(trimws(as.character(tp_dem$parent)))
    ]
  )
  stopifnot("engine_step2: aucune racine dans typo_press" = length(racines) > 0)
  
  message(sprintf(
    "engine_step2: hiérarchie -- %d feuille(s), %d nœud(s) composé(s), %d racine(s)",
    length(P_code_sel), nrow(noeuds_composes), length(racines)
  ))
  
  # -------------------------------------------------------------------------
  # X6 — Granularité de la typologie activité (natif / N2 / N1)
  # -------------------------------------------------------------------------
  build_level <- function(level) {
    agg <- aggregate_activities_at_level(ctx$A_zi, level)
    agg |>
      dplyr::mutate(dplyr::across(-idmesh, ~ f_lognorm(.x))) |>
      dplyr::rename_with(~ paste0(.x, "_norm"), -idmesh)
  }
  
  A_lognorm_by_level <- list(
    natif = ctx$A_zi_lognorm,
    N2    = build_level(2),
    N1    = build_level(1)
  )
  
  iq_by_level <- build_iq_by_level(ctx$A_zi, iq_col_by_code, A_iq_aligned, A_miss_aligned, has_X5)
  
  message(sprintf(
    "engine_step2: X6 -- typologie activité agrégée (natif=%d, N2=%d, N1=%d colonnes)",
    ncol(A_lognorm_by_level$natif) - 1,
    ncol(A_lognorm_by_level$N2) - 1,
    ncol(A_lognorm_by_level$N1) - 1
  ))
  
  # -------------------------------------------------------------------------
  # Assemblage
  # -------------------------------------------------------------------------
  out <- list(
    mat_ap         = mat_ap,
    S_brut         = S_brut,
    hab_rows       = hab_rows,
    hab_iq_vec     = hab_iq_vec,
    disp_mat       = disp_mat,
    A_iq_aligned   = A_iq_aligned,
    iq_col_by_code = iq_col_by_code,
    A_miss_aligned = A_miss_aligned,
    has_X5         = has_X5,
    A_zi           = ctx$A_zi,
    A_lognorm_by_level = A_lognorm_by_level, 
    iq_by_level = iq_by_level,
    hierarchie     = list(
      tp_dem          = tp_dem,
      noeuds_composes = noeuds_composes,
      racines         = racines
    )
  )
  
  message("engine_step2: bundle prêt.\n")
  
  out
}
