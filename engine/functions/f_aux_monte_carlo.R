# engine/functions/f_aux_monte_carlo.R
# version du 23.07.2026

# ---- Registre de classification des facteurs d'incertitude ---------------
# "deterministe" : modalité fixée -> step2b, calc_refc() sans répétition
# "stochastique" : perturbation aléatoire par maille à chaque appel -> step2c (dispersion via step2), calc_refc() à répéter (nrep) si utilisé dans step2d

FACTOR_REGISTRY <- c(
  X1_mat_AP       = "deterministe",
  X2_relation_AP  = "deterministe",
  X3_var_sensi    = "stochastique",
  X4_var_ccr_AP   = "stochastique",
  X5_var_act      = "stochastique",
  X6_typo_act     = "deterministe",
  X7_mat_sensi    = "deterministe",
  X8_var_hab      = "stochastique",
  X9_norm_form    = "deterministe",
  X10_agreg_press = "deterministe"
)

REF_VALUES <- list(
  X1_mat_AP       = 2L,
  X2_relation_AP  = 2L,
  X3_var_sensi    = 0L,
  X4_var_ccr_AP   = 0L,
  X5_var_act      = 0L,
  X6_typo_act     = 2L,  # natif
  X7_mat_sensi    = 2L,
  X8_var_hab      = 0L,
  X9_norm_form    = 2L,
  X10_agreg_press = 2L
)

# X2 : Relations activité-pression -------
optimiste  <- function(x) (((2 / (2 - x))^3) - 1) / ((2^3) - 1)
pessimiste <- function(x) (2^3 - (2 / (1 + x))^3) / ((2^3) - 1)
logistique <- function(x) 1 / (1 + exp(-8 * (x - 0.5)))

# X3 : Fonction sensibilité avec IC ------
f_sensib <- function(val, disp, maxval) {
  
  # garde d'entrée
  if (is.na(val) || is.na(disp) || is.na(maxval) || maxval <= 0) return(NA_real_)
  
  # ND ou confiance nulle: tirage uniforme
  if (val == 99 || disp <= 0) {
    return(runif(1, 0, maxval))
  }
  
  # normalisation en [0,1]
  mu <- val / maxval
  mu <- max(0, min(1, mu))
  
  # bornes: pas de tirage dégénéré
  if (mu <= 0) return(0)
  if (mu >= 1) return(maxval)
  
  # mapping disp -> concentration kappa (= a+b)
  # disp élevé => kappa élevé => faible variance
  # bornes pour stabilité numérique
  kappa_min <- 2     # très incertain
  kappa_max <- 200   # très certain (suffisant sans casser qbeta)
  kappa <- kappa_min + (kappa_max - kappa_min) * (disp / 5)
  
  a <- mu * kappa
  b <- (1 - mu) * kappa
  
  # sécurité numérique
  eps <- 1e-6
  a <- max(a, eps); b <- max(b, eps)
  
  x <- tryCatch(
    stats::rbeta(1, shape1 = a, shape2 = b),
    warning = function(w) NA_real_,
    error   = function(e) NA_real_
  )
  
  if (!is.finite(x)) x <- stats::runif(1, 0, 1)
  
  out <- maxval * x
  out
}

# X3 : Fonction sensibilité sans IC -----
f_sensib2 <- function(val) {
  if (val == 99) {
    return(runif(1, 0, 5))
  }
  val
}

# X4 : Fonction CCR avec IC (ccr = ap dans [0,1], ic dans [0,1]) -----
f_AP <- function(ap, ic) {
  ap <- max(0, min(1, suppressWarnings(as.numeric(ap))))
  if (!is.finite(ap)) return(0)
  ic <- max(0, min(1, suppressWarnings(as.numeric(ic))))
  if (!is.finite(ic)) ic <- 1
  
  # IC=0 explicitement -> absence d'information -> tirage uniforme [0,1]
  if (ic == 0) return(runif(1, 0, 1))
  
  # Reconstruction du CV depuis IC = 1/(1+2CV)
  cv <- (1 / ic - 1) / 2
  
  # Intervalle symétrique autour du CCR médian, clampé sur [0,1]
  lower <- max(0, ap * (1 - cv))
  upper <- min(1, ap * (1 + cv))
  
  # Si l'intervalle est dégénéré (IC=1 -> cv=0 -> lower=upper=ap)
  if (lower >= upper) return(ap)
  
  runif(1, lower, upper)
}

# X9 : Normalisation linéaire (min-max) -----
f_linnorm <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- 0
  if (all(x == 0)) return(rep(0, length(x)))
  r <- range(x, na.rm = TRUE)
  if (!is.finite(diff(r)) || diff(r) == 0) return(rep(1, length(x)))
  out <- (x - r[1]) / diff(r)
  out[!is.finite(out)] <- 0
  out
}

# X9 : Normalisation sigmoïde -----------
# min-max puis courbe logistique re-normalisée pour retomber exactement sur [0,1]
f_signorm <- function(x, k = 8) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- 0
  if (all(x == 0)) return(rep(0, length(x)))
  r <- range(x, na.rm = TRUE)
  if (!is.finite(diff(r)) || diff(r) == 0) return(rep(1, length(x)))
  x01 <- (x - r[1]) / diff(r)
  logist <- function(v) 1 / (1 + exp(-k * (v - 0.5)))
  s0 <- logist(0); s1 <- logist(1)
  out <- (logist(x01) - s0) / (s1 - s0)
  out[!is.finite(out)] <- 0
  out
}

# X9 : Dispatcher vers f_lognorm / f_linnorm / f_signorm ------
f_mode_norm <- function(x, mode = c("log", "linear", "sigmoid")) {
  mode <- match.arg(mode)
  switch(mode,
         log     = f_lognorm(x),
         linear  = f_linnorm(x),
         sigmoid = f_signorm(x)
  )
}

# X8 : Fonction pondération surface habitats --------
f_agreg_surf_hab <- function(hab_rows, P_code_sel, col_idz, seuil_nodata,
                             w_surf = NULL) {
  
  if (!is.null(w_surf)) {
    stopifnot(
      "f_agreg_surf_hab: w_surf doit avoir la même longueur que hab_rows" =
        length(w_surf) == nrow(hab_rows)
    )
    hab_rows$E_surfhab <- w_surf
  }
  
  # pressions absentes de hab_rows (ex: sheet alternative incomplète) -> 99
  manquantes <- setdiff(P_code_sel, names(hab_rows))
  if (length(manquantes) > 0) hab_rows[manquantes] <- 99
  
  df <- hab_rows |>
    dplyr::mutate(
      E_surfhab = as.numeric(.data$E_surfhab),
      E_surfmer = as.numeric(.data$E_surfmer),
      w_hab     = .data$E_surfhab
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
  
  for (p in P_code_sel) {
    nd_col   <- paste0(p, "_nodata")
    nd_val   <- df[[nd_col]]
    sm       <- df$E_surfmer
    flag     <- !is.na(nd_val) & !is.na(sm) & sm > 0 & (nd_val > seuil_nodata * sm)
    val_norm <- ifelse(!is.na(sm) & sm > 0, df[[p]] / sm, NA_real_)
    df[[p]]  <- ifelse(flag, NA_real_, val_norm)
  }
  
  df <- df |> dplyr::select(-dplyr::ends_with("_nodata"))
  
  data.frame(idmesh = as.character(col_idz), stringsAsFactors = FALSE) |>
    dplyr::left_join(dplyr::mutate(df, idmesh = as.character(.data$idmesh)),
                     by = "idmesh")
}
# X10 : Mode d'agrégation des pressions (additif, synergique ou antagoniste) -----
f_mode_agreg_press <- function(mat, mode = c("additif", "synergique", "antagoniste")) {
  mode <- match.arg(mode)
  mat  <- as.matrix(mat); storage.mode(mat) <- "double"
  mat[!is.finite(mat)] <- NA_real_
  
  if (ncol(mat) <= 1 || mode == "additif") {
    out <- rowSums(mat, na.rm = TRUE)
    out[rowSums(!is.na(mat)) == 0] <- NA_real_
    return(out)
  }
  
  n_valid <- rowSums(!is.na(mat))
  ord_mat <- t(apply(mat, 1, sort, decreasing = TRUE, na.last = TRUE))
  
  vapply(seq_len(nrow(mat)), function(i) {
    n_i <- n_valid[i]
    if (n_i == 0) return(NA_real_)
    vals <- ord_mat[i, seq_len(n_i)]
    idx  <- seq_len(n_i)
    w <- if (mode == "antagoniste") (n_i - idx + 1) / n_i else (n_i + idx - 1) / n_i
    sum(vals * w)
  }, numeric(1))
}

# X6 : Dérivation du code parent par troncature -----
# "A_1_1_1" avec level=2 -> "A_1_1" ; un code déjà à ce niveau ou plus
# grossier (depth <= level) reste inchangé (on n'agrège jamais "vers le bas").
act_code_ancestor <- function(code, level) {
  parts <- strsplit(code, "_", fixed = TRUE)[[1]]
  segs  <- parts[-1]
  if (length(segs) <= level) return(code)
  paste(c(parts[1], segs[seq_len(level)]), collapse = "_")
}

# X6 : Agrégation des intensités brutes par code parent -----
# Retourne un data.frame idmesh + une colonne par code parent (somme des
# enfants). L'attribut "child_to_parent" documente le regroupement appliqué.
aggregate_activities_at_level <- function(A_zi, level) {
  act_cols <- setdiff(names(A_zi), "idmesh")
  parents  <- vapply(act_cols, act_code_ancestor, character(1), level = level)
  
  out <- data.frame(idmesh = A_zi$idmesh, stringsAsFactors = FALSE)
  for (p in unique(parents)) {
    children <- act_cols[parents == p]
    out[[p]] <- rowSums(as.matrix(A_zi[, children, drop = FALSE]), na.rm = TRUE)
  }
  attr(out, "child_to_parent") <- setNames(parents, act_cols)
  out
}

# X6 x X5 : Agrégation de l'IQ par code parent, pondérée par l'intensité -----------
# brute de chaque enfant à chaque maille. Un parent est "manquant" à une
# maille si TOUS ses enfants contributeurs y sont marqués manquants.
# Retour : list(iq_by_code = list nommée de vecteurs [0,5],
#               miss_by_code = list nommée de vecteurs logiques)
aggregate_iq_at_level <- function(A_zi, iq_col_by_code, A_iq_aligned,
                                  A_miss_aligned, level) {
  act_cols <- setdiff(names(A_zi), "idmesh")
  parents  <- vapply(act_cols, act_code_ancestor, character(1), level = level)
  nz       <- nrow(A_zi)
  
  iq_by_code   <- list()
  miss_by_code <- list()
  
  for (p in unique(parents)) {
    children <- act_cols[parents == p]
    
    w_mat    <- as.matrix(A_zi[, children, drop = FALSE])
    iq_mat   <- matrix(5, nrow = nz, ncol = length(children))
    miss_mat <- matrix(FALSE, nrow = nz, ncol = length(children))
    
    for (j in seq_along(children)) {
      code_a <- children[j]
      iq_col <- iq_col_by_code[[code_a]]
      if (!is.null(iq_col) && iq_col %in% names(A_iq_aligned)) {
        v <- suppressWarnings(as.numeric(A_iq_aligned[[iq_col]]))
        v <- pmin(5, pmax(0, v)); v[!is.finite(v)] <- 5
        iq_mat[, j] <- v
      }
      if (!is.null(A_miss_aligned) && code_a %in% names(A_miss_aligned)) {
        m <- as.logical(A_miss_aligned[[code_a]]); m[is.na(m)] <- FALSE
        miss_mat[, j] <- m
      }
    }
    
    w_sum  <- rowSums(w_mat, na.rm = TRUE)
    iq_w   <- rowSums(w_mat * iq_mat, na.rm = TRUE)
    iq_avg <- ifelse(w_sum > 0, iq_w / w_sum, 5)
    all_miss <- if (ncol(miss_mat) > 0) apply(miss_mat, 1, all) else rep(FALSE, nz)
    
    iq_by_code[[p]]   <- iq_avg
    miss_by_code[[p]] <- all_miss
  }
  
  list(iq_by_code = iq_by_code, miss_by_code = miss_by_code)
}

# X5 x X6 : construit iq_by_level (natif/N2/N1) ------------
# à partir de A_iq_aligned et A_miss_aligned. 
# Réutilisée par engine_step2() (construction initiale) et
# par tout script générant des scénarios de qualité de données fictifs
# (perturbation de A_iq_aligned/A_miss_aligned), qui doit alors recalculer
# les IQ agrégées en conséquence pour que X5 reste effectif combiné à X6.
build_iq_by_level <- function(A_zi, iq_col_by_code, A_iq_aligned, A_miss_aligned, has_X5) {
  nz <- nrow(A_zi)
  empty_iq <- list(iq_by_code = list(), miss_by_code = list())
  if (!has_X5) return(list(natif = empty_iq, N2 = empty_iq, N1 = empty_iq))
  
  act_cols    <- setdiff(names(A_zi), "idmesh")
  iq_native   <- list()
  miss_native <- list()
  for (code_a in act_cols) {
    iq_col <- iq_col_by_code[[code_a]]
    v <- rep(5, nz)
    if (!is.null(iq_col) && iq_col %in% names(A_iq_aligned)) {
      vv <- suppressWarnings(as.numeric(A_iq_aligned[[iq_col]]))
      vv <- pmin(5, pmax(0, vv)); vv[!is.finite(vv)] <- 5
      v <- vv
    }
    m <- rep(FALSE, nz)
    if (!is.null(A_miss_aligned) && code_a %in% names(A_miss_aligned)) {
      mm <- as.logical(A_miss_aligned[[code_a]]); mm[is.na(mm)] <- FALSE
      m <- mm
    }
    iq_native[[code_a]]   <- v
    miss_native[[code_a]] <- m
  }
  
  list(
    natif = list(iq_by_code = iq_native, miss_by_code = miss_native),
    N2    = aggregate_iq_at_level(A_zi, iq_col_by_code, A_iq_aligned, A_miss_aligned, 2),
    N1    = aggregate_iq_at_level(A_zi, iq_col_by_code, A_iq_aligned, A_miss_aligned, 1)
  )
}

# ---- Récupérer les indices de confiances ----
native_iq_by_code <- function(A_zi, iq_col_by_code, A_iq_aligned, A_miss_aligned) {
  act_cols <- setdiff(names(A_zi), "idmesh")
  nz <- nrow(A_zi)
  iq_by_code <- list(); miss_by_code <- list()
  for (code_a in act_cols) {
    iq_col <- iq_col_by_code[[code_a]]
    v <- rep(5, nz)
    if (!is.null(iq_col) && iq_col %in% names(A_iq_aligned)) {
      vv <- suppressWarnings(as.numeric(A_iq_aligned[[iq_col]]))
      vv <- pmin(5, pmax(0, vv)); vv[!is.finite(vv)] <- 5
      v <- vv
    }
    m <- rep(FALSE, nz)
    if (!is.null(A_miss_aligned) && code_a %in% names(A_miss_aligned)) {
      mm <- as.logical(A_miss_aligned[[code_a]]); mm[is.na(mm)] <- FALSE
      m <- mm
    }
    iq_by_code[[code_a]] <- v
    miss_by_code[[code_a]] <- m
  }
  list(iq_by_code = iq_by_code, miss_by_code = miss_by_code)
}

# ---- Calculer statistiques simulations -----
stat_tab <- function(tab) {
  nco <- ncol(tab)
  nli <- nrow(tab)
  
  moy <- numeric(nli)
  std <- numeric(nli)
  cv <- numeric(nli)
  
  for (i in seq_len(nli)) {
    vals <- as.numeric(tab[i, 2:nco])
    moy[i] <- mean(vals, na.rm = TRUE)
    std[i] <- sd(vals, na.rm = TRUE)
    
    if (moy[i] > 0) {
      cv[i] <- std[i] / moy[i]
    } else {
      cv[i] <- 0
    }
  }
  
  data.frame(
    idmesh = tab[[1]],
    REFC_moy = moy,
    sd_REFC = std,
    cv_REFC = cv
  )
}

# ---- Identifier top/bottom simulations -----
simul_calc <- function(simul_tab, val) {
  n <- ncol(simul_tab)
  nval <- length(val)
  out <- list()
  
  for (i in seq_along(val)) {
    out[[i]] <- simul_tab
    
    for (j in 2:n) {
      out[[i]][[j]] <- 0
      
      quants <- quantile(simul_tab[[j]], probs = seq(0, 1, by = val[i] / 100), na.rm = TRUE)
      top_thresh <- quants[length(quants) - 1]
      bottom_thresh <- quants[2]
      
      out[[i]][[j]][simul_tab[[j]] >= top_thresh] <- 1
      out[[i]][[j]][simul_tab[[j]] <= bottom_thresh] <- -1
    }
  }
  
  out
}

# ---- Exporter table vers PostgreSQL -----
export_table <- function(in_con, in_scheme, in_grid, in_table, in_nametab,
                         id_grid = "idmesh",
                         id_table = "idmesh") {
  
  # ---- Garde-fous environnement ----------------------------------------
  stopifnot(
    "export_table: config introuvable dans .GlobalEnv" =
      exists("config", envir = .GlobalEnv),
    "export_table: DBConnectlocal manquant dans config" =
      !is.null(get("config", .GlobalEnv)$DBConnectlocal)
  )
  
  cfg <- get("config", .GlobalEnv)
  
  stopifnot(inherits(in_grid, "sf"))
  stopifnot(is.data.frame(in_table))
  stopifnot(id_grid %in% names(in_grid))
  stopifnot(id_table %in% names(in_table))
  
  # ---- Merge grille / résultats ----------------------------------------
  merged <- merge(
    in_grid,
    in_table,
    by.x = id_grid,
    by.y = id_table,
    all.x = TRUE,
    sort = FALSE
  )
  
  message(
    "Rows grid=", nrow(in_grid),
    " table=", nrow(in_table),
    " merged=", nrow(merged)
  )
  message(
    "NA ratio first simul col=",
    mean(is.na(merged[[setdiff(names(in_table), id_table)[1]]]))
  )
  
  dsn_pg <- sprintf(
    "PG:host=%s port=%s dbname=%s user=%s password=%s",
    cfg$DBConnectlocal$host,
    cfg$DBConnectlocal$port,
    cfg$DBConnectlocal$dbname,
    cfg$DBConnectlocal$user,
    cfg$DBConnectlocal$password
  )
  
  sf::st_write(
    merged,
    dsn = dsn_pg,
    layer = in_nametab,
    layer_options = c(sprintf("SCHEMA=%s", in_scheme), "OVERWRITE=YES"),
    quiet = TRUE
  )
  
  invisible(NULL)
}

# ---- Construire table speed_sensib pour optimisation calculs ----
build_speed_sensib_table <- function(HabBenth_z, lHabBenth_z, ctx) {
  
  nb_poly <- nrow(HabBenth_z)
  
  # Détection robuste de la colonne qui porte l'id maille
  id_candidates <- c("idmesh", "id2", "E_id2")
  id_col <- id_candidates[id_candidates %in% names(HabBenth_z)][1]
  
  if (is.na(id_col) || !nzchar(id_col)) {
    stop(
      "build_speed_sensib_table: colonne id maille introuvable dans HabBenth_z. ",
      "Colonnes dispo: ", paste(names(HabBenth_z), collapse = ", ")
    )
  }
  
  i_idzhab  <- match(id_col, names(HabBenth_z))
  i_surfhab <- match("E_surfhab", names(HabBenth_z))
  i_surfmer <- match("E_surfmer", names(HabBenth_z))
  
  if (is.na(i_surfhab) || is.na(i_surfmer)) {
    stop(
      "build_speed_sensib_table: colonnes E_surfhab/E_surfmer manquantes. ",
      "Colonnes dispo: ", paste(names(HabBenth_z), collapse = ", ")
    )
  }
  
  n_id <- numeric(nb_poly)
  ratio_hab <- numeric(nb_poly)
  
  for (ipoly in seq_len(nb_poly)) {
    
    idmesh <- HabBenth_z[[i_idzhab]][ipoly]
    
    # index de la maille dans l'ordre de lHabBenth_z$idmesh_z
    idx <- match(idmesh, lHabBenth_z$idmesh_z)
    if (is.na(idx)) {
      # si la maille du polygone n'est pas dans la liste, on met 0 (ne contribuera pas)
      n_id[ipoly] <- 0
      ratio_hab[ipoly] <- 0
      next
    }
    n_id[ipoly] <- idx
    
    surfhab <- suppressWarnings(as.numeric(HabBenth_z[[i_surfhab]][ipoly]))
    surfmer <- suppressWarnings(as.numeric(HabBenth_z[[i_surfmer]][ipoly]))
    
    if (!is.na(surfhab) && !is.na(surfmer) && surfmer > 0) {
      ratio_hab[ipoly] <- surfhab / surfmer
    } else {
      ratio_hab[ipoly] <- 0
    }
  }
  
  data.frame(
    n_id = n_id,
    ratio_hab = ratio_hab,
    stringsAsFactors = FALSE
  )
}

# ---- Récupérer spec d'un dataset par Table_code ----
get_dataset_spec <- function(config, table_code) {
  
  dsets <- config$datasets
  
  if (is.data.frame(dsets)) {
    if (!"Table_code" %in% names(dsets)) {
      stop("get_dataset_spec: colonne 'Table_code' absente de config$datasets")
    }
    
    idx <- which(dsets$Table_code == table_code)
    if (length(idx) == 0) {
      stop(sprintf("Dataset '%s' introuvable dans config$datasets", table_code))
    }
    
    return(dsets[idx[1], , drop = FALSE])
  }
  
  if (is.list(dsets)) {
    for (ds in dsets) {
      if (!is.null(ds$Table_code) && ds$Table_code == table_code) {
        return(ds)
      }
    }
  }
  
  stop(sprintf("Dataset '%s' introuvable dans config$datasets", table_code))
}
