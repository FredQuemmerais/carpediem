# engine/functions/f_aux_step3.R
# Fonctions auxiliaires pour la production de graphiques CARPEDIEM (step3)

library(ggplot2)

# Fonctions helpers ------
{
# Thèmes ------

theme_carpediem <- function(base_size = 35, title_size = 50) {
  theme_bw() +
    theme(
      plot.title        = element_text(size = title_size, face = "bold", margin = margin(b = 20)),
      axis.title.x      = element_text(size = base_size + 10, face = "bold", margin = margin(t = 20)),
      axis.title.y      = element_text(size = base_size + 5,  face = "bold", margin = margin(r = 20)),
      axis.text.x       = element_text(size = base_size,      margin = margin(t = 10)),
      axis.text.y       = element_text(size = base_size,      margin = margin(r = 10)),
      axis.ticks.length = unit(0.3, "cm"),
      panel.grid.major  = element_line(color = "grey90"),
      panel.grid.minor  = element_blank(),
      plot.margin       = margin(t = 30, r = 40, b = 30, l = 30, unit = "pt")
    )
}

theme_carpediem_light <- function() {
  theme_bw() +
    theme(
      axis.title   = element_text(size = 20, face = "bold"),
      axis.text    = element_text(size = 16),
      legend.text  = element_text(size = 18),
      legend.title = element_blank(),
      legend.position = "bottom"
    )
}

# Exporter un graphique ggplot vers un fichier image -------
#' @param plot   Objet ggplot
#' @param dir    Dossier de destination
#' @param filename Nom de fichier avec extension (.jpeg ou .png)
#' @param width,height Dimensions en cm (défaut : 50 × 45)
#' @param dpi    Résolution (défaut : 300)
export_graph <- function(plot, dir, filename, width = 50, height = 45, dpi = 300) {
  path <- file.path(dir, filename)
  if (file.exists(path)) {
    message("  [skip] ", filename, " existe déjà.")
    return(invisible(path))
  }
  ext <- tolower(tools::file_ext(filename))
  ggsave(filename = filename, plot = plot, device = ext,
         path = dir, width = width, height = height,
         units = "cm", dpi = dpi)
  message("  [export] ", filename)
  invisible(path)
}

# Diviser un vecteur en n classes d'intervalles égaux -----
#' @param vec  Vecteur numérique (les NA sont supprimés)
#' @param nbclass Nombre de classes
#' @return data.frame : Cat (facteur), Compte, Pourcentage
prep_graph <- function(vec, nbclass) {
  vec <- vec[!is.na(vec)]
  if (length(vec) == 0) stop("prep_graph : vecteur vide après suppression des NA.")
  
  inter <- diff(range(vec)) / nbclass
  bounds <- min(vec) + (0:nbclass) * inter
  
  labels <- paste(round(bounds[-nbclass - 1], 2), round(bounds[-1], 2), sep = " - ")
  
  compte <- vapply(seq_len(nbclass), function(i) {
    inf_i <- bounds[i]; sup_i <- bounds[i + 1]
    if (i < nbclass) sum(vec >= inf_i & vec < sup_i)
    else             sum(vec >= inf_i & vec <= sup_i)
  }, integer(1))
  
  data.frame(
    Cat        = factor(labels, levels = labels),
    Compte     = compte,
    Pourcentage = round(compte / length(vec) * 100, 2)
  )
}
# Variante de prep_graph – retourne NULL si vec est vide ou tout-zéro
prep_graph_bis <- function(vec, nbclass) {
  vec <- na.omit(vec)
  if (length(vec) == 0 || max(vec) == 0) {
    warning("prep_graph_bis : vecteur vide ou nul.")
    return(NULL)
  }
  prep_graph(vec, nbclass)
}

}
# Fonctions graphiques ------
{
# Histogramme générique (continu ou discret) ---------
#
#' Dispatch automatique selon le type de `col` :
#'   - numérique  → classes d'intervalles égaux (ncat classes)
#'   - facteur    → une barre par modalité
#
#' @param tab    data.frame
#' @param col    Nom ou indice de la colonne à représenter
#' @param title  Libellé de l'axe X
#' @param ncat   Nombre de classes (uniquement pour variables continues)
#' @param fill   Couleur des barres

make_hist <- function(tab, col, title, ncat = 5, fill = "#96d1de") {
  
  values <- tab[[col]]
  
  if (is.factor(values) || is.integer(values) ||
      (is.numeric(values) && all(values == floor(values), na.rm = TRUE) &&
       length(unique(values[!is.na(values)])) <= 20)) {
    # ── Variable discrète ──────────────────────────────────────────────────
    nmax <- max(values, na.rm = TRUE)
    n    <- nrow(tab)
    plot_data <- data.frame(
      valeur     = factor(0:nmax, levels = as.character(0:nmax)),
      Pourcentage = vapply(0:nmax,
                           function(v) sum(values == v, na.rm = TRUE) / n * 100,
                           numeric(1))
    )
    ggplot(plot_data, aes(x = valeur, y = Pourcentage)) +
      geom_bar(stat = "identity", width = 0.7, fill = fill) +
      scale_x_discrete(limits = as.character(0:nmax)) +
      scale_y_continuous(labels = function(x) paste0(x, " %")) +
      labs(x = title, y = "Pourcentage (%)") +
      theme_carpediem(base_size = 20, title_size = 20)
    
  } else {
    # ── Variable continue ──────────────────────────────────────────────────
    plot_data <- prep_graph(values, ncat)
    ggplot(plot_data, aes(x = Cat, y = Pourcentage)) +
      geom_bar(stat = "identity", width = 0.7, fill = fill) +
      scale_y_continuous(labels = function(x) paste0(x, " %"),
                         limits = c(0, 100)) +
      labs(x = paste0(title, " (classes)"), y = "Pourcentage de mailles") +
      theme_carpediem(base_size = 35, title_size = 50)
  }
}

# Histogramme comparatif : zone étudiée vs zone de référence --------
#' @param tab_zone  data.frame de la zone étudiée
#' @param tab_ref   data.frame de référence (zone totale)
#' @param col       Nom ou indice de la colonne (même colonne dans les deux tables)
#' @param title     Libellé de l'axe X
#' @param ncat      Nombre de classes (variables continues uniquement)
#' @param ref_label Libellé de la zone de référence
make_hist_compare <- function(tab_zone, tab_ref, col, title, ncat = 5, ref_label = "Zone totale") {
  
  values_zone <- tab_zone[[col]]
  values_ref  <- tab_ref[[col]]
  COLORS <- c("#D62976", "#feda75")
  
  # Détection discret / continu (même logique que make_hist)
  is_discrete <- is.factor(values_ref) || is.integer(values_ref) ||
    (is.numeric(values_ref) &&
       all(values_ref == floor(values_ref), na.rm = TRUE) &&
       length(unique(values_ref[!is.na(values_ref)])) <= 20)
  
  if (is_discrete) {
    nmax <- max(values_ref, na.rm = TRUE)
    make_side <- function(vals, label) {
      data.frame(
        valeur      = factor(0:nmax, levels = as.character(0:nmax)),
        Pourcentage = vapply(0:nmax,
                             function(v) sum(vals == v, na.rm = TRUE) / length(vals) * 100,
                             numeric(1)),
        Zone = label
      )
    }
    plot_data <- rbind(
      make_side(values_zone, " zone etudiee "),
      make_side(values_ref,  ref_label)
    )
    ggplot(plot_data, aes(x = valeur, y = Pourcentage, fill = Zone)) +
      geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
      scale_fill_manual(values = COLORS) +
      scale_x_discrete(limits = as.character(0:nmax)) +
      labs(x = title, y = "Pourcentage (%)") +
      theme_carpediem_light()
    
  } else {
    pg_zone <- prep_graph_bis(values_zone, ncat)
    pg_ref  <- prep_graph_bis(values_ref,  ncat)
    if (is.null(pg_zone) || is.null(pg_ref)) return(NULL)
    
    plot_data <- rbind(
      cbind(pg_zone, Zone = " zone etudiee "),
      cbind(pg_ref,  Zone = ref_label)
    )
    ggplot(plot_data, aes(x = Cat, y = Pourcentage, fill = Zone)) +
      geom_bar(stat = "identity", width = 0.7, position = position_dodge()) +
      scale_fill_manual(values = COLORS) +
      scale_y_continuous(limits = c(0, 100)) +
      labs(x = title, y = "Pourcentage (%)") +
      theme_carpediem_light()
  }
}

# Alias de rétrocompatibilité (non exportés dans la doc)
f_hist_cont     <- function(tab, col, ncat, title)                    make_hist(tab, col, title, ncat)
f_hist_cont_bis <- function(tab, col, TAB, COL, ncat, title, lbl="Zone totale") make_hist_compare(tab, TAB, col, title, ncat, lbl)
f_hist_dis      <- function(tab, col, title)                          make_hist(tab, col, title)
f_hist_dis_bis  <- function(tab, col, TAB, title, lbl="Zone totale")  make_hist_compare(tab, TAB, col, title, ref_label=lbl)

# Boxplot d'une variable continue en fonction d'une variable discrète ------------
#' @param tab      data.frame
#' @param col_x    Nom ou indice de la colonne X (variable discrète / facteur)
#' @param col_y    Nom ou indice de la colonne Y (variable continue)
#' @param title_x  Libellé axe X
#' @param title_y  Libellé axe Y
f_boxplot <- function(tab, col_x, col_y, title_x, title_y) {
  ggplot(tab, aes(x = .data[[col_x]], y = .data[[col_y]])) +
    geom_boxplot(color = "#2f367f", fill = "#dbdef6") +
    labs(x = title_x, y = title_y) +
    scale_fill_brewer(palette = "Blues") +
    theme_carpediem(base_size = 20, title_size = 20) +
    theme(legend.position = "none")
}

# Graphiques bilan activités et pressions ------------
# Calcule la contribution au REFC total
#' @param res1 Résultat de l'étape 1
#' @return data.frame idmesh + une colonne par activité
compute_contrib_activites <- function(res1) {
  
  A_df       <- res1$ctx$A_zi_lognorm
  MatAP      <- res1$ctx$MatAP_lien        # activités en lignes, pressions en colonnes
  Mat_Pj_z   <- res1$ctx$Mat_Pj_z          # pressions brutes (avant lognorm)
  Mat_REF    <- res1$effets                 # ref_e1_p par pression
  P_code_sel <- res1$ctx$P_code_sel
  act_corresp <- res1$ctx$act_data_corresp  # Code <-> data_col
  
  A_mat <- as.matrix(A_df[, setdiff(names(A_df), "idmesh"), drop = FALSE])
  nz    <- nrow(A_mat)
  
  # Codes activités (sans suffixe _norm)
  codes_A_norm <- colnames(A_mat)                        # "A_1_1_1_norm"
  codes_A      <- gsub("_norm$", "", codes_A_norm)       # "A_1_1_1"
  n_act        <- length(codes_A)
  
  # Somme REFC avant normalisation = somme de toutes les ref_e1_p
  ref_cols_all <- paste0("ref_e1_", P_code_sel)
  ref_cols_all <- intersect(ref_cols_all, colnames(Mat_REF))
  total_refc_raw <- sum(as.matrix(Mat_REF[, ref_cols_all]), na.rm = TRUE)
  
  contrib <- data.frame(idmesh = A_df$idmesh)
  
  for (i in seq_len(n_act)) {
    
    code_act  <- codes_A[i]
    col_norm  <- codes_A_norm[i]
    contrib_col <- numeric(nz)
    
    for (p in P_code_sel) {
      
      if (!(p %in% colnames(MatAP))) next
      if (!(code_act %in% rownames(MatAP))) next
      
      amp_ip <- MatAP[code_act, p]
      if (is.na(amp_ip) || amp_ip <= 0) next
      
      ref_col <- paste0("ref_e1_", p)
      if (!(ref_col %in% colnames(Mat_REF))) next
      
      # Contribution de l'activité i à la pression p sur chaque maille
      Ai_norm  <- A_mat[, col_norm]
      Pj_total <- Mat_Pj_z[[p]]
      
      # Part de l'activité i dans la pression p (maille par maille)
      part_i_p <- ifelse(
        !is.na(Pj_total) & Pj_total > 0,
        (Ai_norm * amp_ip) / Pj_total,
        0
      )
      
      # Pondération par la contribution de la pression p au REFC
      ref_p <- Mat_REF[[ref_col]]
      ref_p[is.na(ref_p)] <- 0
      
      contrib_col <- contrib_col + part_i_p * ref_p
    }
    
    contrib[[col_norm]] <- contrib_col
  }
  
  contrib
}

# Graphique bilan unifié (activités ou pressions) -------
#' @param labels      Vecteur de noms lisibles (activités ou pressions)
#' @param pct_exposed % de mailles exposées par entité
#' @param pct_sensitive % de mailles sensibles par entité
#' @param pct_contrib % de contribution au REFC par entité
#' @param title       Titre du graphique
#' @param colors      Vecteur nommé de 3 couleurs
.make_bilan_plot <- function(labels, pct_exposed, pct_sensitive, pct_contrib, title, colors) {
  n <- length(labels)
  stat_labels <- c("Mailles exposées", "Mailles sensibles", "Contribution REFC")
  
  df <- data.frame(
    Entite      = rep(labels, each = 3),
    Statistique = rep(stat_labels, times = n),
    Pourcentage = c(rbind(pct_exposed, pct_sensitive, pct_contrib)),
    stringsAsFactors = FALSE
  )
  df$Statistique <- factor(df$Statistique, levels = stat_labels)
  
  ordre <- labels[order(pct_contrib)]
  
  ggplot(df, aes(x = Entite, y = Pourcentage, fill = Statistique)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
    coord_flip() +
    scale_x_discrete(limits = ordre) +
    scale_y_continuous(labels = function(x) paste0(x, " %")) +
    scale_fill_manual(values = colors, drop = FALSE) +
    labs(title = title, x = NULL, y = NULL) +
    theme_carpediem() +
    theme(
      axis.text.y      = element_text(size = 20, margin = margin(r = 10)),
      legend.text      = element_text(size = 30),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.key.size  = unit(1.5, "cm"),
      legend.spacing.x = unit(1, "cm")
    ) +
    guides(fill = guide_legend(nrow = 1))
}

# Bilan des activités (mailles exposées, sensibles, contribution REFC)
#' @param res1           Résultat de l'étape 1
#' @param lines          Indices de lignes des mailles à analyser
#' @param activity_names Noms lisibles des activités (NULL = codes bruts)
Stat_Ai_plot <- function(res1, lines, activity_names = NULL) {
  A_zi       <- res1$activites
  Contrib_Ai <- compute_contrib_activites(res1)
  
  if (is.null(activity_names))
    activity_names <- colnames(A_zi)[-1]
  
  n          <- length(activity_names)
  P_code_sel <- res1$ctx$P_code_sel
  MatAP_lien <- res1$ctx$MatAP_lien
  Mat_REX    <- res1$ctx$Mat_REX_PjE1_z   # ← REX, pas effets
  codes_A    <- gsub("_norm$", "", colnames(A_zi)[-1])
  
  # 1. % mailles exposées — inchangé
  pct_exp <- vapply(seq_len(n), function(i)
    sum(A_zi[lines, i + 1] > 0) / length(lines) * 100, numeric(1))
  
  # 2. % mailles sensibles — via Mat_REX_PjE1_z
  pct_sen <- vapply(seq_len(n), function(i) {
    
    code_act <- codes_A[i]
    if (!(code_act %in% rownames(MatAP_lien))) return(0)
    
    pressions_liees <- intersect(P_code_sel, colnames(MatAP_lien))
    pressions_liees <- pressions_liees[
      !is.na(MatAP_lien[code_act, pressions_liees]) &
        MatAP_lien[code_act, pressions_liees] > 0
    ]
    if (length(pressions_liees) == 0) return(0)
    
    rex_cols <- intersect(paste0("REX_E1_", pressions_liees), colnames(Mat_REX))
    if (length(rex_cols) == 0) return(0)
    
    sensible <- rowSums(Mat_REX[lines, rex_cols, drop = FALSE] > 0, na.rm = TRUE) > 0
    sum(sensible) / length(lines) * 100
    
  }, numeric(1))
  
  # 3. % contribution au REFC — dénominateur = somme ref_e1 bruts
  ref_cols_all   <- intersect(paste0("ref_e1_", P_code_sel), colnames(res1$effets))
  total_refc_raw <- sum(as.matrix(res1$effets[lines, ref_cols_all]), na.rm = TRUE)
  
  pct_contrib <- if (total_refc_raw > 0) {
    vapply(seq_len(n), function(i)
      sum(Contrib_Ai[lines, i + 1], na.rm = TRUE) / total_refc_raw * 100, numeric(1))
  } else rep(0, n)
  
  .make_bilan_plot(
    labels        = activity_names,
    pct_exposed   = pct_exp,
    pct_sensitive = pct_sen,
    pct_contrib   = pct_contrib,
    title         = "Bilan Activités",
    colors        = c(
      "Mailles exposées"  = "#4C6B4F",
      "Mailles sensibles" = "#56C46A",
      "Contribution REFC" = "#A3E589"
    )
  )
}

# Bilan des pressions par niveau hiérarchique PressRef (N1, N2, N3)
#'
#' Pour chaque pression du démonstrateur :
#'   pct_exposed   = % mailles avec score > 0
#'   pct_sensitive = % mailles avec au moins une feuille descendante ref_e1 > 0
#'   pct_contrib   = score normalisé à 100% par niveau
#'
#' @param res1  Résultat de l'étape 1
#' @param lines Indices de lignes des mailles à analyser
#' @return Liste nommée list(N1 = ggplot|NULL, N2 = ggplot|NULL, N3 = ggplot|NULL)

Stat_Pj_plot <- function(res1, lines) {
  
  stopifnot(
    "Stat_Pj_plot: res1$score_tbl manquant"     = !is.null(res1$score_tbl),
    "Stat_Pj_plot: res1$ctx$pressure_ref manquant" = !is.null(res1$ctx$pressure_ref),
    "Stat_Pj_plot: res1$ctx$typo_press manquant"   = !is.null(res1$ctx$typo_press),
    "Stat_Pj_plot: res1$effets manquant"        = !is.null(res1$effets)
  )
  
  score_tbl <- res1$score_tbl
  pr_ref    <- res1$ctx$pressure_ref
  typo      <- res1$ctx$typo_press
  effets    <- res1$effets
  
  # Normaliser les noms de colonnes de pressure_ref pour matcher (niveau, nom)
  names(pr_ref) <- norm_name(names(pr_ref))
  stopifnot(
    "Stat_Pj_plot: code_p manquant dans pressure_ref"       = "code_p"       %in% names(pr_ref),
    "Stat_Pj_plot: niveau manquant dans pressure_ref"       = "niveau"       %in% names(pr_ref),
    "Stat_Pj_plot: nom_pression manquant dans pressure_ref" = "nom_pression" %in% names(pr_ref)
  )
  
  # ── Identifier les pressions du démonstrateur présentes dans score_tbl ──
  codes_score <- setdiff(names(score_tbl), "idmesh")  # déjà en lowercase
  if (length(codes_score) == 0) {
    warning("Stat_Pj_plot: score_tbl ne contient aucune pression. Aucun graphique produit.")
    return(list(N1 = NULL, N2 = NULL, N3 = NULL))
  }
  
  codes_ref_norm <- norm_name(as.character(pr_ref$code_p))
  idx_ref        <- match(codes_score, codes_ref_norm)
  
  if (any(is.na(idx_ref))) {
    warning(sprintf(
      "Stat_Pj_plot: %d code(s) de score_tbl absent(s) de pressure_ref : %s",
      sum(is.na(idx_ref)), paste(codes_score[is.na(idx_ref)], collapse = ", ")
    ))
  }
  
  info_pressions <- data.frame(
    code_score  = codes_score,
    niveau      = pr_ref$niveau[idx_ref],
    nom         = pr_ref$nom_pression[idx_ref],
    stringsAsFactors = FALSE
  )
  info_pressions <- info_pressions[!is.na(info_pressions$niveau), , drop = FALSE]
  
  # ── Pré-calcul : table des feuilles évaluées (présentes dans effets) ──
  # Une feuille est ici "feuille évaluée" = code présent dans effets via ref_e1_<code>
  ref_cols       <- grep("^ref_e1_", names(effets), value = TRUE)
  codes_feuilles <- sub("^ref_e1_", "", ref_cols)  # lowercase
  
  # ── Construire la hiérarchie : code_norm -> parent_norm (lowercase) ──
  typo_codes   <- norm_name(as.character(typo$Code_P))
  typo_parents <- norm_name(as.character(typo$parent))
  typo_parents[is.na(typo$parent)] <- NA_character_
  parent_of <- stats::setNames(typo_parents, typo_codes)
  
  # Pour un code donné, renvoie le vecteur de codes des feuilles évaluées
  # qui en descendent (y compris lui-même si c'est une feuille évaluée).
  descendants_feuilles <- function(code_root) {
    out <- character(0)
    if (code_root %in% codes_feuilles) out <- c(out, code_root)
    # parcours descendant : enfants directs = codes dont parent == code_root
    enfants <- names(parent_of)[!is.na(parent_of) & parent_of == code_root]
    for (e in enfants) out <- c(out, descendants_feuilles(e))
    unique(out)
  }
  
  # ── Boucle de calcul par niveau ──
  niveaux <- c(1L, 2L, 3L)
  out <- vector("list", length(niveaux))
  names(out) <- paste0("N", niveaux)
  
  n_total_mailles <- length(lines)
  
  for (lvl in niveaux) {
    
    pressions_lvl <- info_pressions[info_pressions$niveau == lvl, , drop = FALSE]
    if (nrow(pressions_lvl) == 0) {
      message(sprintf("  [info] Bilan_pressions_N%d : aucune pression évaluée à ce niveau, graphique omis.", lvl))
      out[[paste0("N", lvl)]] <- NULL
      next
    }
    
    n <- nrow(pressions_lvl)
    pct_exp     <- numeric(n)
    pct_sen     <- numeric(n)
    contrib_brut <- numeric(n)  # somme des scores sur les mailles ciblées
    
    for (i in seq_len(n)) {
      
      code_p <- pressions_lvl$code_score[i]
      
      # pct_exposed : % de mailles avec score_tbl[m, p] > 0
      vec_score <- score_tbl[lines, code_p]
      vec_score[!is.finite(vec_score)] <- 0
      pct_exp[i] <- sum(vec_score > 0) / n_total_mailles * 100
      
      # pct_sensitive : OR sur les feuilles descendantes
      feuilles <- descendants_feuilles(code_p)
      ref_cols_p <- paste0("ref_e1_", feuilles)
      ref_cols_p <- intersect(ref_cols_p, ref_cols)
      
      if (length(ref_cols_p) == 0) {
        pct_sen[i] <- 0
      } else {
        mat_ref <- as.matrix(effets[lines, ref_cols_p, drop = FALSE])
        mat_ref[!is.finite(mat_ref)] <- 0
        sensible_par_maille <- rowSums(mat_ref > 0) > 0
        pct_sen[i] <- sum(sensible_par_maille) / n_total_mailles * 100
      }
      
      # Contribution brute : somme des scores sur les mailles ciblées
      contrib_brut[i] <- sum(vec_score, na.rm = TRUE)
    }
    
    # Normaliser pct_contrib à 100% par niveau
    total_lvl <- sum(contrib_brut, na.rm = TRUE)
    pct_contrib <- if (total_lvl > 0) contrib_brut / total_lvl * 100 else rep(0, n)
    
    out[[paste0("N", lvl)]] <- .make_bilan_plot(
      labels        = pressions_lvl$nom,
      pct_exposed   = pct_exp,
      pct_sensitive = pct_sen,
      pct_contrib   = pct_contrib,
      title         = sprintf("Bilan Pressions - Niveau %d", lvl),
      colors        = c(
        "Mailles exposées"  = "#842658",
        "Mailles sensibles" = "#DD308D",
        "Contribution REFC" = "#E894C1"
      )
    )
  }
  
  out
}

# ── Versions simplifiées (présence uniquement) ─────────────────────────────────

Stat_Ai_plot_bis <- function(res1, lines, activity_names = NULL) {
  A_zi <- res1$activites
  if (is.null(activity_names)) activity_names <- colnames(A_zi)[-1]
  
  df <- data.frame(
    Activite    = activity_names,
    Pourcentage = vapply(seq_along(activity_names), function(i)
      sum(A_zi[lines, i + 1] > 0) / length(lines) * 100, numeric(1))
  )
  
  ggplot(df, aes(x = Activite, y = Pourcentage)) +
    geom_bar(stat = "identity", fill = "#4C6B4F", width = 0.7) +
    coord_flip() +
    scale_y_continuous(labels = function(x) paste0(x, " %")) +
    labs(title = "Bilan Activités", x = NULL, y = "Mailles exposées") +
    theme_carpediem() +
    theme(legend.position = "none")
}

Stat_Pj_plot_bis <- function(res1, lines, pressure_names = NULL) {
  Mat_Pj_z <- res1$ctx$Mat_Pj_z
  if (is.null(pressure_names)) pressure_names <- colnames(Mat_Pj_z)[-1]
  
  df <- data.frame(
    Pression    = pressure_names,
    Pourcentage = vapply(seq_along(pressure_names), function(i)
      sum(Mat_Pj_z[lines, i + 1] > 0) / length(lines) * 100, numeric(1))
  )
  
  ggplot(df, aes(x = Pression, y = Pourcentage)) +
    geom_bar(stat = "identity", fill = "#33CC00", color = "black", width = 0.7) +
    coord_flip() +
    labs(y = "Présence (% de mailles)") +
    theme_bw() +
    theme(axis.title = element_text(size = 10), axis.text = element_text(size = 10))
}

# Graphique en facettes des indices moyens par zone ------
#' @param data_index  Tableau des indices
#' @param field_zones Vecteur de codes de zone (une valeur par maille)
#' @param categories  Vecteur des catégories à comparer
f_compare <- function(data_index, field_zones, categories) {
  indexes <- c("IMA1", "IMA2", "IPC1", "IPC2", "REFC")
  # colonnes : idmesh [1], IMA1 [2], IMA2 [3], IPC1 [4], IPC2 [5], REFC_E1 [6]
  col_idx <- 2:6
  
  df_list <- lapply(categories, function(cat) {
    rows <- which(field_zones == cat)
    do.call(rbind, lapply(seq_along(indexes), function(h) {
      data.frame(
        categorie = cat,
        index     = indexes[h],
        moyenne   = mean(data_index[rows, col_idx[h]], na.rm = TRUE),
        sd        = sd(data_index[rows, col_idx[h]],   na.rm = TRUE)
      )
    }))
  })
  categ_stat <- do.call(rbind, df_list)
  
  ggplot(categ_stat, aes(x = categorie, y = moyenne, fill = index)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = moyenne - sd, ymax = moyenne + sd),
                  width = 0.2, position = position_dodge(0.9)) +
    facet_grid(index ~ ., scales = "free") +
    scale_fill_manual(values = c("#feda75", "#fa7e1e", "#d62976", "#962fbf", "#4F5BD5")) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.text      = element_text(size = 12, face = "bold"),
      axis.title      = element_blank(),
      axis.text.x     = element_text(size = 12, angle = 70, hjust = 1),
      axis.text.y     = element_text(size = 12)
    )
}

# Nuage de points REFC vs IPC2 coloré par zone -----
#' @param data_index  Tableau des indices
#' @param field_zones Vecteur de codes de zone
#' @param categories  Catégories à représenter
f_compare2 <- function(data_index, field_zones, categories) {
  tab_plot <- cbind(data_index, zone = field_zones)
  tab_plot <- tab_plot[field_zones %in% categories & !is.na(data_index$REFC_E1), ]
  
  ggplot(tab_plot, aes(x = IPC_option2, y = REFC_E1, colour = zone)) +
    geom_point() +
    scale_colour_viridis_d(begin = 0.1, end = 1, direction = 1, option = "C") +
    labs(x = "IPC2", y = "REFC") +
    theme_gray() +
    theme(
      axis.title      = element_text(size = 12),
      axis.text       = element_text(size = 12),
      legend.text     = element_text(size = 12),
      legend.title    = element_blank(),
      legend.position = "bottom"
    )
}

# Radar chart des indices de confiance globaux ----
f_radar_IC <- function(res1,
                       out_dir,
                       file_name = "Radar_IC.jpeg",
                       width     = 50,
                       height    = 45,
                       units     = "cm",
                       res       = 300,
                       phi       = res1$ctx$phi_IC,
                       labels    = c(
                         "Score global de \nQualité des données",
                         "Cartographie\ndes activités",
                         "Lien\nactivités/pressions",
                         "Cartographie\ndes habitats",
                         "Sensibilité\ndes habitats"
                       )) {
  
  if (!requireNamespace("fmsb",   quietly = TRUE)) install.packages("fmsb")
  if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")
  
  stopifnot(
    "res1$ctx manquant"       = !is.null(res1$ctx),
    "ctx$IC_global manquant"  = !is.null(res1$ctx$IC_global),
    "ctx$IQ_A_mean manquant"  = !is.null(res1$ctx$IQ_A_mean),
    "ctx$IC_AP_mean manquant" = !is.null(res1$ctx$IC_AP_mean),
    "ctx$IQ_H_mean manquant"  = !is.null(res1$ctx$IQ_H_mean),
    "ctx$IC_HP_mean manquant" = !is.null(res1$ctx$IC_HP_mean)
  )
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, file_name)
  
  if (file.exists(out_path)) {
    message("  [skip] ", file_name, " existe déjà.")
    return(invisible(list(path = out_path)))
  }
  
  # Petite fonction utilitaire : calcule min / médiane / max sur un vecteur,
  # en gérant les NA et les valeurs non finies, puis clampe entre 0 et 1
  summarise_clamp <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) {
      return(c(min = 0, med = 0, max = 0))
    }
    c(
      min = min(x, na.rm = TRUE),
      med = stats::median(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  }
  
  # Liste des vecteurs sources, dans l'ordre des labels
  sources <- list(
    res1$ctx$IC_global$IC,
    res1$ctx$IQ_A_mean$IQ_A_mean,
    res1$ctx$IC_AP_mean$IC_AP_mean,
    res1$ctx$IQ_H_mean$IQ_H_mean,
    res1$ctx$IC_HP_mean$IC_HP_mean
  )
  
  # Matrice 3 x 5 : lignes = min/med/max, colonnes = indices
  stats_mat <- t(sapply(sources, summarise_clamp))
  
  # Clamp strict 0..1 (sans passer par pmin/pmax qui cassent le dim)
  stats_mat[!is.finite(stats_mat)] <- 0
  stats_mat[stats_mat > 1] <- 1
  stats_mat[stats_mat < 0] <- 0
  
  # --- Annotation des labels avec les poids phi (si disponibles) ----------
  # phi attendu nommé c(IQ_A=, IC_AP=, IQ_H=, IC_HP=) correspondant aux
  # axes 2 à 5 (l'axe 1, IC global, n'a pas de poids propre)
  nms <- labels
  if (!is.null(phi)) {
    phi_order <- c("IQ_A", "IC_AP", "IQ_H", "IC_HP")
    missing_phi <- setdiff(phi_order, names(phi))
    if (length(missing_phi) > 0) {
      warning("f_radar_IC: phi incomplet (manque : ",
              paste(missing_phi, collapse = ", "), "), labels non annotés.",
              call. = FALSE)
    } else {
      w <- phi[phi_order]
      nms[2:5] <- paste0(nms[2:5], sprintf("\n(\u03c6=%.0f%%)", 100 * w))
    }
  }
  
  rownames(stats_mat) <- nms
  
  vals_min <- stats_mat[, "min"]
  vals_med <- stats_mat[, "med"]
  vals_max <- stats_mat[, "max"]
  
  # Construction du data.frame fmsb : ligne 1 = max théorique (1),
  # ligne 2 = min théorique (0), puis une ligne par série (max, med, min)
  radar_df <- as.data.frame(
    rbind(
      rep(1, length(nms)),
      rep(0, length(nms)),
      as.numeric(vals_max),
      as.numeric(vals_med),
      as.numeric(vals_min)
    )
  )
  colnames(radar_df) <- nms
  row.names(radar_df) <- NULL
  
  grDevices::jpeg(filename = out_path, width = width, height = height,
                  units = units, res = res)
  
  old_par <- par(no.readonly = TRUE)
  on.exit({ par(old_par); grDevices::dev.off() }, add = TRUE)
  
  par(mar = c(5, 5, 5, 5), bg = "white", cex.axis = 4, cex.main = 5)
  
  # 3 couleurs : max / médiane / min
  cols_line <- c("#96d1de", "#d62976", "#842658")
  cols_fill <- scales::alpha(cols_line, 0.25)
  
  fmsb::radarchart(
    radar_df,
    axistype     = 1,
    pcol         = cols_line,
    pfcol        = cols_fill,
    plwd         = 4,
    plty         = 1,
    cglcol       = "grey70",
    cglty        = 1,
    cglwd        = 0.8,
    axislabcol   = "grey45",
    caxislabels  = c("0", "0.25", "0.50", "0.75", "1.00"),
    vlcex        = 3,
    seg          = 4,
    title        = "Profil des indices de confiance"
  )
  
  legend(
    x      = "topright",
    legend = c("Max", "Médiane", "Min"),
    col    = cols_line,
    lty    = 1,
    lwd    = 4,
    bty    = "n",
    cex    = 2.5
  )
  
  message("  [export] ", file_name)
  
  invisible(list(
    path   = out_path,
    values = list(min = vals_min, med = vals_med, max = vals_max)
  ))
}

}
# Fonction principale : graph_maker ------------
{
  # Résoudre les noms lisibles des activités depuis la feuille ODS ----
  .resolve_activity_names <- function(res1) {
    codes_act      <- setdiff(names(res1$activites), "idmesh")
    activity_names <- codes_act  # fallback
    
    act_press_path <- res1$ctx$act_press_path
    if (is.null(act_press_path) || !file.exists(act_press_path))
      return(activity_names)
    
    typo_act <- tryCatch(
      readODS::read_ods(act_press_path, sheet = "typologie_activites"),
      error = function(e) {
        warning("Lecture 'typologie_activites' échouée : ", e$message)
        NULL
      }
    )
    if (is.null(typo_act)) return(activity_names)
    
    names(typo_act) <- trimws(names(typo_act))
    names_lc <- tolower(names(typo_act))
    
    idx_code <- which(names_lc %in% c("code_a", "code"))
    idx_nom  <- which(names_lc %in% c("nom_activite", "activite_nom", "nom"))
    
    if (length(idx_code) == 0 || length(idx_nom) == 0) return(activity_names)
    
    names(typo_act)[idx_code[1]] <- "code_a"
    names(typo_act)[idx_nom[1]]  <- "nom_activite"
    
    noms <- typo_act$nom_activite[
      match(codes_act, as.character(typo_act$code_a))
    ]
    
    ifelse(is.na(noms), codes_act, as.character(noms))
  }
  
  # Créer l'ensemble des graphiques pour un sous-ensemble de mailles ------
  #' @param res1         Résultat de l'étape 1
  #' @param lines        Indices de lignes des mailles à analyser
  #' @param dir          Dossier de sortie (créé si absent)
  #' @param title_suffix Suffixe ajouté aux titres (ex. "(Zone: A)")
  graph_maker <- function(res1, lines, dir, title_suffix = "") {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    
    activity_names <- .resolve_activity_names(res1)
    
    # Données
    data_index <- res1$data_index
    sub_tab    <- data_index[lines, ]
    sub_lines  <- lines[!is.na(sub_tab$REFC_E1)]
    
    # data.frame avec X facteurs pour les boxplots
    tab_factor <- transform(data_index,
                            IMA_option1 = as.factor(IMA_option1),
                            IPC_option1 = as.factor(IPC_option1))
    sub_tab_factor <- tab_factor[lines, ]
    
    n_valid <- length(sub_lines)
    message(sprintf("  %d / %d mailles avec REFC non nul%s",
                    n_valid, length(lines),
                    if (nchar(title_suffix) > 0) paste0(" ", title_suffix) else ""))
    
    out_title <- sprintf("Graphique pour %d / %d mailles avec REFC non nul %s",
                         n_valid, length(lines), title_suffix)
    
    # Helper local : export conditionnel avec message ----
    exp <- function(plot, filename, ...) export_graph(plot, dir, filename, ...)
    
    # Histogrammes IMA1 / IMA2 / IPC1 / IPC2 ----
    exp(make_hist(sub_tab, "IMA_option1", "IMA1"), "Histogramme_IMA1.jpeg")
    exp(make_hist(sub_tab, "IPC_option1", "IPC1"), "Histogramme_IPC1.jpeg")
    
    if (max(sub_tab$IMA_option2, na.rm = TRUE) > 0)
      exp(make_hist(sub_tab, "IMA_option2", "IMA2"), "Histogramme_IMA2.jpeg")
    else message("  [skip] Histogramme_IMA2.jpeg : tous les IMA2 sont nuls.")
    
    if (max(sub_tab$IPC_option2, na.rm = TRUE) > 0)
      exp(make_hist(sub_tab, "IPC_option2", "IPC2"), "Histogramme_IPC2.jpeg")
    else message("  [skip] Histogramme_IPC2.jpeg : tous les IPC2 sont nuls.")
    
    # Boxplots IMA1/IMA2 et IPC1/IPC2 ----
    exp(f_boxplot(sub_tab_factor, "IMA_option1", "IMA_option2",
                  "IMA1 : nombre d'activités", "IMA2 : somme de l'intensité des activités"),
        "Boxplot_IMA1_IMA2.jpeg")
    
    exp(f_boxplot(sub_tab_factor, "IPC_option1", "IPC_option2",
                  "IPC1 : nombre de pressions", "IPC2 : somme de l'intensité des pressions"),
        "Boxplot_IPC1_IPC2.jpeg")
    
    # Graphiques dépendant du REFC (>= 5 mailles valides) ----
    if (n_valid > 5) {
      
      sub_data <- data_index[sub_lines, ]
      sub_fac  <- tab_factor[sub_lines, ]
      has_ic   <- "IC" %in% colnames(data_index)
      
      # Histogramme REFC
      if (max(sub_data$REFC_E1, na.rm = TRUE) > 0) {
        p_refc <- make_hist(sub_data, "REFC_E1", "REFC") +
          ggtitle("Distribution du Risque d'Effets Cumulés") +
          theme(plot.title = element_text(size = 50, face = "bold", margin = margin(b = 20)))
        exp(p_refc, "Histogramme_REFC.jpeg")
      } else {
        message("  [skip] Histogramme_REFC.jpeg : tous les REFC sont nuls.")
      }
      
      # Boxplots vs REFC (et IC si présent)
      exp(f_boxplot(sub_fac, "IMA_option1", "REFC_E1",
                    "IMA1 : nombre d'activités", "REFC") + ggtitle(out_title),
          "Boxplot_IMA1_REFC.jpeg")
      
      exp(f_boxplot(sub_fac, "IPC_option1", "REFC_E1",
                    "IPC1 : nombre de pressions", "REFC") + ggtitle(out_title),
          "Boxplot_IPC1_REFC.jpeg")
      
      if (has_ic) {
        exp(f_boxplot(sub_fac, "IMA_option1", "IC",
                      "IMA1 : nombre d'activités", "IC") + ggtitle(out_title),
            "Boxplot_IMA1_IC.jpeg")
        exp(f_boxplot(sub_fac, "IPC_option1", "IC",
                      "IPC1 : nombre de pressions", "IC") + ggtitle(out_title),
            "Boxplot_IPC1_IC.jpeg")
      }
      
      # Nuages de points REFC vs IPC2 (et IC)
      make_scatter <- function(x_col, x_lab, filename) {
        p <- ggplot(sub_data, aes(x = .data[[x_col]], y = REFC_E1)) +
          geom_point() +
          labs(x = x_lab, y = "REFC") +
          theme_bw() +
          ggtitle(out_title) +
          theme(axis.title = element_text(size = 12), axis.text = element_text(size = 12))
        exp(p, filename)
      }
      make_scatter("IPC_option2", "IPC2", "REFC_IPC2.jpeg")
      if (has_ic) make_scatter("IC", "IC", "REFC_IC.jpeg")
      
      # Bilans activités / pressions
      p_act  <- Stat_Ai_plot(res1, sub_lines, activity_names = activity_names)
      p_pres_list <- Stat_Pj_plot(res1, sub_lines)
      
    } else {
      p_act       <- Stat_Ai_plot_bis(res1, lines, activity_names = activity_names)
      p_pres_list <- list(N1 = NULL, N2 = NULL, N3 = Stat_Pj_plot_bis(res1, lines))
    }
    
    # Export bilans activités + CSV -------------
    exp(p_act, "Bilan_activites.jpeg")
    write.csv(p_act$data,  file.path(dir, "Bilan_activites.csv"),  row.names = FALSE)
    
    # Export bilans pressions par niveau + CSV -----------
    # p_pres_list est une liste nommée list(N1, N2, N3). Les niveaux vides
    # sont NULL et donc omis.
    for (lvl_nm in names(p_pres_list)) {
      p_lvl <- p_pres_list[[lvl_nm]]
      if (is.null(p_lvl)) next
      exp(p_lvl, paste0("Bilan_pressions_", lvl_nm, ".jpeg"))
      write.csv(p_lvl$data,
                file.path(dir, paste0("Bilan_pressions_", lvl_nm, ".csv")),
                row.names = FALSE)
    }
    
    # Radar IC (uniquement si les indices de confiance sont disponibles) ----
    has_ic_radar <- !is.null(res1$ctx$IC_global)  &&
      !is.null(res1$ctx$IQ_A_mean)  &&
      !is.null(res1$ctx$IC_AP_mean) &&
      !is.null(res1$ctx$IQ_H_mean)  &&
      !is.null(res1$ctx$IC_HP_mean)
    
    if (has_ic_radar) {
      f_radar_IC(res1, out_dir = dir)
    } else {
      message("  [skip] Radar_IC.jpeg : indices de confiance non disponibles dans res1$ctx.")
    }
    
    invisible(NULL)
  }
}
