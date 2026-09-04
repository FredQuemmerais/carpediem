# engine/steps/step3_graphiques.R
# Production des graphs de synthèse de l'analyse
# Dépends de engine/functions/f_aux_step3.R
#
#   # Analyse globale uniquement
#   res3 <- quick_graphics(res1)
#
#   # Analyse globale + zonale
#   res3 <- generate_carpediem_graphics(
#     res1           = res1,
#     zone_analysis  = TRUE,
#     zone_field     = "zone")

#  Helpers internes ---------

#' Affiche un message d'étape formaté
.log_step <- function(n, label) {
  message(sprintf("\n[ÉTAPE %s] %s", n, toupper(label)))
}

#' Récupère le vecteur de zones depuis res1, ou NULL si introuvable
.get_zones_data <- function(res1, zone_field) {
  if (zone_field %in% colnames(res1$grid_final))
    return(res1$grid_final[[zone_field]])
  
  if (!is.null(res1$ctx$grid_z) && zone_field %in% colnames(res1$ctx$grid_z))
    return(res1$ctx$grid_z[[zone_field]])
  
  warning("Le champ de zone '", zone_field, "' est introuvable. Analyse par zone ignorée.")
  NULL
}

#' Filtre les catégories de zones ayant au moins `min_cells` mailles
.filter_categories <- function(zones_data, min_cells = 10) {
  categories <- unique(zones_data[!is.na(zones_data) & zones_data != ""])
  valid <- Filter(function(cat) sum(zones_data == cat, na.rm = TRUE) >= min_cells,
                  categories)
  skipped <- setdiff(categories, valid)
  if (length(skipped) > 0)
    message("  [skip] Catégories ignorées (< ", min_cells, " mailles) : ",
            paste(skipped, collapse = ", "))
  valid
}

#  Graphiques par zone -------
#' @param res1        Résultat de l'étape 1
#' @param zones_data  Vecteur de codes de zones (une valeur par maille)
#' @param output_dir  Dossier racine de sortie
#' @return Liste nommée des dossiers créés (une entrée par zone)
.run_zone_analysis <- function(res1, zones_data, output_dir) {
  
  categories <- .filter_categories(zones_data)
  
  if (length(categories) == 0) {
    message("  Aucune catégorie de zone valide trouvée.")
    return(invisible(list()))
  }
  
  message(sprintf("  %d catégorie(s) : %s",
                  length(categories), paste(categories, collapse = ", ")))
  
  zone_paths <- lapply(categories, function(cat) {
    cat_clean <- gsub("[^[:alnum:]_-]", "_", cat)
    cat_dir   <- file.path(output_dir, cat_clean)
    lines_cat <- which(zones_data == cat)
    
    message(sprintf("\n  -- Zone '%s' (%d mailles)", cat, length(lines_cat)))
    graph_maker(res1, lines_cat, cat_dir,
                title_suffix = paste0("(Zone: ", cat, ")"))
    cat_dir
  })
  names(zone_paths) <- gsub("[^[:alnum:]_-]", "_", categories)
  
  # Graphiques comparatifs multi-zones
  if (length(categories) > 1) {
    message("\n  -- Graphiques comparatifs inter-zones")
    export_graph(f_compare(res1$data_index, zones_data, categories),
                 output_dir, "index_compare.jpeg")
    export_graph(f_compare2(res1$data_index, zones_data, categories),
                 output_dir, "REFC_IPC2_par_zone.jpeg")
  }
  
  zone_paths
}

#  Graphiques globaux + par zone) -------
#' @param res1          Résultat de step1_analyse_simple
#' @param output_dir    Dossier de sortie principal (NULL = res1$paths$outputsdir)
#' @param zone_analysis Activer l'analyse par zones (TRUE/FALSE)
#' @param zone_field    Nom du champ de zones dans la grille (si zone_analysis = TRUE)
#' @return Liste invisible des chemins de sortie : $global et éventuellement $zones
generate_carpediem_graphics <- function(res1,
                                        output_dir   = NULL,
                                        zone_analysis = FALSE,
                                        zone_field    = NULL) {
  
  # Initialisation ------
  if (is.null(output_dir))
    output_dir <- res1$paths$outputsdir
  
  graphics_dir <- file.path(output_dir, "graphs")
  dir.create(graphics_dir, recursive = TRUE, showWarnings = FALSE)
  
  message("╔══════════════════════════════════════════════════════╗")
  message("║   CARPEDIEM – ÉTAPE 3 : PRODUCTION DE GRAPHIQUES     ║")
  message("╚══════════════════════════════════════════════════════╝")
  message(sprintf("  Dossier de sortie : %s", graphics_dir))
  message(sprintf("  Mailles           : %d", nrow(res1$data_index)))
  message(sprintf("  Activités         : %d", ncol(res1$activites) - 1))
  message(sprintf("  Pressions         : %d", ncol(res1$pressions_norm) - 1))
  
  output_paths <- list()
  
  # 1. Analyse globale -------
  .log_step(1, "Graphiques – analyse globale")
  graph_maker(res1, seq_len(nrow(res1$data_index)), graphics_dir)
  output_paths$global <- graphics_dir
  message(sprintf("  ✓ Graphiques globaux → %s", graphics_dir))
  
  # 2. Analyse par zones (optionnelle) -------
  if (isTRUE(zone_analysis) && !is.null(zone_field)) {
    .log_step(2, "Graphiques – analyse par zones")
    
    zones_data <- .get_zones_data(res1, zone_field)
    
    if (!is.null(zones_data)) {
      output_paths$zones <- .run_zone_analysis(res1, zones_data, graphics_dir)
      message(sprintf("  ✓ %d zone(s) analysée(s)", length(output_paths$zones)))
    }
  }
  
  # ── Résumé ──────────────────────────────────────────────────────────────────
  .log_step("✓", "Production de graphiques terminée")
  message(sprintf("  Analyse globale : %s", output_paths$global))
  if (!is.null(output_paths$zones)) {
    for (nm in names(output_paths$zones))
      message(sprintf("  Zone %-20s : %s", nm, output_paths$zones[[nm]]))
  }
  
  invisible(output_paths)
}

#  Version simplifiee -----------

#' Raccourci : génère uniquement les graphiques globaux
#' @param res1 Résultat de step1_analyse_simple
#' @return Chemins des graphiques générés (invisiblement)
quick_graphics <- function(res1) {
  generate_carpediem_graphics(res1, zone_analysis = FALSE)
}
