# app.R - Interface Shiny pour configuration et lancement d'analyse PNM

# Etape 1 : Initialisation --------
{
  if (!requireNamespace("shiny", quietly = TRUE))    stop("Package manquant: shiny")
  if (!requireNamespace("bslib", quietly = TRUE))    stop("Package manquant: bslib")
  if (!requireNamespace("yaml", quietly = TRUE))     stop("Package manquant: yaml")
  if (!requireNamespace("DT", quietly = TRUE))       stop("Package manquant: DT")
  if (!requireNamespace("processx", quietly = TRUE)) stop("Package manquant: processx")
  if (!requireNamespace("here", quietly = TRUE))     stop("Package manquant: here")
  if (!requireNamespace("dplyr", quietly = TRUE))    stop("Package manquant: dplyr")
  
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  library(dplyr)
  
  source(here::here("engine", "load_config.R"),     chdir = TRUE)
  source(here::here("engine", "validate_config.R"), chdir = TRUE)
  source(here::here("engine", "io_paths.R"))
  
  config_template <- here::here("study", "config", "config.yml")
  runs_dir        <- here::here("study", "config", "runs")
  logs_dir        <- here::here("study", "outputs", "logs")
  
  dir.create(runs_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  
}

# Etape 2 : Fonctions utilitaires -------
{
  # Schéma de colonnes canonique pour rv$dsets --------
  DATASET_COLS_CANON <- c(
    "Table_code", "enabled", "Data_type", "Source",
    "DB_schema", "DB_table", "id_col", "geom_col",
    "srm_col", "surfmesh_col", "surfmer_col", "zone_col",
    "surfhab_col", "habcode_col", "intensity_cols"
  )
  
  # Convertit datasets df vers une forme éditable -------
  datasets_to_editable <- function(dsets_df) {
    d <- as.data.frame(dsets_df, stringsAsFactors = FALSE)
    
    # --- PATCH : canonicalisation des colonnes ---
    # as.data.frame() sur une liste de listes ne crée une colonne que si au
    # moins une entrée du YAML source la possède. En mode flat_files,
    # DB_schema/DB_table sont absents de toutes les entrées -> colonnes
    # absentes de d -> plantage plus loin (cell_edit avec j_glob vide,
    # rbind() avec nombre de colonnes différent lors de l'ajout d'un
    # dataset). On force donc systématiquement le même jeu de colonnes,
    # en les créant à NA_character_ si absentes.
    for (col in DATASET_COLS_CANON) {
      if (!col %in% names(d)) d[[col]] <- NA_character_
    }
    
    if (!"enabled" %in% names(d))        d$enabled        <- NA
    if (!"intensity_cols" %in% names(d)) d$intensity_cols <- NA_character_
    
    isA <- !is.na(d$Data_type) & d$Data_type == "A"
    
    if (any(isA) && is.list(d$intensity_cols)) {
      rows <- list()
      k <- 1
      for (i in which(isA)) {
        vals <- d$intensity_cols[[i]]
        if (is.null(vals) || length(vals) == 0) vals <- NA_character_
        vals <- as.character(vals)
        
        for (v in vals) {
          r <- d[i, , drop = FALSE]
          r$intensity_cols <- v
          rows[[k]] <- r
          k <- k + 1
        }
      }
      d_nonA <- d[!isA, , drop = FALSE]
      dA <- if (length(rows) > 0) do.call(rbind, rows) else d[0, , drop = FALSE]
      d <- rbind(d_nonA, dA)
    } else {
      d$intensity_cols <- as.character(d$intensity_cols)
    }
    
    rownames(d) <- NULL
    d
  }
  
  # Force enabled à TRUE pour grid uniquement -------
  # NOTE : règle réduite par rapport à la version précédente, qui forçait
  # également tous les datasets E et A. L'utilisateur conserve désormais
  # la liberté de désactiver tout dataset E ou A via la case enabled.
  force_enabled_rules <- function(d) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    if (!"enabled" %in% names(d)) d$enabled <- NA
    
    is_grid <- !is.na(d$Table_code) & !is.na(d$Data_type) &
      d$Table_code == "grid" & d$Data_type == "G"
    d$enabled[is_grid] <- TRUE
    
    d
  }
  
  # Écrit un YAML "run" à partir d'un cfg existant + datasets édités -------
  write_run_yaml <- function(cfg_base, dsets_edit, out_path, act_col_mapping = list()) {
    cfg <- cfg_base
    dsets_edit <- force_enabled_rules(dsets_edit)
    
    d      <- as.data.frame(dsets_edit, stringsAsFactors = FALSE)
    is_A   <- !is.na(d$Data_type) & d$Data_type == "A"
    d_nonA <- d[!is_A, , drop = FALSE]
    d_A    <- d[is_A,  , drop = FALSE]
    
    rows_nonA <- lapply(seq_len(nrow(d_nonA)), function(i) {
      row <- as.list(d_nonA[i, , drop = FALSE])
      row$activite_ref   <- NULL
      row$intensity_cols <- NULL
      row[!sapply(row, function(x) is.null(x) || (length(x) == 1 && is.na(x)))]
    })
    
    rows_A <- list()
    k <- 1
    for (tc in unique(d_A$Table_code)) {
      dd <- d_A[d_A$Table_code == tc, , drop = FALSE]
      
      ic <- unique(trimws(as.character(dd$intensity_cols)))
      ic <- ic[!is.na(ic) & nzchar(ic)]
      
      base <- as.list(dd[1, , drop = FALSE])
      base$intensity_cols <- NULL
      base$activite_ref   <- NULL
      base$Data_type      <- NULL
      base <- base[!sapply(base, function(x) is.null(x) || (length(x) == 1 && is.na(x)))]
      
      base$Data_type      <- "A"
      base$intensity_cols <- ic
      
      rows_A[[k]] <- base
      k <- k + 1
    }
    
    cfg$datasets <- c(rows_nonA, rows_A)
    
    if (length(act_col_mapping) > 0) {
      cfg$act_col_mapping <- act_col_mapping
    }
    
    # Nettoyage : supprimer la section legacy simul si elle existait
    cfg$simul <- NULL
    
    yaml::write_yaml(cfg, out_path)
    out_path
  }
  # Définition des facteurs pilotables pour la sensibilité formelle (Shapley) -----
  sensi_factor_defs <- list(
    X1_mat_AP       = c("Médiane"          = "2", "Précaution"        = "3", "Binaire"        = "4"),
    X2_relation_AP  = c("Linéaire"         = "2", "Optimiste"         = "3", "Pessimiste"     = "4", "Logistique" = "5"),
    X3_var_sensi    = c("Sans IC"          = "0", "Avec IC"           = "1"),
    X4_var_ccr_AP   = c("Sans IC"          = "0", "Avec IC"           = "1"),
    X5_var_act      = c("Sans IQ"          = "0", "Avec IQ"           = "1"),
    X6_typo_act     = c("Natif"            = "2", "Agrégé N2"         = "3", "Agrégé N1"      = "4"),
    X7_mat_sensi    = c("Précaution"       = "2", "Médiane"           = "3", "Binaire"        = "4"),
    X8_var_hab      = c("Sans perturbation" = "0", "Avec perturbation" = "1"),
    X9_norm_form    = c("Log"              = "2", "Linéaire"          = "3", "Sigmoïde"       = "4"),
    X10_agreg_press = c("Additif"          = "2", "Synergique"        = "3", "Antagoniste"    = "4")
  )
  
  sensi_factor_labels <- c(
    X1_mat_AP       = "X1 - Matrice activité-pression",
    X2_relation_AP  = "X2 - Relation activité-pression",
    X3_var_sensi    = "X3 - Variabilité sensibilité",
    X4_var_ccr_AP   = "X4 - Variabilité CCR activité-pression",
    X5_var_act      = "X5 - Variabilité activités",
    X6_typo_act     = "X6 - Granularité typologie activité",
    X7_mat_sensi    = "X7 - Matrice sensibilité",
    X8_var_hab      = "X8 - Perturbation surfaces habitats",
    X9_norm_form    = "X9 - Forme de normalisation",
    X10_agreg_press = "X10 - Agrégation des pressions"
  )
}

# Etape 3 : Code interface utilisateur --------
{
  ui <- bslib::page_navbar(
    title = "Analyse des Risques d'Effets Cumulés",
    theme = bslib::bs_theme(
      version    = 5,
      bootswatch = "flatly"
    ) %>% bslib::bs_add_rules("
  .navbar.navbar-expand-lg {
    background-color: #94d0db !important;
  }
  .navbar.navbar-expand-lg .navbar-brand,
  .navbar.navbar-expand-lg .nav-link {
    color: #ffffff !important;
  }
  .navbar.navbar-expand-lg .nav-link.active {
    font-weight: 600;
    border-bottom: 2px solid #ffffff;
  }
  .nav-underline .nav-link:not(.active) {
    color: #adb5bd !important;
  }
  .btn-run-analysis {
    background-color: #f4a6b5 !important;
    border-color: #f4a6b5 !important;
    color: #ffffff !important;
  }
  .btn-run-analysis:hover {
    background-color: #ee90a3 !important;
    border-color: #ee90a3 !important;
    color: #ffffff !important;
  }
  #ui_actv2_mapping_inputs .selectize-input,
  #ui_actv2_mapping_inputs .selectize-dropdown {
    width: 100% !important;
    min-width: 400px;
  }
  #ui_actv2_mapping_inputs td:last-child {
    width: 65% !important;
  }
  #ui_actv2_mapping_inputs .form-group {
    margin-bottom: 0;
  }
  #ui_actv2_mapping_inputs select {
    width: 100% !important;
  }
  .shiny-input-container {
    width: 100% !important;
    max-width: 100% !important;
  }
"),
    
    # Onglet 1: Projet & Base de données ------
    bslib::nav_panel(
      title = "Projet & DB",
      icon  = shiny::icon("database"),
      
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 350,
          
          bslib::card(
            bslib::card_header("Actions"),
            tags$style(HTML("#file_config_progress { display: none !important; }")),
            shiny::fileInput(
              "file_config",
              label       = NULL,
              accept      = c(".yml", ".yaml"),
              buttonLabel = "Charger config",
              placeholder = "config.yml",
              width       = "100%"
            ),
            shiny::actionButton(
              "btn_run",
              "Enregistrer et lancer l'analyse",
              icon  = shiny::icon("play"),
              class = "btn-run-analysis w-100 mb-2"
            ),
            shiny::actionButton(
              "open_report",
              "Ouvrir le rapport HTML",
              icon  = shiny::icon("file-lines"),
              class = "btn w-100",
              style = "background-color: #2c3e50 !important;
                       border-color: #2c3e50 !important;
                       color: #ffffff !important;"
            )
          ),
          
          bslib::card(
            bslib::card_header("Statut"),
            shiny::verbatimTextOutput("out_status")
          ),
          
          bslib::card(
            bslib::card_header("Fichiers"),
            shiny::verbatimTextOutput("out_paths", placeholder = TRUE)
          )
        ),
        
        bslib::navset_card_underline(
          
          bslib::nav_panel(
            "Projet",
            shiny::selectInput(
              "project_srm",
              "Région Maritime (SRM)",
              choices = c(
                "Méditerranée"       = "MED",
                "Golfe de Gascogne"  = "GG",
                "Mers Celtiques"     = "MC",
                "Manche Mer du Nord" = "MMN"
              ),
              selected = "MED"
            )
          ),
          
          bslib::nav_panel(
            "Base de données",
            shiny::selectInput(
              "db_data_source",
              "Source des données",
              choices = c(
                "Base de données PostgreSQL"       = "postgis",
                "Fichiers locaux (dossier inputs)" = "flat_files"
              ),
              selected = "postgis"
            ),
            shiny::conditionalPanel(
              condition = "input.db_data_source == 'postgis'",
              shiny::checkboxInput("db_enable_postgis", "Activer PostGIS", value = TRUE),
              shiny::selectInput(
                "db_choice", "Configuration DB",
                choices = c("local", "dist1"), selected = "local"
              ),
              shiny::textInput("db_host",         "Hôte",            value = "localhost"),
              shiny::numericInput("db_port",       "Port",            value = 5432, min = 1, max = 65535),
              shiny::textInput("db_dbname",        "Nom de la base",  value = ""),
              shiny::textInput("db_user",          "Utilisateur",     value = ""),
              shiny::passwordInput("db_password",  "Mot de passe"),
              shiny::helpText("Le mot de passe sera écrit dans le fichier YAML de configuration")
            ),
            shiny::conditionalPanel(
              condition = "input.db_data_source == 'flat_files'",
              shiny::helpText(
                "Déposez vos fichiers .gpkg dans le dossier study/inputs/,",
                "nommés selon le Table_code déclaré dans les datasets",
                "(ex: grid.gpkg, habbenth.gpkg, chalutage.gpkg...)."
              )
            )
          )
          
        )
      )
    ),
    
    # Onglet 2: Paramètres de calcul --------
    bslib::nav_panel(
      title = "Calculs",
      icon  = shiny::icon("calculator"),
      
      bslib::layout_columns(
        col_widths = c(6, 6),
        
        bslib::card(
          bslib::card_header("Fichiers d'entrée"),
          shiny::selectInput(
            "inputs_act_press", "Matrice liens activités-pressions",
            width   = "100%",
            choices = setNames(
              paste0("config/", list.files(here::here("study", "config"), pattern = "\\.ods$")),
              list.files(here::here("study", "config"), pattern = "\\.ods$")
            )
          ),
          shiny::selectInput(
            "inputs_sensi_hab_press", "Matrice sensibilité habitats-pressions",
            width   = "100%",
            choices = setNames(
              paste0("config/", list.files(here::here("study", "config"), pattern = "\\.ods$")),
              list.files(here::here("study", "config"), pattern = "\\.ods$")
            )
          )
        ),
        
        bslib::card(
          bslib::card_header("Paramètres généraux"),
          shiny::selectInput(
            "calc_agreg_sensib", "Agrégation sensibilité",
            choices  = c("Précaution" = 1, "Médiane" = 2),
            selected = 1
          ),
          shiny::numericInput(
            "calc_th_sensib", "Seuil % données manquantes acceptable",
            value = 20, min = 0, max = 100, step = 5
          ),
          shiny::checkboxInput(
            "calc_l_corr_surfhab", "Corriger les surfaces d'habitat",
            value = TRUE
          )
        )
      ),
      
      bslib::card(
        bslib::card_header("Unités de surface des données sources"),
        shiny::helpText(
          "Renseignez l'unité dans laquelle chaque champ surface est stocké en base. ",
          "Le moteur convertit ensuite vers l'unité cible via convert_area()."
        ),
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          shiny::selectInput(
            "units_target", "Unité cible (calculs internes)",
            choices = c("m2", "km2", "ha"), selected = "m2"
          )
        ),
        tags$hr(),
        tags$h6("Habitat benthique"),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::selectInput(
            "units_surfhab", "Surface d'habitat (E_surfhab)",
            choices = c("m2", "km2", "ha"), selected = "m2"
          ),
          shiny::selectInput(
            "units_surfmer", "Surface mer (E_surfmer)",
            choices = c("m2", "km2", "ha"), selected = "km2"
          )
        ),
        tags$h6("Grille"),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::selectInput(
            "units_grid_surfmesh", "Surface de maille (G_surfmesh)",
            choices = c("m2", "km2", "ha"), selected = "km2"
          ),
          shiny::selectInput(
            "units_grid_surfmer", "Surface mer (G_surfmer)",
            choices = c("m2", "km2", "ha"), selected = "km2"
          )
        )
      )
    ),
    
    # Onglet 3: Datasets --------
    bslib::nav_panel(
      title = "Datasets",
      icon  = shiny::icon("table"),
      
      bslib::card(
        bslib::card_header("Datasets Grille & Habitats (G / E)", class = "bg-primary text-white"),
        shiny::helpText("Datasets de type Grille (G) et Habitat benthique (E). Tous les champs sont éditables via double-clic. La case enabled de la grille principale est forcée à TRUE."),
        DT::DTOutput("tbl_datasets_ge"),
        bslib::card_footer(
          shiny::actionButton("btn_add_dataset_ge",    "Ajouter",                icon = shiny::icon("plus"),  class = "btn-sm btn-outline-primary"),
          shiny::actionButton("btn_remove_dataset_ge", "Supprimer la sélection", icon = shiny::icon("trash"), class = "btn-sm btn-outline-danger")
        )
      ),
      
      bslib::card(
        bslib::card_header("Datasets Activités (A)", class = "bg-primary text-white"),
        shiny::helpText("Une ligne par colonne d'intensité. Les champs Table_code / Source / DB_* sont répétés par dataset. Double-clic pour éditer."),
        DT::DTOutput("tbl_datasets_a"),
        bslib::card_footer(
          shiny::actionButton("btn_add_dataset_a",    "Ajouter un dataset A",   icon = shiny::icon("plus"),  class = "btn-sm btn-outline-primary"),
          shiny::actionButton("btn_remove_dataset_a", "Supprimer la sélection", icon = shiny::icon("trash"), class = "btn-sm btn-outline-danger")
        )
      )
    ),
    
    # Onglet 4: Monte-Carlo --------
    bslib::nav_panel(
      title = "Monte-Carlo",
      icon  = shiny::icon("dice"),
      
      bslib::layout_columns(
        col_widths = c(6, 6),
        fillable   = FALSE,
        
        # ---- Colonne gauche : Monte Carlo
        bslib::card(
          bslib::card_header("Tirages Monte Carlo"),
          shiny::checkboxInput(
            "mc_enabled", "Activer le module de simulations",
            value = TRUE
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::numericInput(
              "mc_nbsimul", "Nombre de simulations",
              value = 5, min = 1, max = 10000
            ),
            shiny::textInput(
              "mc_thres_stat", "Seuils d'identification des cellules à haut risque (%)",
              value = "10, 25"
            )
          ),
          tags$hr(),
          tags$h6("Facteurs d'incertitude"),
          shiny::helpText(
            "Facteurs testés lors des simulations Monte Carlo. ",
            "'Aléatoire' tire une modalité à chaque simulation."
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::selectInput(
              "factor_x1_mat_ap", "X1 - Matrice activité-pression",
              choices  = c("Aléatoire" = 1, "Médiane (ref)" = 2, "Précaution" = 3, "Binaire" = 4),
              selected = 2
            ),
            shiny::selectInput(
              "factor_x2_relation_ap", "X2 - Relation activité-pression",
              choices  = c("Aléatoire" = 1, "Linéaire (ref)" = 2, "Optimiste" = 3, "Pessimiste" = 4, "Logistique" = 5),
              selected = 2
            )
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::selectInput(
              "factor_x3_var_sensi", "X3 - Variabilité sensibilité",
              choices  = c("Sans IC (ref)" = 0, "Avec IC" = 1),
              selected = 0
            ),
            shiny::selectInput(
              "factor_x4_var_ccr_ap", "X4 - Variabilité CCR activité-pression",
              choices  = c("Sans IC (ref)" = 0, "Avec IC" = 1),
              selected = 0
            )
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::selectInput(
              "factor_x5_var_act", "X5 - Variabilité activités",
              choices  = c("Sans IQ (ref)" = 0, "Avec IQ" = 1),
              selected = 0
            ),
            shiny::selectInput(
              "factor_x6_typo_act", "X6 - Granularité typologie activité",
              choices  = c("Aléatoire" = 1, "Natif (ref)" = 2, "Agrégé N2" = 3, "Agrégé N1" = 4),
              selected = 2
            )
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::selectInput(
              "factor_x7_mat_sensi", "X7 - Matrice sensibilité",
              choices  = c("Aléatoire" = 1, "Précaution (ref)" = 2, "Médiane" = 3, "Binaire" = 4),
              selected = 2
            ),
            shiny::selectInput(
              "factor_x8_var_hab", "X8 - Perturbation surfaces habitats",
              choices  = c("Sans perturbation (ref)" = 0, "Avec perturbation" = 1),
              selected = 0
            )
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::selectInput(
              "factor_x9_norm_form", "X9 - Forme de normalisation",
              choices  = c("Aléatoire" = 1, "Log (ref)" = 2, "Linéaire" = 3, "Sigmoïde" = 4),
              selected = 2
            ),
            shiny::selectInput(
              "factor_x10_agreg_press", "X10 - Agrégation des pressions",
              choices  = c("Aléatoire" = 1, "Additif (ref)" = 2, "Synergique" = 3, "Antagoniste" = 4),
              selected = 2
            )
          )
        ),
        
        # ---- Colonne droite : Sensibilité + Robustesse
        shiny::tagList(
          
          bslib::card(
            bslib::card_header("Analyse de Sensibilité"),
            shiny::checkboxInput(
              "mod_sensibilite_enabled",
              "Activer le module d'analyse de sensibilité",
              value = FALSE
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::numericInput(
                "sensi_m", "Nombre de permutations (m)",
                value = 20, min = 1, max = 5000
              ),
              shiny::numericInput(
                "sensi_nrep", "Répétitions stochastiques (nrep)",
                value = 5, min = 1, max = 200
              )
            ),
            shiny::helpText(
              "Analyse de sensibilité globale par la méthode de Shapley sur le ",
              "classement spatial du risque. Nécessite au moins 2 facteurs actifs. "
            ),
            tags$hr(),
            tags$h6("Facteurs d'incertitude"),
            do.call(
              bslib::layout_columns,
              c(
                list(col_widths = rep(6, length(sensi_factor_defs))),
                lapply(names(sensi_factor_defs), function(fn) {
                  shiny::checkboxInput(
                    inputId = paste0("sensi_factor_", fn),
                    label   = sensi_factor_labels[[fn]],
                    value   = FALSE
                  )
                })
              )
            )
          ),
          
          bslib::card(
            bslib::card_header("Analyse de Robustesse"),
            shiny::checkboxInput(
              "mod_robustesse_det_enabled",
              "Activer le module d'analyse de robustesse déterministe",
              value = TRUE
            ),
            shiny::checkboxInput(
              "mod_robustesse_stoch_enabled",
              "Activer le module d'analyse de robustesse stochastique",
              value = TRUE
            ),
            shiny::helpText(
              "La robustesse déterministe teste tout les scénarios fixes (facteurs déterministes ; paramétrisation non nécessaire).",
              "La robustesse stochastique s'appuie sur les tirages Monte Carlo ci-dessus (facteurs stochastiques sélectionnés)."
            )
          )
        )
        
      )
    ),
    
    # Onglet 5: Outputs & Step3 --------
    bslib::nav_panel(
      title = "Outputs",
      icon  = shiny::icon("chart-line"),
      
      bslib::layout_columns(
        col_widths = c(6, 6),
        
        bslib::card(
          bslib::card_header("Paramètres généraux"),
          shiny::checkboxInput("outputs_write_maps",     "Écrire les cartes",        FALSE),
          shiny::checkboxInput("outputs_export_details", "Exporter détails analyse", FALSE),
          shiny::conditionalPanel(
            condition = "input.db_data_source == 'postgis'",
            shiny::checkboxInput("outputs_export_db",         "Exporter vers DB",    FALSE),
            shiny::checkboxInput("outputs_export_db_details", "Exporter détails DB", FALSE)
          )
        ),
        
        bslib::card(
          bslib::card_header("Export cartes détaillées"),
          shiny::checkboxInput("export_maps_ai",     "Export maps Ai",     TRUE),
          shiny::checkboxInput("export_maps_pj",     "Export maps Pj",     TRUE),
          shiny::checkboxInput("export_maps_rex_pj", "Export maps REX Pj", TRUE)
        )
      ),
      
      shiny::conditionalPanel(
        condition = "input.db_data_source == 'postgis'",
        bslib::card(
          bslib::card_header("Export Monte Carlo vers DB"),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::checkboxInput("export_simul",  "Exporter chaque simulation",    value = TRUE),
            shiny::checkboxInput("export_result", "Exporter statistiques finales", value = TRUE)
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::textInput("export_scheme", "Schéma d'export PostgreSQL", value = "resultats"),
            shiny::selectInput(
              "db_choice_export", "Choix DB export",
              choices = c("local", "dist1"), selected = "local"
            )
          )
        )
      ),
      
      bslib::card(
        bslib::card_header("Production Graphique"),
        bslib::layout_columns(
          col_widths = c(4, 8),
          shiny::checkboxInput("step3_enabled", "Activer le module de production des graphiques", TRUE),
          shiny::selectInput(
            "step3_mode", "Mode d'analyse",
            choices  = c("Simplifié" = "simplifie", "Complet" = "complet"),
            selected = "simplifie"
          )
        ),
        shiny::conditionalPanel(
          condition = "input.step3_mode == 'complet'",
          shiny::textInput("step3_zone_field", "Champ zone (requis en mode complet)", value = "zone")
        )
      ),
      
      bslib::card(
        bslib::card_header("Rapport HTML"),
        shiny::checkboxInput("report_enabled", "Générer le rapport HTML global", value = TRUE)
      )
    ),
    
    # Onglet 6: Logs & Exécution --------
    bslib::nav_panel(
      title = "Logs",
      icon  = shiny::icon("terminal"),
      
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Journal d'exécution", class = "bg-dark text-white"),
        shiny::verbatimTextOutput("out_log", placeholder = TRUE),
        bslib::card_footer(
          shiny::actionButton("btn_refresh_log", "Rafraîchir",
                              icon = shiny::icon("sync"), class = "btn-sm btn-outline-secondary"),
          shiny::downloadButton("btn_download_log", "Télécharger le log",
                                class = "btn-sm btn-outline-primary")
        )
      )
    )
    
  )
}

# Etape 4 : Code serveur --------
server <- function(input, output, session) {
  
  # Valeurs réactives ------
  rv <- shiny::reactiveValues(
    cfg_base          = NULL,
    dsets             = NULL,
    act_col_mapping   = list(),
    cfg_run_path      = NULL,
    log_path          = NULL,
    p                 = NULL,
    status            = "IDLE",
    last_update       = Sys.time(),
    finished_notified = TRUE
  )
  
  active_config      <- shiny::reactiveVal(config_template)
  active_config_name <- shiny::reactiveVal(basename(config_template))
  
  # Signal de fin --------
  shiny::observe({
    shiny::invalidateLater(1000, session)
    if (!is.null(rv$p) && !is.null(rv$status) && rv$status == "RUNNING") {
      if (!rv$p$is_alive()) {
        code      <- rv$p$get_exit_status()
        rv$status <- if (isTRUE(code == 0)) "DONE" else "ERROR"
        if (code == 0) {
          shiny::showNotification("Analyse terminée avec succès.", type = "message", duration = 10)
        } else {
          shiny::showNotification(
            paste0("Analyse terminée avec erreur (code: ", code, "). Consultez le log."),
            type = "error", duration = 12
          )
        }
      }
    }
  })
  
  # Chargement configuration ----------
  load_all <- function() {
    tryCatch({
      cfg <- load_config(active_config())
      validate_config(cfg)
      
      # Projet --------
      shiny::updateSelectInput(session, "project_srm", selected = cfg$project$srm %||% "MED")
      
      # DB --------
      shiny::updateCheckboxInput(session, "db_enable_postgis", value = isTRUE(cfg$db$enable_postgis))
      shiny::updateSelectInput(session,   "db_choice",         selected = cfg$db$choice %||% "local")
      shiny::updateSelectInput(session,   "db_data_source",    selected = cfg$db$data_source %||% "postgis")
      
      db_block <- if ((cfg$db$choice %||% "local") == "local") cfg$DBConnectlocal else cfg$DBConnectdist1
      shiny::updateTextInput(session,    "db_host",     value = as.character(db_block$host     %||% ""))
      shiny::updateNumericInput(session, "db_port",     value = as.integer(db_block$port       %||% 5432))
      shiny::updateTextInput(session,    "db_dbname",   value = as.character(db_block$dbname   %||% ""))
      shiny::updateTextInput(session,    "db_user",     value = as.character(db_block$user     %||% ""))
      shiny::updateTextInput(session,    "db_password", value = as.character(db_block$password %||% ""))
      
      # Calc --------
      shiny::updateSelectInput(session,   "calc_agreg_sensib",   selected = cfg$Calc$agreg_sensib   %||% 1)
      shiny::updateNumericInput(session,  "calc_th_sensib",      value    = cfg$Calc$th_sensib      %||% 20)
      shiny::updateCheckboxInput(session, "calc_l_corr_surfhab", value    = isTRUE(cfg$Calc$l_corr_surfhab))
      
      # Inputs ODS --------
      shiny::updateSelectInput(session, "inputs_act_press",       selected = cfg$inputs$ods$act_press       %||% "")
      shiny::updateSelectInput(session, "inputs_sensi_hab_press", selected = cfg$inputs$ods$sensi_hab_press %||% "")
      
      # Units --------
      shiny::updateSelectInput(session, "units_target",
                               selected = cfg$units$areas$target %||% "m2")
      shiny::updateSelectInput(session, "units_surfhab",
                               selected = cfg$units$areas$HabBenth_z$E_surfhab %||% "m2")
      shiny::updateSelectInput(session, "units_surfmer",
                               selected = cfg$units$areas$HabBenth_z$E_surfmer %||% "km2")
      shiny::updateSelectInput(session, "units_grid_surfmesh",
                               selected = cfg$units$areas$Grid_z$G_surfmesh %||% "km2")
      shiny::updateSelectInput(session, "units_grid_surfmer",
                               selected = cfg$units$areas$Grid_z$G_surfmer %||% "km2")
      
      # Modules d'analyse --------
      mc <- cfg$analyses$monte_carlo %||% list()
      shiny::updateCheckboxInput(session, "mc_enabled", value = isTRUE(mc$enabled))
      shiny::updateCheckboxInput(session, "mod_robustesse_det_enabled",
                                 value = isTRUE(cfg$analyses$robustesse_deterministe$enabled))
      shiny::updateCheckboxInput(session, "mod_robustesse_stoch_enabled",
                                 value = isTRUE(cfg$analyses$robustesse_stoch$enabled))
      shiny::updateCheckboxInput(session, "mod_sensibilite_enabled",
                                 value = isTRUE(cfg$analyses$sensibilite_formelle$enabled))
      
      # Monte Carlo : simulation --------
      shiny::updateNumericInput(session, "mc_nbsimul",    value = as.integer(mc$nbsimul %||% 5))
      shiny::updateTextInput(session,    "mc_thres_stat", value = paste(mc$thres_stat %||% c(10, 25), collapse = ", "))
      
      # Monte Carlo : facteurs --------
      factors <- mc$factors %||% list()
      shiny::updateSelectInput(session, "factor_x1_mat_ap",      selected = factors$X1_mat_AP      %||% 2)
      shiny::updateSelectInput(session, "factor_x2_relation_ap", selected = factors$X2_relation_AP %||% 2)
      shiny::updateSelectInput(session, "factor_x3_var_sensi",   selected = factors$X3_var_sensi   %||% 0)
      shiny::updateSelectInput(session, "factor_x4_var_ccr_ap",  selected = factors$X4_var_ccr_AP  %||% 0)
      shiny::updateSelectInput(session, "factor_x5_var_act",     selected = factors$X5_var_act     %||% 0)
      shiny::updateSelectInput(session, "factor_x6_typo_act",    selected = factors$X6_typo_act    %||% 2)
      shiny::updateSelectInput(session, "factor_x7_mat_sensi",   selected = factors$X7_mat_sensi   %||% 2)
      shiny::updateSelectInput(session, "factor_x8_var_hab",      selected = factors$X8_var_hab      %||% 0)
      shiny::updateSelectInput(session, "factor_x9_norm_form",    selected = factors$X9_norm_form    %||% 2)
      shiny::updateSelectInput(session, "factor_x10_agreg_press", selected = factors$X10_agreg_press %||% 2)
      
      # Sensibilité formelle (Shapley) --------
      sf_cfg <- cfg$analyses$sensibilite_formelle %||% list()
      shiny::updateNumericInput(session, "sensi_m",    value = as.integer(sf_cfg$m    %||% 20))
      shiny::updateNumericInput(session, "sensi_nrep", value = as.integer(sf_cfg$nrep %||% 5))
      
      sf_facteurs <- sf_cfg$facteurs %||% list()
      for (fn in names(sensi_factor_defs)) {
        est_actif <- !is.null(sf_facteurs[[fn]]) && length(sf_facteurs[[fn]]) > 0
        shiny::updateCheckboxInput(session, paste0("sensi_factor_", fn), value = est_actif)
      }
      
      # Monte Carlo : export DB --------
      shiny::updateCheckboxInput(session, "export_simul",   value = isTRUE(mc$export_simul))
      shiny::updateCheckboxInput(session, "export_result",  value = isTRUE(mc$export_result))
      shiny::updateTextInput(session,    "export_scheme",   value = mc$export_scheme %||% "resultats")
      shiny::updateSelectInput(session,  "db_choice_export", selected = mc$db_choice %||% "local")
      
      # Outputs --------
      shiny::updateCheckboxInput(session, "outputs_write_maps",        value = isTRUE(cfg$Outputs$write_maps))
      shiny::updateCheckboxInput(session, "outputs_export_details",    value = isTRUE(cfg$Outputs$export_details))
      shiny::updateCheckboxInput(session, "outputs_export_db",         value = isTRUE(cfg$Outputs$export_db))
      shiny::updateCheckboxInput(session, "outputs_export_db_details", value = isTRUE(cfg$Outputs$export_db_details))
      shiny::updateCheckboxInput(session, "export_maps_ai",            value = isTRUE(cfg$Outputs$export_maps_Ai))
      shiny::updateCheckboxInput(session, "export_maps_pj",            value = isTRUE(cfg$Outputs$export_maps_Pj))
      shiny::updateCheckboxInput(session, "export_maps_rex_pj",        value = isTRUE(cfg$Outputs$export_maps_REX_Pj))
      
      # Step 3 --------
      shiny::updateCheckboxInput(session, "step3_enabled",    value    = isTRUE(cfg$Step3$enabled))
      shiny::updateSelectInput(session,   "step3_mode",       selected = cfg$Step3$mode       %||% "simplifie")
      shiny::updateTextInput(session,     "step3_zone_field", value    = cfg$Step3$zone_field %||% "zone")
      
      # Report --------
      shiny::updateCheckboxInput(session, "report_enabled", value = isTRUE(cfg$report$enabled))
      
      # Datasets --------
      rv$cfg_base <- cfg
      rv$dsets    <- datasets_to_editable(cfg$datasets)
      
      if (!is.null(cfg$act_col_mapping)) {
        rv$act_col_mapping <- as.list(cfg$act_col_mapping)
      }
      
      rv$status <- "Configuration chargée"
      
    }, error = function(e) {
      rv$status <- paste("Erreur:", e$message)
      shiny::showNotification(paste("Erreur:", e$message), type = "error", duration = 10)
    })
  }
  
  shiny::observeEvent(TRUE, { load_all() }, once = TRUE)
  
  shiny::observeEvent(input$file_config, {
    req(input$file_config)
    active_config(input$file_config$datapath)
    active_config_name(input$file_config$name)
    load_all()
    shiny::showNotification(
      paste("Configuration chargée :", input$file_config$name),
      type = "message"
    )
  })
  
  # DATASETS — réactifs partagés -------------
  dsets_ge <- shiny::reactive({
    req(rv$dsets)
    d    <- force_enabled_rules(rv$dsets)
    d_ge <- d[!is.na(d$Data_type) & d$Data_type %in% c("G", "E"), ]
    cols <- c("Table_code","enabled","Data_type","Source","DB_schema",
              "DB_table","id_col","geom_col","srm_col","surfmesh_col",
              "surfmer_col","zone_col","surfhab_col","habcode_col")
    d_ge[, intersect(cols, names(d_ge)), drop = FALSE]
  })
  
  dsets_a <- shiny::reactive({
    req(rv$dsets)
    d   <- force_enabled_rules(rv$dsets)
    d_a <- d[!is.na(d$Data_type) & d$Data_type == "A", ]
    if (!"activite_ref" %in% names(d_a)) d_a$activite_ref <- NA_character_
    for (i in seq_len(nrow(d_a))) {
      col <- d_a$intensity_cols[i]
      if (!is.na(col) && nzchar(col) && col %in% names(rv$act_col_mapping)) {
        if (is.na(d_a$activite_ref[i]) || !nzchar(d_a$activite_ref[i])) {
          d_a$activite_ref[i] <- rv$act_col_mapping[[col]]
        }
      }
    }
    cols <- c("Table_code","enabled","Source","DB_schema","DB_table",
              "id_col","geom_col","intensity_cols","activite_ref")
    d_a[, intersect(cols, names(d_a)), drop = FALSE]
  })
  
  activities_choices <- shiny::reactive({
    req(input$inputs_act_press)
    ods_path <- here::here("study", input$inputs_act_press)
    if (!file.exists(ods_path)) return(character(0))
    tryCatch({
      df_ods <- readODS::read_ods(ods_path, sheet = "typologie_activites", col_names = TRUE)
      df_ods <- df_ods[!is.na(df_ods$Code_A) & !is.na(df_ods$Nom_activite), ]
      setNames(
        trimws(as.character(df_ods$Code_A)),
        paste0(trimws(df_ods$Nom_activite), " (", trimws(df_ods$Code_A), ")")
      )
    }, error = function(e) character(0))
  })
  
  # TABLEAU G/E ----------------------
  output$tbl_datasets_ge <- DT::renderDT({
    req(dsets_ge())
    d <- dsets_ge()
    
    # Colonnes à afficher selon le mode
    if (!is.null(input$db_data_source) && input$db_data_source == "flat_files") {
      cols_show <- c("Table_code", "enabled", "Data_type",
                     "id_col", "geom_col", "srm_col", "surfmesh_col",
                     "surfmer_col", "zone_col", "surfhab_col", "habcode_col")
    } else {
      cols_show <- c("Table_code", "enabled", "Data_type",
                     "DB_schema", "DB_table",
                     "id_col", "geom_col", "srm_col", "surfmesh_col",
                     "surfmer_col", "zone_col", "surfhab_col", "habcode_col")
    }
    
    d <- d[, intersect(cols_show, names(d)), drop = FALSE]
    
    DT::datatable(
      d,
      rownames  = FALSE,
      editable  = list(target = "cell"),
      selection = "multiple",
      options   = list(pageLength = 10, scrollX = TRUE, dom = 'rtip'),
      class     = "display compact"
    )
  })
  
  shiny::observeEvent(input$tbl_datasets_ge_cell_edit, {
    info <- input$tbl_datasets_ge_cell_edit
    req(rv$dsets)
    
    d      <- rv$dsets
    idx_ge <- which(!is.na(d$Data_type) & d$Data_type %in% c("G", "E"))
    i_glob <- idx_ge[info$row]
    
    if (!is.null(input$db_data_source) && input$db_data_source == "flat_files") {
      cols_ge <- c("Table_code", "enabled", "Data_type",
                   "id_col", "geom_col", "srm_col", "surfmesh_col",
                   "surfmer_col", "zone_col", "surfhab_col", "habcode_col")
    } else {
      cols_ge <- c("Table_code", "enabled", "Data_type",
                   "DB_schema", "DB_table",
                   "id_col", "geom_col", "srm_col", "surfmesh_col",
                   "surfmer_col", "zone_col", "surfhab_col", "habcode_col")
    }
    
    j_name <- cols_ge[info$col + 1]
    j_glob <- which(names(d) == j_name)
    
    if (length(j_glob) == 0 || length(i_glob) == 0) return()
    
    if (identical(j_name, "enabled")) {
      vv <- tolower(trimws(as.character(info$value)))
      d[i_glob, j_glob] <- vv %in% c("true","1","yes","y","TRUE")
    } else {
      d[i_glob, j_glob] <- info$value
    }
    rv$dsets  <- d
    rv$status <- "Dataset modifié (non enregistré)"
  })
  
  shiny::observeEvent(input$btn_add_dataset_ge, {
    shiny::showModal(shiny::modalDialog(
      title = "Ajouter un dataset Grille / Habitat",
      shiny::selectInput("new_ge_type",   "Type",         choices = c("G","E"), selected = "G"),
      shiny::textInput("new_ge_code",     "Table_code",   value = "nouveau_ge"),
      shiny::textInput("new_ge_schema",   "DB_schema",    value = ""),
      shiny::textInput("new_ge_table",    "DB_table",     value = ""),
      shiny::textInput("new_ge_id",       "id_col",       value = "id"),
      shiny::textInput("new_ge_geom",     "geom_col",     value = "geom"),
      shiny::textInput("new_ge_srm",      "srm_col",      value = ""),
      shiny::textInput("new_ge_surfmesh", "surfmesh_col", value = ""),
      shiny::textInput("new_ge_surfmer",  "surfmer_col",  value = ""),
      shiny::conditionalPanel(
        condition = "input.new_ge_type == 'G'",
        shiny::textInput("new_ge_zone", "zone_col", value = "")
      ),
      shiny::conditionalPanel(
        condition = "input.new_ge_type == 'E'",
        shiny::textInput("new_ge_surfhab",  "surfhab_col",  value = ""),
        shiny::textInput("new_ge_habcode",  "habcode_col",  value = "")
      ),
      footer = shiny::tagList(
        shiny::modalButton("Annuler"),
        shiny::actionButton("btn_confirm_add_ge", "Ajouter", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })
  
  shiny::observeEvent(input$btn_confirm_add_ge, {
    new_row <- data.frame(
      Table_code     = input$new_ge_code,
      enabled        = TRUE,
      Data_type      = input$new_ge_type,
      Source         = "postgis",
      DB_schema      = input$new_ge_schema,
      DB_table       = input$new_ge_table,
      id_col         = input$new_ge_id,
      geom_col       = input$new_ge_geom,
      srm_col        = if (nzchar(input$new_ge_srm      %||% "")) input$new_ge_srm      else NA_character_,
      surfmesh_col   = if (nzchar(input$new_ge_surfmesh %||% "")) input$new_ge_surfmesh else NA_character_,
      surfmer_col    = if (nzchar(input$new_ge_surfmer  %||% "")) input$new_ge_surfmer  else NA_character_,
      zone_col       = if (input$new_ge_type == "G" && nzchar(input$new_ge_zone     %||% "")) input$new_ge_zone     else NA_character_,
      surfhab_col    = if (input$new_ge_type == "E" && nzchar(input$new_ge_surfhab  %||% "")) input$new_ge_surfhab  else NA_character_,
      habcode_col    = if (input$new_ge_type == "E" && nzchar(input$new_ge_habcode  %||% "")) input$new_ge_habcode  else NA_character_,
      intensity_cols = NA_character_,
      activite_ref   = NA_character_,
      stringsAsFactors = FALSE
    )
    rv$dsets <- rbind(rv$dsets, new_row)
    shiny::removeModal()
    shiny::showNotification("Dataset G/E ajouté", type = "message")
  })
  
  shiny::observeEvent(input$btn_remove_dataset_ge, {
    rows_sel <- input$tbl_datasets_ge_rows_selected
    if (length(rows_sel) == 0) {
      shiny::showNotification("Aucune ligne sélectionnée", type = "warning"); return()
    }
    d      <- rv$dsets
    idx_ge <- which(!is.na(d$Data_type) & d$Data_type %in% c("G","E"))
    to_rm  <- idx_ge[rows_sel]
    rv$dsets <- d[-to_rm, , drop = FALSE]
    shiny::showNotification(paste("Suppression de", length(rows_sel), "dataset(s)"), type = "message")
  })
  
  # TABLEAU A — DT éditable + colonne activite_ref en select JS ----------
  output$tbl_datasets_a <- DT::renderDT({
    req(dsets_a())
    acts <- activities_choices()
    d    <- dsets_a()
    
    # Colonnes à afficher selon le mode
    if (!is.null(input$db_data_source) && input$db_data_source == "flat_files") {
      cols_show <- c("Table_code", "enabled",
                     "id_col", "geom_col", "intensity_cols", "activite_ref")
    } else {
      cols_show <- c("Table_code", "enabled",
                     "DB_schema", "DB_table",
                     "id_col", "geom_col", "intensity_cols", "activite_ref")
    }
    
    d <- d[, intersect(cols_show, names(d)), drop = FALSE]
    
    col_idx_ref <- which(names(d) == "activite_ref") - 1
    
    opts_list <- c(list(`-- Choisir --` = ""), as.list(acts))
    opts_json <- jsonlite::toJSON(opts_list, auto_unbox = TRUE)
    
    DT::datatable(
      d,
      rownames  = FALSE,
      selection = "multiple",
      editable  = list(
        target  = "cell",
        disable = list(columns = col_idx_ref)
      ),
      options = list(
        paging         = FALSE,
        scrollY        = "60vh",
        scrollX        = TRUE,
        scrollCollapse = TRUE,
        dom            = 'rt',
        drawCallback = DT::JS(sprintf("
function(settings) {
  var api     = this.api();
  var colIdx  = %d;
  var choices = %s;

  api.rows({ page: 'current' }).every(function(rowIdx) {
    var cell   = api.cell(rowIdx, colIdx);
    var td     = $(cell.node());
    var curVal = (cell.data() === null || cell.data() === undefined)
                   ? '' : String(cell.data());

    if (td.find('.selectize-control').length > 0) {
      var existing = td.find('.dt-act-select');
      if (existing.length > 0 && existing[0].selectize) {
      existing[0].selectize.destroy();
      }
    td.empty();
    }

    var sel = $('<select class=\"dt-act-select\" style=\"width:100%%;\"></select>');
    $.each(choices, function(label, val) {
      var opt = $('<option></option>').val(val).text(label);
      if (val === curVal) opt.prop('selected', true);
      sel.append(opt);
    });

    td.empty().append(sel);

    sel.selectize({
      create:      false,
      sortField:   'text',
      placeholder: '-- Rechercher une activité --',
      onChange: function(newVal) {
        if (!newVal) return;
        cell.data(newVal);
        Shiny.setInputValue(
          'tbl_datasets_a_select_edit',
          { row: rowIdx + 1, col: colIdx, value: newVal },
          { priority: 'event' }
        );
      }
    });
  });
}
", col_idx_ref, opts_json))
      ),
      class = "display compact"
    )
  })
  
  shiny::observeEvent(input$tbl_datasets_a_cell_edit, {
    info <- input$tbl_datasets_a_cell_edit
    req(rv$dsets)
    
    d      <- rv$dsets
    idx_a  <- which(!is.na(d$Data_type) & d$Data_type == "A")
    if (!is.null(input$db_data_source) && input$db_data_source == "flat_files") {
      cols_a <- c("Table_code", "enabled",
                  "id_col", "geom_col", "intensity_cols", "activite_ref")
    } else {
      cols_a <- c("Table_code", "enabled",
                  "DB_schema", "DB_table",
                  "id_col", "geom_col", "intensity_cols", "activite_ref")
    }
    
    j_name <- cols_a[info$col + 1]
    
    if (identical(j_name, "activite_ref")) return()
    
    i_glob <- idx_a[info$row]
    j_glob <- which(names(d) == j_name)
    if (length(j_glob) == 0 || length(i_glob) == 0) return()
    
    if (identical(j_name, "enabled")) {
      vv <- tolower(trimws(as.character(info$value)))
      d[i_glob, j_glob] <- vv %in% c("true","1","yes","y","TRUE")
    } else if (identical(j_name, "Table_code")) {
      old_tc <- rv$dsets$Table_code[i_glob]
      same   <- which(!is.na(d$Data_type) & d$Data_type == "A" & d$Table_code == old_tc)
      d$Table_code[same] <- info$value
    } else {
      d[i_glob, j_glob] <- info$value
    }
    rv$dsets  <- d
    rv$status <- "Dataset modifié (non enregistré)"
  })
  
  shiny::observeEvent(input$tbl_datasets_a_select_edit, {
    info <- input$tbl_datasets_a_select_edit
    req(rv$dsets)
    
    d      <- rv$dsets
    idx_a  <- which(!is.na(d$Data_type) & d$Data_type == "A")
    i_glob <- idx_a[info$row]
    if (length(i_glob) == 0) return()
    
    if (!"activite_ref" %in% names(d)) d$activite_ref <- NA_character_
    d$activite_ref[i_glob] <- info$value
    
    col_name <- d$intensity_cols[i_glob]
    if (!is.null(col_name) && !is.na(col_name) && nzchar(col_name)) {
      rv$act_col_mapping[[col_name]] <- info$value
    }
    
    rv$dsets  <- d
    rv$status <- "Mapping modifié (non enregistré)"
  })
  
  shiny::observeEvent(input$btn_add_dataset_a, {
    shiny::showModal(shiny::modalDialog(
      title = "Ajouter un dataset Activité (A)",
      shiny::textInput("new_a_code",   "Table_code", value = "nouveau_act"),
      shiny::textInput("new_a_schema", "DB_schema",  value = ""),
      shiny::textInput("new_a_table",  "DB_table",   value = ""),
      shiny::textInput("new_a_id",     "id_col",     value = "id"),
      shiny::textInput("new_a_geom",   "geom_col",   value = "geom"),
      shiny::textInput("new_a_icols",  "Colonnes d'intensité (séparées par des virgules)",
                       placeholder = "ex: i_col1, i_col2, i_col3"),
      shiny::helpText("Une ligne sera créée dans le tableau pour chaque colonne d'intensité."),
      footer = shiny::tagList(
        shiny::modalButton("Annuler"),
        shiny::actionButton("btn_confirm_add_a", "Ajouter", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })
  
  shiny::observeEvent(input$btn_confirm_add_a, {
    tryCatch({
      
      ic_cols <- trimws(strsplit(input$new_a_icols %||% "", ",")[[1]])
      ic_cols <- ic_cols[nzchar(ic_cols)]
      
      if (length(ic_cols) == 0) {
        shiny::showNotification("Renseignez au moins une colonne d'intensité.", type = "warning")
        return()
      }
      
      new_rows <- data.frame(
        Table_code     = input$new_a_code,
        enabled        = TRUE,
        Data_type      = "A",
        Source         = "postgis",
        DB_schema      = input$new_a_schema,
        DB_table       = input$new_a_table,
        id_col         = input$new_a_id,
        geom_col       = input$new_a_geom,
        srm_col        = NA_character_,
        surfmesh_col   = NA_character_,
        surfmer_col    = NA_character_,
        zone_col       = NA_character_,
        surfhab_col    = NA_character_,
        habcode_col    = NA_character_,
        intensity_cols = ic_cols,
        activite_ref   = NA_character_,
        stringsAsFactors = FALSE
      )
      
      rv$dsets <- rbind(rv$dsets, new_rows)
      shiny::removeModal()
      shiny::showNotification(
        paste0("Dataset '", input$new_a_code, "' ajouté (", length(ic_cols), " colonne(s))"),
        type = "message"
      )
    }, error = function(e) {
      shiny::showNotification(
        paste("Erreur ajout dataset A :", e$message),
        type = "error", duration = 15
      )
      message("Erreur ajout dataset A : ", e$message)
    })
  })
  
  shiny::observeEvent(input$btn_remove_dataset_a, {
    rows_sel <- input$tbl_datasets_a_rows_selected
    if (length(rows_sel) == 0) {
      shiny::showNotification("Aucune ligne sélectionnée", type = "warning")
      return()
    }
    d     <- rv$dsets
    idx_a <- which(!is.na(d$Data_type) & d$Data_type == "A")
    to_rm <- idx_a[rows_sel]
    
    # Nettoyage du mapping pour les colonnes supprimées
    removed_cols <- d$intensity_cols[to_rm]
    removed_cols <- removed_cols[!is.na(removed_cols) & nzchar(removed_cols)]
    for (col in removed_cols) {
      rv$act_col_mapping[[col]] <- NULL
    }
    
    removed_info <- paste0(d$Table_code[to_rm], " / ", d$intensity_cols[to_rm])
    rv$dsets <- d[-to_rm, , drop = FALSE]
    
    shiny::showNotification(
      paste0("Ligne(s) supprimée(s) : ", paste(removed_info, collapse = " ; ")),
      type = "message"
    )
  })
  
  # Synchronisation act_col_mapping -> rv$dsets$activite_ref
  shiny::observe({
    req(rv$dsets)
    d <- rv$dsets
    if (!"activite_ref" %in% names(d)) d$activite_ref <- NA_character_
    idx_a   <- which(!is.na(d$Data_type) & d$Data_type == "A")
    changed <- FALSE
    for (i in idx_a) {
      col <- d$intensity_cols[i]
      if (!is.na(col) && nzchar(col) && col %in% names(rv$act_col_mapping)) {
        new_val <- rv$act_col_mapping[[col]]
        if (is.na(d$activite_ref[i]) || d$activite_ref[i] != new_val) {
          d$activite_ref[i] <- new_val
          changed <- TRUE
        }
      }
    }
    if (changed) rv$dsets <- d
  })
  
  # Configuration courante (reactive) -------------------
  current_cfg <- shiny::reactive({
    req(rv$cfg_base)
    cfg <- rv$cfg_base
    cfg$Outputs$verbose_maps <- FALSE
    
    # Projet --------
    cfg$project$srm        <- input$project_srm
    
    # DB --------
    cfg$db$enable_postgis  <- isTRUE(input$db_enable_postgis)
    cfg$db$choice          <- input$db_choice
    cfg$db$data_source     <- input$db_data_source
    
    blk <- list(
      drv      = "RPostgres",
      host     = input$db_host,
      port     = as.integer(input$db_port),
      dbname   = input$db_dbname,
      user     = input$db_user,
      password = input$db_password
    )
    if (input$db_choice == "local") cfg$DBConnectlocal <- blk else cfg$DBConnectdist1 <- blk
    
    # Calc --------
    cfg$Calc$agreg_sensib   <- as.integer(input$calc_agreg_sensib)
    cfg$Calc$th_sensib      <- as.numeric(input$calc_th_sensib)
    cfg$Calc$l_corr_surfhab <- isTRUE(input$calc_l_corr_surfhab)
    
    # Inputs ODS --------
    cfg$inputs$ods$act_press       <- input$inputs_act_press
    cfg$inputs$ods$sensi_hab_press <- input$inputs_sensi_hab_press
    
    # Units --------
    cfg$units$areas$target               <- input$units_target
    cfg$units$areas$HabBenth_z$E_surfhab <- input$units_surfhab
    cfg$units$areas$HabBenth_z$E_surfmer <- input$units_surfmer
    cfg$units$areas$Grid_z$G_surfmesh    <- input$units_grid_surfmesh
    cfg$units$areas$Grid_z$G_surfmer     <- input$units_grid_surfmer
    
    # Modules d'analyse --------
    if (is.null(cfg$analyses)) cfg$analyses <- list()
    if (is.null(cfg$analyses$monte_carlo)) cfg$analyses$monte_carlo <- list()
    
    cfg$analyses$monte_carlo$enabled <- isTRUE(input$mc_enabled)
    
    # Monte Carlo : simulation --------
    thres_vec <- as.numeric(strsplit(gsub("\\s","", input$mc_thres_stat), ",")[[1]])
    cfg$analyses$monte_carlo$nbsimul    <- as.integer(input$mc_nbsimul)
    cfg$analyses$monte_carlo$thres_stat <- thres_vec
    
    # Monte Carlo : facteurs --------
    cfg$analyses$monte_carlo$factors <- list(
      X1_mat_AP       = as.integer(input$factor_x1_mat_ap),
      X2_relation_AP  = as.integer(input$factor_x2_relation_ap),
      X3_var_sensi    = as.integer(input$factor_x3_var_sensi),
      X4_var_ccr_AP   = as.integer(input$factor_x4_var_ccr_ap),
      X5_var_act      = as.integer(input$factor_x5_var_act),
      X6_typo_act     = as.integer(input$factor_x6_typo_act),
      X7_mat_sensi    = as.integer(input$factor_x7_mat_sensi),
      X8_var_hab      = as.integer(input$factor_x8_var_hab),
      X9_norm_form    = as.integer(input$factor_x9_norm_form),
      X10_agreg_press = as.integer(input$factor_x10_agreg_press)
    )
    
    # Monte Carlo : export DB --------
    if (input$db_data_source == "flat_files") {
      cfg$analyses$monte_carlo$export_simul  <- FALSE
      cfg$analyses$monte_carlo$export_result <- FALSE
      cfg$Outputs$export_db                  <- FALSE
      cfg$Outputs$export_db_details          <- FALSE
    } else {
      cfg$analyses$monte_carlo$export_simul  <- isTRUE(input$export_simul)
      cfg$analyses$monte_carlo$export_result <- isTRUE(input$export_result)
      cfg$Outputs$export_db                  <- isTRUE(input$outputs_export_db)
      cfg$Outputs$export_db_details          <- isTRUE(input$outputs_export_db_details)
    }
    
    # Sensibilité formelle (Shapley) --------
    sf <- cfg$analyses$sensibilite_formelle %||% list()
    shiny::updateNumericInput(session, "sensi_m",    value = as.integer(sf$m    %||% 20))
    shiny::updateNumericInput(session, "sensi_nrep", value = as.integer(sf$nrep %||% 5))
    
    sf_facteurs <- sf$facteurs %||% list()
    for (fn in names(sensi_factor_defs)) {
      est_actif <- !is.null(sf_facteurs[[fn]]) && length(sf_facteurs[[fn]]) > 0
      shiny::updateCheckboxInput(session, paste0("sensi_factor_", fn), value = est_actif)
    }
    
    # Monte Carlo : sauvegarde / reprise --------
    # Champs non exposés dans l'UI. Si le YAML chargé contient des valeurs
    # personnalisées (utilisateur averti modifiant à la main), elles sont
    # préservées. Sinon, on applique les défauts ci-dessous.
    sr_defaults <- list(
      lrepr_activite     = FALSE,
      lsauv_activite     = TRUE,
      lrepr_iq           = FALSE,
      lsauv_iq           = TRUE,
      lrepr_hab_sensib   = FALSE,
      lsauv_hab_sensib   = TRUE,
      lrepr_grid_z       = FALSE,
      lsauv_grid_z       = TRUE,
      daterepr           = "",
      daterepr_activite  = "",
      daterepr_iq        = "",
      daterepr_hab_sensib = "",
      daterepr_grid_z    = ""
    )
    for (k in names(sr_defaults)) {
      if (is.null(cfg$analyses$monte_carlo[[k]])) {
        cfg$analyses$monte_carlo[[k]] <- sr_defaults[[k]]
      }
    }
    
    # Autres modules : on touche uniquement au flag enabled
    if (is.null(cfg$analyses$robustesse_deterministe)) cfg$analyses$robustesse_deterministe <- list()
    cfg$analyses$robustesse_deterministe$enabled <- isTRUE(input$mod_robustesse_det_enabled)
    
    if (is.null(cfg$analyses$robustesse_stoch)) cfg$analyses$robustesse_stoch <- list()
    cfg$analyses$robustesse_stoch$enabled <- isTRUE(input$mod_robustesse_stoch_enabled)
    
    if (is.null(cfg$analyses$sensibilite_formelle)) cfg$analyses$sensibilite_formelle <- list()
    cfg$analyses$sensibilite_formelle$enabled <- isTRUE(input$mod_sensibilite_enabled)
    cfg$analyses$sensibilite_formelle$methode <- "shapley"
    cfg$analyses$sensibilite_formelle$m       <- as.integer(input$sensi_m)
    cfg$analyses$sensibilite_formelle$nrep    <- as.integer(input$sensi_nrep)
    
    sensi_facteurs_sel <- list()
    for (fn in names(sensi_factor_defs)) {
      if (isTRUE(input[[paste0("sensi_factor_", fn)]])) {
        # Toutes les modalités concrètes du facteur
        sensi_facteurs_sel[[fn]] <- as.integer(unname(sensi_factor_defs[[fn]]))
      }
    }
    cfg$analyses$sensibilite_formelle$facteurs <- sensi_facteurs_sel
    
    # Outputs --------
    cfg$Outputs$write_maps         <- isTRUE(input$outputs_write_maps)
    cfg$Outputs$export_details     <- isTRUE(input$outputs_export_details)
    cfg$Outputs$export_maps_Ai     <- isTRUE(input$export_maps_ai)
    cfg$Outputs$export_maps_Pj     <- isTRUE(input$export_maps_pj)
    cfg$Outputs$export_maps_REX_Pj <- isTRUE(input$export_maps_rex_pj)
    
    # Step 3 --------
    cfg$Step3$enabled    <- isTRUE(input$step3_enabled)
    cfg$Step3$mode       <- input$step3_mode
    cfg$Step3$zone_field <- input$step3_zone_field
    
    # Report --------
    if (is.null(cfg$report)) cfg$report <- list()
    cfg$report$enabled <- isTRUE(input$report_enabled)
    
    # Nettoyage section legacy
    cfg$simul <- NULL
    
    cfg
  })
  
  # Bouton enregistrer + lancer ----------
  shiny::observeEvent(input$btn_run, {
    req(rv$dsets)
    
    if (input$db_data_source == "postgis" && !nzchar(trimws(input$db_password))) {
      shiny::showNotification(
        "Mot de passe DB vide. Renseignez-le avant de lancer l'analyse.",
        type = "error", duration = 8
      )
      return()
    }
    
    if (isTRUE(input$mod_sensibilite_enabled)) {
      n_facteurs_actifs <- sum(vapply(names(sensi_factor_defs), function(fn) {
        isTRUE(input[[paste0("sensi_factor_", fn)]])
      }, logical(1)))
      
      if (n_facteurs_actifs < 2) {
        shiny::showNotification(
          paste(
            "Analyse de sensibilité de Shapley activée : sélectionnez au moins 2",
            "facteurs (avec au moins une modalité chacun) avant de lancer."
          ),
          type = "error", duration = 8
        )
        return()
      }
    }
    
    # Etape A : construire et écrire la configuration (avec diagnostic) --------
    out_path <- NULL
    cfg      <- NULL
    cfg_ok   <- FALSE
    
    # A.1 : construction de l'objet cfg en mémoire
    cfg <- tryCatch({
      current_cfg()
    }, error = function(e) {
      rv$status <- paste("Erreur [A.1 current_cfg] :", e$message)
      shiny::showNotification(paste("Erreur [A.1] :", e$message), type = "error", duration = 12)
      NULL
    })
    if (is.null(cfg)) return()
    
    # A.2 : écriture du YAML sur disque
    out_path <- tryCatch({
      ts  <- format(Sys.time(), "%Y%m%d_%H%M%S")
      p   <- file.path(runs_dir, paste0("config_", ts, ".yml"))
      write_run_yaml(cfg, rv$dsets, p, act_col_mapping = rv$act_col_mapping)
      p
    }, error = function(e) {
      rv$status <- paste("Erreur [A.2 write_run_yaml] :", e$message)
      shiny::showNotification(paste("Erreur [A.2] :", e$message), type = "error", duration = 12)
      NULL
    })
    if (is.null(out_path)) return()
    
    rv$cfg_run_path <- out_path
    
    # A.3 : relecture du YAML écrit
    cfg_written <- tryCatch({
      load_config(out_path)
    }, error = function(e) {
      rv$status <- paste("Erreur [A.3 load_config relecture] :", e$message)
      shiny::showNotification(paste("Erreur [A.3] :", e$message), type = "error", duration = 12)
      NULL
    })
    if (is.null(cfg_written)) return()
    
    # A.4 : validation
    val_ok <- tryCatch({
      validate_config(cfg_written)
      TRUE
    }, error = function(e) {
      rv$status <- paste("Erreur [A.4 validate_config] :", e$message)
      shiny::showNotification(paste("Erreur [A.4] :", e$message), type = "error", duration = 12)
      FALSE
    })
    if (!isTRUE(val_ok)) return()
    
    rv$status      <- "Configuration enregistrée et validée"
    rv$last_update <- Sys.time()
    shiny::showNotification(
      paste("Configuration enregistrée :", basename(out_path)),
      type = "message", duration = 4
    )
    cfg_ok <- TRUE
    
    # Etape B : lancer l'analyse --------
    tryCatch({
      ts          <- format(Sys.time(), "%Y%m%d_%H%M%S")
      log_path    <- file.path(logs_dir, paste0("run_", ts, ".log"))
      rv$log_path <- log_path
      rv$status   <- "RUNNING"
      
      rv$p <- processx::process$new(
        command = "Rscript",
        args = c(
          "--vanilla",
          here::here("execution", "run_batch.R"),
          normalizePath(rv$cfg_run_path, winslash = "/", mustWork = TRUE)
        ),
        wd        = here::here(),
        stdout    = rv$log_path,
        stderr    = rv$log_path,
        supervise = TRUE
      )
      rv$finished_notified <- FALSE
      shiny::showNotification(
        "Analyse lancée. Consultez l'onglet Logs pour suivre la progression.",
        type = "message", duration = 5
      )
    }, error = function(e) {
      rv$status <- paste("Erreur lancement :", e$message)
      shiny::showNotification(paste("Erreur :", e$message), type = "error", duration = 10)
    })
  })
  
  # Outputs texte / log -------------------------
  output$out_paths <- shiny::renderText({
    paste0(
      "Config active   : ", active_config_name(), "\n",
      "Dernière config : ", if (!is.null(rv$cfg_run_path)) basename(rv$cfg_run_path) else "(aucune)", "\n",
      "Dernier log     : ", if (!is.null(rv$log_path))     basename(rv$log_path)     else "(aucun)",  "\n",
      "Mise à jour     : ", format(rv$last_update, "%Y-%m-%d %H:%M:%S")
    )
  })
  
  output$out_status <- shiny::renderText({
    rv$last_update
    if (is.null(rv$p)) return(rv$status %||% "En attente")
    if (rv$p$is_alive()) { rv$status <- "Analyse en cours..."; return(rv$status) }
    
    code <- rv$p$get_exit_status()
    rv$status <- if (isTRUE(code == 0)) "Analyse terminée avec succès"
    else paste0("Analyse terminée avec erreur (code: ", code, ")")
    
    if (!isTRUE(rv$finished_notified)) {
      rv$finished_notified <- TRUE
      if (code == 0) shiny::showNotification("Analyse terminée avec succès.", type = "message", duration = 10)
      else           shiny::showNotification("Analyse terminée avec erreur. Consultez les logs.", type = "error", duration = 10)
    }
    rv$status
  })
  
  output$out_log <- shiny::renderText({
    rv$last_update
    if (is.null(rv$log_path) || !file.exists(rv$log_path))
      return("Aucun log disponible. Lancez une analyse pour générer des logs.")
    tryCatch({
      x <- readLines(rv$log_path, warn = FALSE)
      if (length(x) == 0) return("Log vide (analyse en cours de démarrage...)")
      paste(tail(x, 500), collapse = "\n")
    }, error = function(e) paste("Erreur lecture log :", e$message))
  })
  
  shiny::observeEvent(input$btn_refresh_log, { rv$last_update <- Sys.time() })
  
  output$btn_download_log <- shiny::downloadHandler(
    filename = function() {
      if (!is.null(rv$log_path)) basename(rv$log_path)
      else paste0("log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
    },
    content = function(file) {
      if (!is.null(rv$log_path) && file.exists(rv$log_path)) file.copy(rv$log_path, file)
      else writeLines("Aucun log disponible", file)
    }
  )
  
  shiny::observe({
    shiny::invalidateLater(2000, session)
    if (!is.null(rv$p) && rv$p$is_alive()) rv$last_update <- Sys.time()
  })
  
  shiny::observeEvent(input$open_report, {
    runs       <- list.dirs(here::here("study", "outputs"), recursive = FALSE, full.names = TRUE)
    info_files <- file.path(runs, "run_info.json")
    info_files <- info_files[file.exists(info_files)]
    
    if (length(info_files) == 0) {
      shiny::showNotification("Aucun rapport disponible pour le moment.", type = "warning", duration = 5)
      return()
    }
    
    info_file <- info_files[which.max(file.info(info_files)$mtime)]
    info      <- jsonlite::read_json(info_file, simplifyVector = TRUE)
    
    report_html <- if (is.null(info$report_html)) "" else as.character(info$report_html)
    
    if (!nzchar(report_html) || !file.exists(report_html)) {
      shiny::showNotification("Le fichier report.html n'est pas encore disponible.", type = "warning", duration = 5)
      return()
    }
    
    utils::browseURL(report_html)
  })
}

shiny::shinyApp(ui, server)
