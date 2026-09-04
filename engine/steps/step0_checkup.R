# engine/steps/step0_checkup.R
# Vérifie l'environnement (dossiers, connexion DB)
# Importe/valide les tables déclarées dans config$datasets (postgis ou flat_files)
# avec option de reprise depuis un cache et remontée structurée des problèmes détectés

step0_checkup <- function(config, paths, con) {
  
  # ---- Guards --------------------------------------------------------------
  
  data_source <- tolower(trimws(config$db$data_source %||% "postgis"))
  mode_flat   <- identical(data_source, "flat_files")
  
  if (!mode_flat && (is.null(con) || !DBI::dbIsValid(con))) {
    stop("step0_checkup: 'con' est NULL ou invalide. Créez-le via get_db_conn(config, ...) et passez-le ici.")
  }
  
  if (is.null(config$datasets) || !is.data.frame(config$datasets)) {
    stop("step0_checkup: config$datasets doit être un data.frame (issu de load_config).")
  }
  
  # ---- Helpers -------------------------------------------------------------
  # Quote SQL identifiers safely (schema/table)
  if (!mode_flat) {
    q_ident <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
    
    table_has_geometry <- function(schema, table) {
      qry <- paste0(
        "SELECT 1 FROM information_schema.columns ",
        "WHERE table_schema = ", DBI::dbQuoteString(con, schema),
        " AND table_name = ",  DBI::dbQuoteString(con, table),
        " AND udt_name = 'geometry' ",
        "LIMIT 1;"
      )
      nrow(DBI::dbGetQuery(con, qry)) > 0
    }
    
    read_pg_table <- function(schema, table) {
      if (table_has_geometry(schema, table)) {
        sql <- paste0("SELECT * FROM ", q_ident(schema), ".", q_ident(table))
        sf::st_read(con, query = sql, quiet = TRUE)
      } else {
        DBI::dbReadTable(con, DBI::Id(schema = schema, table = table))
      }
    }
  }
  
  # ---- Sorties structurées -------------------------------------------------
  problemes <- list(
    chemins_acces = list(),
    schemas_tables_champs = list(),
    donnees_geographiques = list()
  )
  
  verif <- list(
    project_root = paths$project_root,
    study_dir    = paths$study_dir,
    config_dir   = paths$config_dir,
    outputs_dir  = paths$outputs_dir,
    cache_dir    = paths$cache_dir,
    sig_dir      = paths$sig_dir
  )
  
  # ---- Check dossiers clés -------------------------------------------------
  must_exist_dirs <- c(
    paths$project_root, paths$engine_dir, paths$study_dir,
    paths$config_dir, paths$outputs_dir, paths$cache_dir
  )
  for (d in must_exist_dirs) {
    if (!dir.exists(d)) problemes$chemins_acces[[d]] <- paste("Dossier manquant:", d)
  }
  
  # ---- Reprise / sauvegarde des tables importées ---------------------------
  lrepr_DBtables <- as_logical01(cfg_get(config, c("SauvRep","lrepr_DBtables"), 0), default = FALSE)
  lsauv_DBtables <- as_logical01(cfg_get(config, c("SauvRep","lsauv_DBtables"), 0), default = FALSE)
  daterepr       <- cfg_get(config, c("SauvRep","daterepr"), "")
  
  datesauv <- format(Sys.time(), "%Y-%m-%d_%H-%M")
  list_sauv <- NULL
  
  if (lrepr_DBtables) {
    if (is.null(daterepr) || identical(daterepr, "")) {
      problemes$donnees_geographiques[["Reprise_tables"]] <- "Reprise demandée (lrepr_DBtables=1) mais daterepr est vide."
      lrepr_DBtables <- FALSE
    } else {
      filerepr <- file.path(paths$cache_dir, paste0("init_tables_", daterepr, ".Rdata"))
      if (file.exists(filerepr)) {
        load(filerepr)  # doit créer list_sauv
        if (is.null(list_sauv) || !is.list(list_sauv)) {
          problemes$donnees_geographiques[["Reprise_tables"]] <- paste("Fichier de reprise lu mais 'list_sauv' introuvable:", filerepr)
          lrepr_DBtables <- FALSE
          list_sauv <- NULL
        }
      } else {
        problemes$donnees_geographiques[["Reprise_tables"]] <- paste("Fichier de reprise introuvable:", filerepr)
        lrepr_DBtables <- FALSE
      }
    }
  }
  
  # ---- Import des tables décrites dans datasets ----------------------------
  if (!lrepr_DBtables) {
    list_sauv <- list()
    
    df <- as.data.frame(config$datasets, stringsAsFactors = FALSE)
    act_rows <- df[df$Data_type == "A", , drop = FALSE] # vérifier unicité des tables activités
    if (mode_flat) {
      act_keys <- unique(act_rows[, c("Table_code", "Source"), drop = FALSE])
    } else {
      act_keys <- unique(act_rows[, c("Table_code", "Source", "DB_schema", "DB_table"), drop = FALSE])
    }
    dup <- act_keys$Table_code[duplicated(act_keys$Table_code)]
    
    if (length(dup) > 0) {
      stop(
        "step0_checkup: plusieurs tables activités utilisent le même Table_code: ",
        paste(unique(dup), collapse = ", "),
        ". Chaque table activité doit avoir un Table_code unique."
      )
    }
    
    # Colonnes minimales attendues pour l'import
    needed_import <- if (mode_flat) {
      c("Table_code", "Data_type", "Source")
    } else {
      c("Table_code", "Data_type", "Source", "DB_table")
    }
    missing_import <- setdiff(needed_import, names(df))
    if (length(missing_import) > 0) {
      stop("step0_checkup: datasets colonnes manquantes pour import: ", paste(missing_import, collapse = ", "))
    }
    
    # DB_schema requis uniquement pour postgis
    if (!mode_flat && "Source" %in% names(df)) {
      is_pg <- norm_source(df$Source) == "postgis"
      if (any(is_pg, na.rm = TRUE) && !"DB_schema" %in% names(df)) {
        stop("step0_checkup: colonne requise manquante: DB_schema (nécessaire pour Source=postgis).")
      }
    }
    
    # Déduplication "une table importée une seule fois"
    if (mode_flat) {
      df_unique_tables <- df |>
        dplyr::group_by(.data$Table_code, .data$Source, .data$Data_type) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
    } else {
      df_unique_tables <- df |>
        dplyr::group_by(.data$DB_table, .data$Source, .data$Table_code, .data$Data_type, dplyr::across(dplyr::any_of("DB_schema"))) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
    }
    
    for (i in seq_len(nrow(df_unique_tables))) {
      row_data <- df_unique_tables[i, ]
      
      if ("enabled" %in% names(row_data)) {
        enabled_flag <- as_logical01(row_data$enabled, default = TRUE)
        if (isFALSE(enabled_flag)) {
          message("[step0] dataset ignoré (enabled = FALSE): ", as.character(row_data$Table_code))
          next
        }
      }
      
      table_code <- as.character(row_data$Table_code)
      data_type  <- as.character(row_data$Data_type)
      source     <- norm_source(row_data$Source)
      
      base_name <- paste0(data_type, "_", table_code)
      obj_name  <- base_name
      
      temp_obj <- NULL
      
      if (identical(source, "postgis")) {
        
        if (mode_flat) {
          flat_path <- file.path(paths$inputs_dir, paste0(table_code, ".gpkg"))
          if (!file.exists(flat_path)) {
            flat_path <- file.path(paths$inputs_dir, paste0(table_code, ".shp"))
          }
          if (!file.exists(flat_path)) {
            problemes$donnees_geographiques[[obj_name]] <- paste(
              "Mode flat_files: fichier introuvable pour", table_code,
              "dans", paths$inputs_dir,
              "(cherché: .gpkg puis .shp)"
            )
            temp_obj <- NULL
          } else {
            temp_obj <- tryCatch(
              sf::st_read(flat_path, quiet = TRUE),
              error = function(e) {
                problemes$donnees_geographiques[[obj_name]] <- paste(
                  "Import flat_files échoué:", flat_path, "-", conditionMessage(e)
                )
                NULL
              }
            )
          }
          
        } else {
          schema <- as.character(row_data$DB_schema)
          table  <- as.character(row_data$DB_table)
          
          temp_obj <- tryCatch(
            read_pg_table(schema, table),
            error = function(e) {
              problemes$donnees_geographiques[[obj_name]] <- paste(
                "Import postgis échoué:", schema, table, "-", conditionMessage(e)
              )
              NULL
            }
          )
        }
        
      } else if (identical(source, "shp")) {
        shp_path <- file.path(paths$sig_dir, as.character(row_data$DB_table))
        temp_obj <- tryCatch(
          sf::st_read(shp_path, quiet = TRUE),
          error = function(e) {
            problemes$donnees_geographiques[[obj_name]] <- paste(
              "Import SHP échoué:", shp_path, "-", conditionMessage(e)
            )
            NULL
          }
        )
        
      } else if (identical(source, "gpkg")) {
        parts <- strsplit(as.character(row_data$DB_table), "::", fixed = TRUE)[[1]]
        if (length(parts) != 2) {
          problemes$donnees_geographiques[[obj_name]] <- "Format GPKG attendu: 'fichier.gpkg::layer'"
          temp_obj <- NULL
        } else {
          gpkg_path <- file.path(paths$sig_dir, parts[1])
          layer     <- parts[2]
          temp_obj <- tryCatch(
            sf::st_read(dsn = gpkg_path, layer = layer, quiet = TRUE),
            error = function(e) {
              problemes$donnees_geographiques[[obj_name]] <- paste(
                "Import GPKG échoué:", gpkg_path, "::", layer, "-", conditionMessage(e)
              )
              NULL
            }
          )
        }
        
      } else {
        problemes$donnees_geographiques[[obj_name]] <- paste("Source non supportée:", as.character(row_data$Source))
        temp_obj <- NULL
      }
      
      list_sauv[[obj_name]] <- temp_obj
    }
    
    if (lsauv_DBtables) {
      filesauv <- file.path(paths$cache_dir, paste0("init_tables_", datesauv, ".Rdata"))
      try(save(list_sauv, file = filesauv), silent = TRUE)
    }
  }
  
  # ---- Checks colonnes déclarées (si Field est présent) --------------------
  df2 <- unique(as.data.frame(config$datasets, stringsAsFactors = FALSE))
  
  # Cohérence DB_choice (optionnel)
  if ("DB_choice" %in% names(df2) && !is.null(config$db$choice)) {
    cfg_choice <- tolower(trimws(as.character(config$db$choice)))
    dset_choice <- tolower(trimws(as.character(df2$DB_choice)))
    bad <- which(!is.na(dset_choice) & nzchar(dset_choice) & dset_choice != cfg_choice)
    if (length(bad) > 0) {
      problemes$schemas_tables_champs[["DB_choice"]] <-
        "Certains datasets ont DB_choice différent de db.choice, mais step0 utilise une seule connexion."
    }
  }
  
  # Vérifier l'existence des champs uniquement si la colonne Field existe
  if (!mode_flat && "Field" %in% names(df2)) {
    
    needed_cols <- c("Source","DB_schema","DB_table","Field")
    missing_cols <- setdiff(needed_cols, names(df2))
    if (length(missing_cols) > 0) {
      stop("step0_checkup: datasets colonnes manquantes pour check Field: ", paste(missing_cols, collapse = ", "))
    }
    
    for (i in seq_len(nrow(df2))) {
      rowdata <- df2[i, ]
      if (!identical(norm_source(rowdata$Source), "postgis")) next
      
      schema <- as.character(rowdata$DB_schema)
      table  <- as.character(rowdata$DB_table)
      field  <- as.character(rowdata$Field)
      
      qry <- paste0(
        "SELECT 1 FROM information_schema.columns ",
        "WHERE table_schema = ", DBI::dbQuoteString(con, schema),
        " AND table_name = ",  DBI::dbQuoteString(con, table),
        " AND column_name = ", DBI::dbQuoteString(con, field),
        " LIMIT 1;"
      )
      
      res <- tryCatch(DBI::dbGetQuery(con, qry), error = function(e) data.frame())
      if (nrow(res) == 0) {
        probleme <- paste(schema, table, field, sep = ".")
        problemes$schemas_tables_champs[[probleme]] <- paste("La donnée", probleme, "n'existe pas (ou droits insuffisants).")
      }
    }
  }
  
  list(
    verif = verif,
    problemes = problemes,
    list_sauv = list_sauv
  )
}
