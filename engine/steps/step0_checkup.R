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
      err_msg  <- NULL

      if (identical(source, "postgis") && !mode_flat) {
        schema <- as.character(row_data$DB_schema)
        table  <- as.character(row_data$DB_table)
        temp_obj <- tryCatch(
          read_pg_table(schema, table),
          error = function(e) {
            err_msg <<- paste("Import postgis échoué:", schema, table, "-", conditionMessage(e))
            NULL
          }
        )
      } else {
        # Fichiers locaux : mode flat_files, ou Source = gpkg / shp
        spec <- resolve_flat_dataset(row_data, paths$inputs_dir)
        if (!is.null(spec$error)) {
          err_msg <- spec$error
        } else {
          temp_obj <- tryCatch(
            if (is.null(spec$layer)) sf::st_read(spec$path, quiet = TRUE)
            else sf::st_read(spec$path, layer = spec$layer, quiet = TRUE),
            error = function(e) {
              err_msg <<- paste("Import échoué:", spec$label, "-", conditionMessage(e))
              NULL
            }
          )
          if (!is.null(temp_obj)) message("[step0] ", obj_name, " <- ", spec$label)
        }
      }

      if (!is.null(err_msg)) problemes$donnees_geographiques[[obj_name]] <- err_msg

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

# Résout le fichier (et la couche) d'un dataset lu depuis des fichiers locaux.
# Ordre de priorité :
#   1. clés explicites `file` (relatif au dossier inputs) et `layer` (optionnelle) ;
#   2. Source = gpkg avec DB_table = "fichier.gpkg::couche", ou Source = shp avec DB_table = "fichier.shp" ;
#   3. par défaut : <Table_code>.gpkg, puis <Table_code>.shp.
# Pour un GeoPackage sans couche précisée : couche unique, sinon couche nommée comme le Table_code,
# sinon erreur (plus de lecture silencieuse de la première couche).
resolve_flat_dataset <- function(row_data, inputs_dir) {
  val <- function(col) {
    if (!col %in% names(row_data)) return(NULL)
    v <- as.character(row_data[[col]][[1]])
    if (length(v) == 0 || is.na(v) || !nzchar(trimws(v))) NULL else trimws(v)
  }
  table_code <- as.character(row_data$Table_code)
  source     <- norm_source(row_data$Source)
  file  <- val("file")
  layer <- val("layer")

  if (is.null(file) && identical(source, "gpkg") && !is.null(val("DB_table"))) {
    parts <- strsplit(val("DB_table"), "::", fixed = TRUE)[[1]]
    file  <- parts[1]
    if (length(parts) >= 2 && is.null(layer)) layer <- parts[2]
  }
  if (is.null(file) && identical(source, "shp") && !is.null(val("DB_table"))) {
    file <- val("DB_table")
  }

  if (is.null(file)) {
    candidates <- file.path(inputs_dir, paste0(table_code, c(".gpkg", ".shp")))
    path <- candidates[file.exists(candidates)][1]
    if (is.na(path)) {
      return(list(error = paste0(
        "Fichier introuvable pour '", table_code, "' dans ", inputs_dir,
        " (cherché : ", table_code, ".gpkg puis ", table_code, ".shp ; ",
        "ou préciser la clé 'file' du dataset)")))
    }
  } else {
    path <- file.path(inputs_dir, file)
    if (!file.exists(path)) {
      return(list(error = paste0("Fichier introuvable pour '", table_code, "' : ", path)))
    }
  }

  if (grepl("\\.gpkg$", path, ignore.case = TRUE)) {
    layers <- tryCatch(sf::st_layers(path)$name, error = function(e) NULL)
    if (is.null(layers)) {
      return(list(error = paste0("GeoPackage illisible pour '", table_code, "' : ", path)))
    }
    if (is.null(layer)) {
      if (length(layers) == 1) {
        layer <- layers[1]
      } else if (table_code %in% layers) {
        layer <- table_code
      } else {
        return(list(error = paste0(
          "Le GeoPackage ", basename(path), " contient plusieurs couches (",
          paste(layers, collapse = ", "), ") : préciser la clé 'layer' du dataset '", table_code, "'")))
      }
    } else if (!layer %in% layers) {
      return(list(error = paste0(
        "Couche '", layer, "' absente de ", basename(path), " (couches : ", paste(layers, collapse = ", "), ")")))
    }
  }

  list(path = path, layer = layer,
       label = if (is.null(layer)) basename(path) else paste0(basename(path), "::", layer),
       error = NULL)
}
