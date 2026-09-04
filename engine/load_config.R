# engine/load_config.R
# charge le fichier de config yalm et normalise les objets 
# pour qu'ils soient au format attendus pas validate_config

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

load_config <- function(yaml_path) {
  stopifnot(is.character(yaml_path), length(yaml_path) == 1)
  
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package requis manquant: yaml. Installez-le via install.packages('yaml').")
  }
  
  if (!file.exists(yaml_path)) {
    stop(sprintf("Fichier YAML introuvable: %s", yaml_path))
  }
  
  cfg <- yaml::read_yaml(yaml_path)
  
  as_df_safe <- function(x, name) {
    if (is.null(x)) return(NULL)
    if (is.data.frame(x)) return(x)
    
    if (!is.list(x)) {
      stop(sprintf("Type non supporté pour '%s': %s", name, class(x)[1]))
    }
    if (length(x) == 0) return(data.frame())
    
    if (!is.null(names(x)) && any(names(x) != "")) {
      ok_cols <- all(vapply(x, function(v) is.atomic(v) || is.null(v), logical(1)))
      if (ok_cols) {
        return(as.data.frame(x, stringsAsFactors = FALSE))
      }
      stop(sprintf("'%s' est une liste nommée non convertible en data.frame. Attendu: liste de lignes ou liste de colonnes.", name))
    }
    
    all_cols <- unique(unlist(lapply(x, names)))
    
    rows <- lapply(x, function(row) {
      if (is.null(row)) row <- list()
      row <- as.list(row)
      row <- lapply(row, function(v) if (is.null(v)) NA else v)
      
      missing <- setdiff(all_cols, names(row))
      if (length(missing) > 0) for (m in missing) row[[m]] <- NA
      
      row[all_cols]
    })
    
    df <- as.data.frame(
      do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)),
      stringsAsFactors = FALSE
    )
    rownames(df) <- NULL
    df
  }
  
  # DBData
  if (!is.null(cfg$DBData) && !is.null(cfg$DBData$Data)) {
    cfg$DBData$Data <- as_df_safe(cfg$DBData$Data, "DBData:Data")
    if (!is.data.frame(cfg$DBData$Data)) {
      stop("DBData$Data doit être un data.frame (après load_config). Vérifiez le format YAML.", call. = FALSE)
    }
  }
  
  # tabgraph
  if (!is.null(cfg$tabgraph) && !is.null(cfg$tabgraph$Data)) {
    cfg$tabgraph$Data <- as_df_safe(cfg$tabgraph$Data, "tabgraph:Data")
    if (!is.data.frame(cfg$tabgraph$Data)) {
      stop("tabgraph$Data doit être un data.frame (après load_config). Vérifiez le format YAML.", call. = FALSE)
    }
  }
  
  if (!is.null(cfg$datasets)) {
    cfg$datasets <- as_df_safe(cfg$datasets, "datasets")
    if (!is.data.frame(cfg$datasets)) {
      stop("datasets doit être un data.frame après load_config. Vérifiez le format YAML.", call. = FALSE)
    }
  }
  
  # Coercion simple des ports (listes)
  if (!is.null(cfg$DBConnectlocal$port)) cfg$DBConnectlocal$port <- as.integer(cfg$DBConnectlocal$port)
  if (!is.null(cfg$DBConnectdist1$port)) cfg$DBConnectdist1$port <- as.integer(cfg$DBConnectdist1$port)
  
  # Validation data_source
  valid_sources <- c("postgis", "flat_files")
  ds <- tolower(trimws(cfg$db$data_source %||% "postgis"))
  if (!ds %in% valid_sources) {
    stop(sprintf(
      "load_config: db$data_source invalide ('%s'). Valeurs acceptées: %s",
      ds, paste(valid_sources, collapse = ", ")
    ))
  }
  
  # Désactivation automatique des exports DB en mode flat_files
  if (identical(ds, "flat_files")) {
    if (!is.null(cfg$analyses$monte_carlo)) {
      cfg$analyses$monte_carlo$export_simul  <- FALSE
      cfg$analyses$monte_carlo$export_result <- FALSE
      if (getOption("app.verbose", default = FALSE)) {
        message("[load_config] Mode flat_files : export base de données désactivé, export fichiers actif.")
      }
    }
    if (!is.null(cfg$Outputs)) {
      cfg$Outputs$export_db         <- FALSE
      cfg$Outputs$export_db_details <- FALSE
    }
  }
  
  cfg
}
