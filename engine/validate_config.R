# engine/validate_config.R
# valide la configuration avant lancement de l'analyse
validate_config <- function(cfg) {
  
  fail <- function(msg) stop(paste0("Config invalide: ", msg), call. = FALSE)
  
  # accède à une valeur imbriquée via un chemin "a.b.c"
  get_path <- function(x, path) {
    parts <- strsplit(path, "\\.", fixed = FALSE)[[1]]
    cur <- x
    for (p in parts) {
      if (is.null(cur) || is.null(cur[[p]])) return(NULL)
      cur <- cur[[p]]
    }
    cur
  }
  
  # vérifie qu'un champ existe et n'est pas vide
  must_have <- function(path) {
    v <- get_path(cfg, path)
    if (is.null(v) || (is.character(v) && length(v) == 1 && nchar(trimws(v)) == 0)) {
      fail(paste0("champ requis manquant: ", path))
    }
    v
  }
  
  # vérifie qu'un champ est numérique et le convertit en entier
  must_int <- function(path) {
    v <- must_have(path)
    if (!is.numeric(v) && !is.integer(v)) {
      fail(paste0("doit être numérique: ", path))
    }
    as.integer(v)
  }
  
  # Validation data_source en premier
  data_source <- tolower(trimws(as.character(
    get_path(cfg, "db.data_source") %||% "postgis"
  )))
  if (!data_source %in% c("postgis", "flat_files")) {
    fail(paste0(
      "db.data_source doit être 'postgis' ou 'flat_files' (reçu: '",
      data_source, "')"
    ))
  }
  
  # Validation connexion PostgreSQL uniquement en mode postgis
  if (identical(data_source, "postgis")) {
    
    # "local" par défaut si non renseigné
    db_choice <- get_path(cfg, "db.choice")
    if (is.null(db_choice) || !nzchar(trimws(as.character(db_choice)))) {
      db_choice <- "local"
    } else {
      db_choice <- tolower(trimws(as.character(db_choice)))
    }
    
    if (!db_choice %in% c("local", "dist1")) {
      fail(paste0("db.choice doit être 'local' ou 'dist1' (reçu: '", db_choice, "')"))
    }
    
    # sélectionne le bloc de connexion correspondant
    db_block <- if (db_choice == "local") "DBConnectlocal" else "DBConnectdist1"
    
    must_have(paste0(db_block, ".host"))
    must_int (paste0(db_block, ".port"))
    must_have(paste0(db_block, ".dbname"))
    must_have(paste0(db_block, ".user"))
    must_have(paste0(db_block, ".password"))
  }
  
  TRUE
}
