# engine/db_connect.R
# Gestion de la connexion à la base de données

get_db_block <- function(config, choice = c("local", "dist1")) {
  if (!missing(choice) && length(choice) == 1 && !is.null(choice)) {
    choice <- tolower(trimws(as.character(choice)))
    if (!choice %in% c("local", "dist1")) {
      stop(sprintf("DB choice invalide: '%s'. Attendu: 'local' ou 'dist1'.", choice))
    }
  } else {
    choice <- match.arg(choice)
  }
  
  if (choice == "local") return(config$DBConnectlocal)
  if (choice == "dist1") return(config$DBConnectdist1)
  
  stop(sprintf("DB choice non géré: '%s'.", choice))
}

get_db_conn <- function(config, which = c("local", "dist1")) {
  
  # Mode flat_files : aucune connexion PostgreSQL
  data_source <- tolower(trimws(config$db$data_source %||% "postgis"))
  if (identical(data_source, "flat_files")) {
    message("[DB] Mode flat_files : aucune connexion PostgreSQL établie.")
    return(NULL)
  }
  
  dbco <- get_db_block(config, which)
  
  if (!requireNamespace("DBI", quietly = TRUE)) stop("Package manquant: DBI")
  if (!requireNamespace("RPostgres", quietly = TRUE)) stop("Package manquant: RPostgres")
  
  if (is.null(dbco)) {
    stop(sprintf("Bloc config manquant pour la DB (choice='%s').", as.character(which)))
  }
  
  host <- dbco$host
  port <- dbco$port
  dbname <- dbco$dbname
  user <- dbco$user
  password <- dbco$password
  
  if (is.null(password) || !nzchar(as.character(password))) {
    stop(sprintf("Mot de passe absent dans le bloc DB pour choice='%s' (champ 'password').", as.character(which)))
  }
  
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = host, port = port, dbname = dbname,
    user = user, password = password
  )
}

db_disconnect_safe <- function(con) {
  if (!is.null(con)) {
    try(DBI::dbDisconnect(con), silent = TRUE)
  }
  invisible(TRUE)
}
