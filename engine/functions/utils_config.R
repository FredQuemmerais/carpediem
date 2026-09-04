#engine/functions/utils_config.R

cfg_get <- function(x, path, default = NULL) {
  cur <- x
  for (p in path) {
    if (is.null(cur) || !is.list(cur) || is.null(cur[[p]])) return(default)
    cur <- cur[[p]]
  }
  cur
}

as_logical01 <- function(x, default = FALSE) {
  if (is.null(x) || is.na(x)) return(default)
  if (is.logical(x)) return(isTRUE(x))
  if (is.numeric(x)) return(as.integer(x) != 0)
  if (is.character(x)) return(tolower(trimws(x)) %in% c("1","true","t","yes","y","oui"))
  default
}

norm_source <- function(x) {
  if (is.null(x)) return(NA_character_)
  tolower(trimws(as.character(x)))
}
