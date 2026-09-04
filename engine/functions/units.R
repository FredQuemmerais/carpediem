# engine/steps/functions/units.R

# ---- Units helpers ------------------------------------------------------------

convert_area <- function(x, from, to) {
  x <- suppressWarnings(as.numeric(x))
  if (!is.character(from) || !nzchar(from)) stop("convert_area: 'from' vide.")
  if (!is.character(to)   || !nzchar(to))   stop("convert_area: 'to' vide.")
  if (identical(from, to)) return(x)
  
  # Convertir en m2, puis vers cible
  to_m2 <- function(v, unit) {
    if (unit == "m2")  return(v)
    if (unit == "km2") return(v * 1e6)
    if (unit == "ha")  return(v * 1e4)
    stop("Unité surface inconnue: ", unit)
  }
  from_m2 <- function(v, unit) {
    if (unit == "m2")  return(v)
    if (unit == "km2") return(v / 1e6)
    if (unit == "ha")  return(v / 1e4)
    stop("Unité surface inconnue: ", unit)
  }
  
  from_m2(to_m2(x, from), to)
}

get_area_unit <- function(config, dataset, field, default = "m2") {
  u <- default
  if (!is.null(config$units$areas[[dataset]][[field]])) {
    u <- as.character(config$units$areas[[dataset]][[field]])
  }
  u
}

assert_allowed_units <- function(config, units_vec, context = "") {
  allowed <- config$units$areas$allowed
  if (is.null(allowed) || length(allowed) == 0) {
    allowed <- c("m2","km2","ha")
  }
  bad <- setdiff(unique(units_vec), allowed)
  if (length(bad) > 0) {
    stop(context, ": unité(s) non autorisée(s): ", paste(bad, collapse = ", "),
         ". Autorisées: ", paste(allowed, collapse = ", "))
  }
}

# Sanity check: détecte mismatch de 1e6 typique m2/km2
check_area_ratio_signature <- function(surfhab_m2, surfmer_m2, context = "") {
  surfhab_m2 <- suppressWarnings(as.numeric(surfhab_m2))
  surfmer_m2 <- suppressWarnings(as.numeric(surfmer_m2))
  ok <- is.finite(surfhab_m2) & is.finite(surfmer_m2) & surfmer_m2 > 0
  if (!any(ok)) return(invisible(NULL))
  
  rat <- surfhab_m2[ok] / surfmer_m2[ok]
  med <- stats::median(rat, na.rm = TRUE)
  
  # Ici, si vous aviez surfhab en m2 et surfmer en km2 mais traités pareil,
  # vous observez un ratio ~ 1e6. Après conversion vers m2, ce ratio retombe proche de 1.
  if (is.finite(med) && med > 1e3) {
    warning(context, ": ratio median(surfhab/surfmer)=", signif(med,4),
            " très grand. Probable incohérence d'unités (ex: m2 vs km2).")
  }
  invisible(NULL)
}
