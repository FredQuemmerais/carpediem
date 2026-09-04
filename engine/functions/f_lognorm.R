#engine/functions/f_lognorm

f_lognorm <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- 0
  
  # cas 1 : pression absente partout
  if (all(x == 0)) {
    return(rep(0, length(x)))
  }
  
  y <- log(x + 1)
  r <- range(y, na.rm = TRUE)
  
  # cas 2 : pression constante mais > 0
  if (!is.finite(diff(r)) || diff(r) == 0) {
    return(rep(1, length(x)))
  }
  
  out <- (y - r[1]) / diff(r)
  out[!is.finite(out)] <- 0
  out
}
