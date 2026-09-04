# engine/steps/functions/f_norm
# Normalisation des scores de sensibilité entre 0 et 1
f_norm <- function(in.MAT) {
  
  out <- in.MAT[, 1, drop = FALSE]
  ncol_mat <- ncol(in.MAT)
  if (ncol_mat < 2) return(out)
  
  for (i in 2:ncol_mat) {
    
    colname_i <- names(in.MAT)[i]
    x_raw <- in.MAT[[i]]
    
    if (is.list(x_raw) && !is.data.frame(x_raw)) {
      x_raw <- unlist(x_raw, recursive = TRUE, use.names = FALSE)
    }
    
    if (length(x_raw) != nrow(in.MAT)) {
      out_i <- x_raw
    } else {
      x_num <- suppressWarnings(as.numeric(x_raw))
      x_ok  <- x_num[is.finite(x_num)]
      
      if (length(x_ok) == 0) {
        out_i <- x_raw
      } else {
        xmax <- max(x_ok)
        if (xmax <= 0) {
          out_i <- x_raw
        } else {
          xmin <- min(x_ok)
          den  <- xmax - xmin
          if (!is.finite(den) || den == 0) {
            out_i <- x_raw
          } else {
            out_i <- (x_num - xmin) / den
          }
        }
      }
    }
    
    out_i <- setNames(as.data.frame(out_i), colname_i)
    out <- cbind.data.frame(out, out_i)
  }
  
  return(out)
}
