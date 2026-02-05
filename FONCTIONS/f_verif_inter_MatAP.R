# Fonction f_verif_inter_MatAP
# Fonction permettant d'aller chercher les liens activites pressions dans la matrice


f_verif_inter_MatAP <- function(vecirow,vecicol,Mat,cond){
  #testFonction : vecirow <- row_MatAP  ; vecicol <-col_MatAP   ; Mat <- MatAP  ; cond <- "x"
  
  nrow  <- length(vecirow)
  ncol  <- length(vecicol)
  out <- FALSE
  for(ivec in 1:nrow){
    for(jvec in 1:ncol){
      if(tolower(Mat[vecirow[ivec],vecicol[jvec]])==cond)  out <- TRUE
    }
  }
  return(out)
}