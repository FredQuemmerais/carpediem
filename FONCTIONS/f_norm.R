# Etape 10
# - fonction pour normaliser les scores de sensibilite entre 0 et 1 
f_norm <- function(in.MAT){
  out <- in.MAT[,1] # on initialise la matrice de sortie avec la colonne identifiants de maille 
  for(i in 2:(dim(in.MAT)[2])){ # boule sur les pressions
    if(max(in.MAT[,i], na.rm=T) > 0){ # si il y  des mailles dont la sensibilite a la pression i est non nulle
      imin   <- min(in.MAT[,i],na.rm=TRUE) 
      irange <- range(in.MAT[,i],na.rm=TRUE)
      out_i <- (in.MAT[,i]-imin)/diff(irange) # sensib_norm <- (sensib - sensib_min)/(sensib_max - sensib_min)
    }else {
      out_i <- in.MAT[,i] # si toutes les mailles ot une sensibilite nulle ou inconnue, on ne normalise pas et on garde la colonne d'origine
    } 
    out <- cbind.data.frame(out, out_i)
  }
  return(out)
}