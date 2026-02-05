# Etape 9
# ecriture d'une fonction permettant a partir d'un nombre x de categorie et d'un vecteur 
# de diviser les valeurs du vacteur en x categorie (intervalles egaux) 
# La fonction renvoie un tableau contenant pour chaque intervalle le nombre 
# total et le pourcentage d'elements du vecteur se trouvant dans cette categorie

prep_graph <- function (vec,nbclass) {
  out <- as.data.frame(matrix(data=0.,nrow=nbclass,ncol=2))
  inter <- diff(range(vec))/nbclass
  for(i in 1:nbclass) {
    inf_i <- min(vec)+(i-1)*inter
    sup_i <- inf_i+inter
    row.names(out)[i]=paste(as.character(round(inf_i,2)), as.character(round(sup_i,2)), sep=" - ")
    for(j in 1:length(vec)){
      if (vec[j]>=inf_i && vec[j]<sup_i){
        out[i,1] <- out[i,1]+1
      }
    }
    out[i,2]=round((out[i,1]/length(vec)*100),2)
  }
  colnames(out) <- c("Compte","Pourcentage")
  Cat<-as.factor(row.names(out))
  out <- cbind(Cat,out)
  row.names(out)<-c(1:nbclass)
  return(out)
}