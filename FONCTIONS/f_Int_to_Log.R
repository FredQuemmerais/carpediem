# -------------------------------------------------------------
# Mini fonction pour transformer valeurs entieres en logiques pour le fichier de paramètres
# Assure que tout remplissage mal fait du fichier résulte en un NA, pas en un TRUE
# Note : as.logical() décide que tout ce qui n'est pas 0 (FALSE) est forcément TRUE.
# -------------------------------------------------------------
f_Int_to_Log <- function(x){
  ifelse(x == 0, FALSE, ifelse(x == 1, TRUE, NA))
}
