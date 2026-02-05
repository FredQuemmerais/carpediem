# CARPEDIEM - Script des fonctions retirées du code
# Ce script auxiliaire doit être redistribué par la suite.

# Date          : 20yy/mm/jj
# Version code  : 1.0
# Version R     : R-4.3.3
# Auteurs       : Alexis Esquerré (Cerema) d'après les travaux de :
# Auteurs       : Alice Vanhoutte-Brunier (OFB), Julien Barrere (OFB)
# Contributeurs : Frederic Quemmerais-Amice (OFB), Guilhem Autret (OFB)
# Correctrice   : Tiphaine Lodier (OFB)
# Contact, Documentation, Citations, Licence, Responsabilité      : Veuillez consulter les scripts principaux.


## ETAPE 2 ----
# Preparation sauvegarde des resultats
# - Creation d'une fonction pour convertir le binaire entre dans le fichier de parametrage en terme de SRM etudiees
#### !TODO Cette fonction ne devra plus servir car on va retirer cette façon de noter les SRM. Pour l'instant elle est conservée. 
read_srm <- function(x){
  out <- c()
  if(as.numeric(substr(x, 1, 1))==1) out <- c(out, "MED")
  if(as.numeric(substr(x, 2, 2))==1) out <- c(out, "GDG")
  if(as.numeric(substr(x, 3, 3))==1) out <- c(out, "CEL")
  if(as.numeric(substr(x, 4, 4))==1) out <- c(out, "MMN")
  return(out)
}
