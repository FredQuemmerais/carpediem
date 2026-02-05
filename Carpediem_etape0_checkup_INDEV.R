# CARPEDIEM - Etape 0 - Checkup vérifiant la conformité des données d'entrée et des paramètres du fichier ODS

# 

# Date          : 2025/06/18
# Version code  : 1.0
# Version R     : R-4.3.3
# Auteurs       : Alexis Esquerré (Cerema) d'après les travaux de :
# Auteurs       : Alice Vanhoutte-Brunier (OFB), Julien Barrere (OFB)
# Contributeurs : Frederic Quemmerais-Amice (OFB), Guilhem Autret (OFB)
# Correctrice   : Tiphaine Lodier (OFB)

# Contact       : frederic.quemmerais-amice@ofb.gouv.fr                     (sur le projet Carpediem)
# Contact       : alexis.esquerre@cerema.fr / alexis.esquerre7@gmail.com    (sur cette version du code)

# Office Français de la Biodiversite (https://www.ofb.gouv.fr/)
# Cerema (https://www.cerema.fr/)

# Licence       : Ce programme est un logiciel libre diffusé sous les termes de la licence publique générale GNU
#                 (GNU General Public License, GNU GPL) version 3 ou toute version ultérieure.
#                 Vous pouvez consulter le guide rapide de la GNU GPL v3 sur https://www.gnu.org/licenses/quick-guide-gplv3.fr.html
#                 Vous pouvez redistribuer et/ou modifier le contenu de ce programme suivant les termes 
#                 de la GNU GPL version 3 ou ultérieure telle que publiee par la Free Software Foundation.
#                 Consultez la GNU General Public License pour plus de details.

# Responsabilité : Ce programme est diffusé dans l'espoir qu'il sera utile.
#                 Les auteurs, les contributeurs et l'OFB n'offrent aucune garantie de fonctionnement et de résultat liée à l'utilisation de ce programme.
#                 Ils ne peuvent en aucun cas être tenu responsables des interprétations et des conclusions qui pourraient être faites suite à l'utilisation de ce programme.
#

# Que vérifie le script :
#     - Conformité du fichier de paramétrage
#     - Correspondances des codes des activités, pressions, etc.
#     - Chaque donnée d'entrée est accessible et bien formatée (noms de champs)
#     - 
#     - 

verif <- list()      # probablement pas utile... a voir !
problemes <- list()  # problemes = liste de tous les problemes rencontres
temp <- NA           # temp = variable temporaire reutilisee tout au long des tests


# Partie 1 - Configuration -----------------------------------------------------
## Librairies et chemins d'acces -----------------------------------------------
library(rpostgis) #lintr me dit qu'il n'est pas utilisé... vérifier dans les scripts annexes
library(sp) ### A remplacer par sf
library(sf)
library(raster)       # Attention : select() de la librairie raster est en conflit avec select() de tidyverse
library(DBI)
library(RPostgres)
library(RPostgreSQL)  # pour lire DB et transferer data.frames vers DB PostgreSQL
library(ggplot2)
library(ggmap)        # fonds de cartes, en association avec ggplot2
library(readODS)
library(tidyverse)
source("./FONCTIONS/f_importConfig.R")                       # code source pour importer les parametres / tables de DB
source("./FONCTIONS/f_importTableFromDBPostgresSQL.R")       # code source pour importer les parametres / tables de DB
source("./FONCTIONS/f_argDB.R")                              # code source de fonction aidant à préparer un SELECT sur BD PostgreSQL
source("./FONCTIONS/f_idFieldDBTable_from_nameField.R")      # autre fonction servant aussi à préparer un SELECT sur SQL
source("./FONCTIONS/Rpostgis_marha_202403.R")                # code source Nicolas Lambert - modifie par AVB
source("./FONCTIONS/Plot_VARz_grid2D.R")                     # code source fonctions de plot des cartes
source("./FONCTIONS/f_lognorm.R")                            # Fonction de normalisation par logarithme
source("./FONCTIONS/f_export_table.R")                       # Fonction d'export des tables
source("./FONCTIONS/f_verif_inter_MatAP.R")
source("./FONCTIONS/prep_graph.R")                           # Fonction pour préparer les graphiques
source("./FONCTIONS/f_norm.R")                               # Fonction de normalisation
source("./FONCTIONS/f_Int_to_Log.R")                         # Similaire à as.logical(), mais renvoie NA si input n'est ni 0 ni 1

source("./00_chemins_acces.R")                               # Recupere le chemin d'acces au dossier et le nom du fichier de parametrage
setwd(moddir)

file_path <- paste0(paramdir,"/",fileparam)       # chemin d'acces et nom du fichier qui contient les parametres
sensi_path <- paste0(paramdir,"/",filesensi)      # chemin et nom de l'ods avec les sensibilites


for (chemin in c(file_path,filesensi,userdir,moddir,indir,outdir,paramdir,sauvrepdir,sigdir)){
  if(!file.exists(chemin)) {problemes$chemins_acces[[chemin]] <- paste("Le chemin n'existe pas ici :",chemin)}
}

config    <- f_importConfig(file_path)            # lecture du fichier de parametrage via une fonction dans un script annexe






codes_manquants_dans_sensibilite <- HabBenth_z$E_habcode[!HabBenth_z$E_habcode %in% Matrice_Sensi$code_hab]









## Import des donnees geographiques --------------------------------------------
temp <- f_Int_to_Log(config$SauvRep$lrepr_DBtables)
if(temp == TRUE) {
  filerepr <- paste0(sauvrepdir,"/init_tables_",config$SauvRep$daterepr,".Rdata")
  if(file.exists(filerepr)) {
    load(file = filerepr)
  } else {
    problemes$donnees_geographiques[["Fichier_de_reprise"]] <- paste0("Le fichier .Rdata suivant n existe pas : ",filerepr)
    temp <- FALSE
  }
}

if(temp == FALSE){ # Note : a ce niveau soit config$lrepr = FALSE soit filerepr n'existe pas donc temp = FALSE
  list_sauv <- list()
  vtables <- unique(config$DBData$Data[,"DB_table"])
  for(table_i in vtables){
    row_data <- config$DBData$Data %>% 
      as.data.frame() %>% 
      dplyr::filter(DB_table == table_i) %>% 
      slice(1) 
    tab_name <- paste0(row_data["Data_type"],"_",row_data["Table_code"])  
    if(row_data[,"Source"] == "PgAdmin"){
      args <- f_argDB(row_data[,"Table_code"],config$DBData$Data,config)
      temp <- f_importTableFromDBPostgresSQL(args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8])
    } else if(row_data[,"Source"] == "SHP") {
      filename <- paste0(sigdir,"/INPUTS/", row_data[,"DB_table"])
      temp <- st_read(filename)
    } else if(row_data[,"Source"] == "GPKG") {
      filename <- strsplit(row_data[,"DB_table"], "::")[[1]]
      temp <- st_read(
        dsn = paste0(sigdir,"/INPUTS/", filename[1]),
        layer = filename[2]
      )
    } else {
      temp <- NA
      problemes$donnees_geographiques[[row_data[,"DB_table"]]] <- paste(row_data[,"DB_table"], row_data[,"Source"], "n'est pas dans un format pris en charge", sep = ', ')
    }
    list_sauv[[tab_name]] <- temp 
  }
  if(f_Int_to_Log(config$SauvRep$lsauv_DBtables) == TRUE) {
    filesauv <- paste0(sauvrepdir,"/init_tables_",datesauv,".Rdata")
    save(list_sauv,file=filesauv)
  }
}









# Des pressions qui manquent dans les sensibilités des habitats 
codes_problematiques <- setdiff(paste0(tolower(P_code), suf_sensib), lHabBenth_z$P_codes_HabBenth)
if(length(codes_problematiques) > 0) {
  print("ATTENTION : Ces pressions manquent dans les sensibilités des habitats :")
  stop(paste0(codes_problematiques," / "))
  # Et si on stoppait le code AVANT de lancer une boucle longue et tout et tout ?
}






# 3 - Ouvrir les données




problemes$schemas_tables_champs <- list()

### Attention il te faut ouvrir config avant
data_unique <- unique(as.data.frame(config$DBData$Data))

for (rownum in seq_len(nrow(data_unique))) {
  rowdat <- data_unique[rownum, ]
  
  if(rowdat$DB_choice == "local"){
    dbco <- config$DBConnectlocal
  } else if(rowdat$DB_choice == "dist1"){
    dbco <- config$DBConnectdist1
  }
  
  con <- dbConnect(
    drv = dbDriver(dbco$DBConnect_drv), 
    dbname = dbco$DBConnect_dbname, 
    host = dbco$DBConnect_host, 
    port = dbco$DBConnect_port,
    user = dbco$DBConnect_user,
    password = dbco$DBConnect_password
  )
  on.exit(dbDisconnect(con))
  
  res <- dbGetQuery(con,
    "SELECT 1 FROM information_schema.columns
    WHERE table_schema = $1
    AND table_name = $2
    AND column_name = $3",
    params = list()
  )
  
  if(nrow(res)==0){
    probleme <- paste(rowdat$DB_scheme, rowdat$DB_table, rowdat$Field, sep = ".")
    problemes$schemas_tables_champs[[probleme]] <-  paste("La donnée", probleme, "n'existe pas.")
  }
}








# Ci-dessous un bout de script de quand j'avais des 
# NA au lieu de 99 dans mes sensibilites des enjeux ecologiques -----
# A faire : Remplacer ça par un checkup : si la valeur correspondant à Inconnu est 99, y a-t-il des NA ?
#
# @Alexis@ Rajout de deux petits bouts de scripts pour reconvertir des colonnes Char en Num, et remplacer des NA par la valeur de 99
#!TODO : remplacer 13:16 et 30:787 par des nombres plus... flexibles
# for(col in 13:16) {
#   HabBenth_z[[col]] <- as.numeric(HabBenth_z[[col]])
# }
# for(col in 30:787) {
#   HabBenth_z[[col]][is.na(HabBenth_z[[col]])] <- 99
# }






# Verification des surfaces d'habitat vs surface totale de mer ----
# transformer pour que ce soit uniquement 
# via les ID des cellules, pas par leurs coordonnées, parce qu'en plus ça marche très mal...
# Suggestion : group_by ID_cellule, surfmer = max(surfmer), surfhab = sum(surfhab), voir pour quels ID_cellule c'est pas bon

# config$surfmer_z <- lHabBenth_z$surfmer_z
# nbdec <- 6 # nombre de decimales (pour surfaces exprimees en km2)
# depa        <- which(apply(as.matrix(c(1:nz)), c(1,2), function(x) (round(lHabBenth_z$surfhab_z[x],nbdec) > round(config$surfmer_z[x],nbdec))==TRUE))
# if(length(depa)>0){
#   print(paste("ATTENTION : ",length(depa)," mailles sur ",nz," ont surfhab > surfmer"," AVEC MARGE ",nbdec," chiffres apres la virgule"))
#   for(idepa in 1:length(depa)){
#     # - indice correspondant grid_z
#     iz <- which(col_idz==lHabBenth_z$idmesh_z[depa[idepa]])
#     if(config$lat_z[iz] < config$latmax && config$lat_z[iz] > config$latmin && config$long_z[iz] < config$longmax && config$long_z[iz] > config$longmin )
#     {
#       print(paste(" SURFACE HAB > SURF MER DANS le perimetre  maille: ",iz,lHabBenth_z$idmesh_z[depa[idepa]],
#                   round(lHabBenth_z$surfhab_z[depa[idepa]],nbdec),round(config$surfmer_z[depa[idepa]],nbdec),
#                   "lat:",round(config$lat_z[iz],3),"long:",round(config$long_z[iz],3)))
#     }
#   }
# }


# - reajustement des surfaces d'habitats par maille pour mailles surfhab > surfmesh en attente couche habitats "clean"
if(config$Calc$l_corr_surfhab){
  lHabBenth_z$surfhab_z_NEW <- lHabBenth_z$surfhab_z
  lHabBenth_z$surfhab_z_NEW[lHabBenth_z$surfhab_z_NEW > config$surfmer_z] <- 0
  
  for(ipoly in 1:nb_poly){
    id <- which(lHabBenth_z$idmesh_z == st_drop_geometry(HabBenth_z)[[i_idzhab]][[ipoly]])
    if(lHabBenth_z$surfhab_z[id] > config$surfmer_z[id]){
      # print(paste("# :: surface habitat du polygone corrigee OLD : ",ipoly,HabBenth_z[[i_surfhab]][[ipoly]]))   #Commenté - AE2025
      HabBenth_z[[i_surfhab]][[ipoly]] <- HabBenth_z[[i_surfhab]][[ipoly]] * config$surfmer_z[id] / lHabBenth_z$surfhab_z[id]
      # print(paste("#                                         NEW : ",ipoly,HabBenth_z[[i_surfhab]][[ipoly]]))   #Commenté - AE2025
      lHabBenth_z$surfhab_z_NEW[id] <- lHabBenth_z$surfhab_z_NEW[id]+ HabBenth_z[[i_surfhab]][[ipoly]]
    }
  }
  lHabBenth_z$surfhab_z <- lHabBenth_z$surfhab_z_NEW
}



## Verifier que les codes d'activite sont bien remplis dans la feuille de parametres 

test_codes <- as.data.frame(config$DBData$Data) %>% 
  filter(Data_type == "A") %>% 
  select(Code1, Code2, Code3, Code4, Code5) %>%
  pivot_longer(everything(), values_to = "code") %>%
  filter(!is.na(code)) %>%
  pull(code)
if(any(duplicated(test_codes))) {
  temp <- duplicated(test_codes)
  problemes$donnees_geographiques$codes_dupliques <- paste("Il y a plusieurs fois le meme code d'activite :", paste(temp, collapse = ", "))
}




# VERIFICATIONS ----
## VERIF 1 - Ordre des feuillets
noms_feuillets <- ods_sheets(file_path)
noms_attendus <- c("a_lire", "configuration", "connexion_bd", "typologie_pressions", "typologie_activites", "matrice_activites_pressions", "data", "statistiques_par_zones", "simulation_monte_carlo")

if (identical(noms_feuillets[1:9], noms_attendus)) {
  verif$noms_feuillets_ods <- "Les 9 feuillets ODS sont bien nommés et dans le bon ordre."
} else if (all(noms_attendus %in% noms_feuillets)) {
  verif$noms_feuillets_ods <- "PROBLEME - Feuillets ODS pas dans le bon ordre"
  problemes$noms_feuillets_ods <- paste("Feuillets ODS pas dans le bon ordre. Ordre attendu : \n",
                                        paste(noms_attendus, collapse = ', '))
} else {
  verif$noms_feuillets_ods <- "PROBLEME - Feuillets ODS mal nommés ou manquants"
  problemes$noms_feuillets_ods <- paste("Feuillets ODS mal nommés ou manquants :\n",
                                        paste(noms_feuillets, collapse = ', '), "\n",
                                        "Noms attendus : \n",
                                        paste(noms_attendus, collapse = ', '))
}


codes_problematiques <- setdiff(paste0(tolower(P_code), suf_sensib), lHabBenth_z$P_codes_HabBenth)
if(length(codes_problematiques) > 0) {
  print("ATTENTION : Ces pressions manquent dans les sensibilités des habitats : ")
  stop(paste0(codes_problematiques," / "))
}

