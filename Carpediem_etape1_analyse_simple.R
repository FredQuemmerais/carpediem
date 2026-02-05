# CARPEDIEM - ANALYSE PAR DEFAUT ----

# Analyse standard du risque d'effets cumulés (sans prise en compte de zones, ni Monte-Carlo) par Carpediem

# Date          : 2025/05/28
# Version code  : 2.0
# Version R     : R-4.3.3
# Auteurs       : Alexis Esquerré (Cerema), Sébastien Bouland (Cerema), d'après les travaux de :
# Auteurs       : Alice Vanhoutte-Brunier (OFB), Julien Barrere (OFB)
# Contributeurs : Frederic Quemmerais-Amice (OFB), Guilhem Autret (OFB)
# Correctrice   : Tiphaine Lodier (OFB)

# Contact       : frederic.quemmerais-amice@ofb.gouv.fr                     (sur le projet Carpediem)
# Contact       : alexis.esquerre@cerema.fr / alexis.esquerre7@gmail.com    (sur cette version du code)

# Office Français de la Biodiversite (https://www.ofb.gouv.fr/)
# Cerema (https://www.cerema.fr/)

# Documentation : Guide d'utilisation des outils d'analyses de donnees du projet Carpediem:
#                 cartographie du risque d'effets cumules sur les habitats benthiques
#                 version 7.1 publique (octobre 2022), OFB, 149 pages.
#                 Julien Barrere, Frederic Quemmerais-Amice

# Citation      : Quemmerais-Amice Frédéric, Barrere Julien, La Rivière Marie, Contin Gabriel, Bailly Denis, 2020.
#                 A Methodology and Tool for Mapping the Risk of Cumulative Effects on Benthic Habitats.
#                 Frontiers in Marine Science. 7:569205. https://doi.org/10.3389/fmars.2020.569205

# Licence       : Ce programme est un logiciel libre diffusé sous les termes de la licence publique générale GNU
#                 (GNU General Public License, GNU GPL) version 3 ou toute version ultérieure.
#                 Vous pouvez consulter le guide rapide de la GNU GPL v3 sur https://www.gnu.org/licenses/quick-guide-gplv3.fr.html
#                 Vous pouvez redistribuer et/ou modifier le contenu de ce programme suivant les termes 
#                 de la GNU GPL version 3 ou ultérieure telle que publiee par la Free Software Foundation.
#                 Consultez la GNU General Public License pour plus de details.

# Responsabilité : Ce programme est diffusé dans l'espoir qu'il sera utile.
#                 Les auteurs, les contributeurs, l'OFB et le Cerema n'offrent aucune garantie de fonctionnement et de résultat liée à l'utilisation de ce programme.
#                 Ils ne peuvent en aucun cas être tenu responsables des interprétations et des conclusions qui pourraient être faites suite à l'utilisation de ce programme.

# Sommaire ----------------------------------------------------------------------------------------------------
#              ETAPE 1 - Initialisation
#              ETAPE 2 - Import de tables PostGreSQL
#              ETAPE 3 - Reformatage de G_grid (carroyage)
#              ETAPE 4 - Reformatage de E_habbenth (habitats)
#              ETAPE 5 - Calcul sensibilité cumulee
#              ETAPE 6 - Cartographie des activites
#              ETAPE 7 - Cartographie des pressions
#              ETAPE 8 - Risque d'exposition
#              ETAPE 9 - Risque d'effets cumules
#              ETAPE 10 - Indice de confiance
#              ETAPE 11 - Export des resultats en PostgreSQL

# Que fait le script :
#     - 1res etapes : Chargement librairies, import fonctions annexes, lecture fichier de parametrage, creation dossier de sauvegarde
#     - Lecture donnees geographiques (habitats, pressions, carroyage) et reconstruction de dataframes de donnees avec une structure predefinie
#     - 
#     - 



        # AVANT L'ANALYSE ---- 
        # Remplir correctement les noms de fichier dans le script 00_chemins_acces.R



# ========== DEBUT DU CODE ==================================================== 
# ETAPE 1 - Initialisation ----------------------------------------------------
# ============================================================================= # 
# Cette etape recupere les chemins d'acces aux dossiers et fichiers,
# charge les librairies R utilisees, les scripts des fonctions annexes, lit le
# fichier de parametrage et determine quelles regions marines sont a analyser

rm(list = ls())
## Parametres de configuration (a renseigner dans 00_chemins_acces.R) ----------
source("./00_chemins_acces.R")                               #Recupere le chemin d'acces au dossier et le nom du fichier de parametrage
print(paste0("# Fichier de parametrage :   ", fileparam))
print(paste0("# Repertoire utilisateur :   ", userdir))

setwd(moddir)

## Charger les librairies ------------------------------------------------------
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
# library(lattice)
# library(ncdf4)
# library(psy)          # statistiques ACP

## Lire les scripts de fonctions ------------------------------------------------
source("./FONCTIONS/f_importConfig.R")                       # code source pour importer les parametres
source("./FONCTIONS/f_importTableFromDBPostgresSQL.R")       # code source pour importer les donnees geographiques en SQL
source("./FONCTIONS/f_argDB.R")                              # code source de fonction aidant a preparer un SELECT sur BD PostgreSQL
source("./FONCTIONS/f_idFieldDBTable_from_nameField.R")      # autre fonction servant aussi a preparer un SELECT sur SQL
source("./FONCTIONS/Rpostgis_marha_202403.R")                # code source Nicolas Lambert - modifie par AVB
source("./FONCTIONS/Plot_VARz_grid2D.R")                     # code source fonctions de plot des cartes
source("./FONCTIONS/f_lognorm.R")                            # Fonction de normalisation par logarithme
source("./FONCTIONS/f_linnorm.R")                            # Fonction de normalisation lineaire (x - xmin) / range
source("./FONCTIONS/f_export_table.R")                       # Fonction d'export des tables
source("./FONCTIONS/f_verif_inter_MatAP.R")
source("./FONCTIONS/prep_graph.R")                           # Fonction pour préparer les graphiques
source("./FONCTIONS/f_norm.R")                               # Fonction de normalisation
source("./FONCTIONS/f_Int_to_Log.R")                         # Similaire a as.logical(), mais renvoie NA si input n'est ni 0 ni 1

## Lire et traiter les fichiers tableurs ---------------------------------------
file_path <- paste0(paramdir,"/",fileparam)       # chemin d'acces et nom du fichier qui contient les parametres
config    <- f_importConfig(file_path)            # lecture du fichier de parametrage via une fonction dans un script annexe

sensi_path <- paste0(paramdir,"/",filesensi)

## Separer les SRM avec les separateurs "; , ou espaces blancs" ----------------
lcond_SRM <- config$TimeSpace$config_SRM %>% 
  strsplit(split = "[;,\\s]+") %>% 
  unlist() %>% 
  trimws()

print("Les codes des SRM retenues pour analyse sont :")
print(lcond_SRM)

## Gestion des sauvegardes et outputs ------------------------------------------
# On cree un dossier unique pour les outputs de CETTE analyse dans lequel on commence par exporter le fichier ODS de parametres
datesauv <- format(Sys.time(), "%Y-%m-%d_%H-%M") # Suffixe a ajouter au nom des sauvegardes et outputs

graphdir <- paste0(outdir, "/", datesauv)
dir.create(graphdir)

file.copy(from = file_path, to = paste0(graphdir, "/", fileparam) )


# ============================================================================= # 
# ETAPE 2 - Import de tables PostGreSQL ----------------------------------------
# ============================================================================= # 
# Dans cette etape :
# Soit simple reprise d'un RData de sauvegarde, soit :
# 
# Fonctions : 
# Import PostgreSQL : f_argDB recupere les arguments de requete SQL, f_importTable execute la requete


## Lecture ou reprise des donnees ----------------------------------------------
if(f_Int_to_Log(config$SauvRep$lrepr_DBtables) == FALSE) {
  list_sauv <- list()                                 # liste dans laquelle on enregistre les tables 
  vtables <- unique(config$DBData$Data[,"DB_table"])  # vecteur des noms de toutes les tables a importer

  for(table_i in vtables){
    # On cherche la 1re ligne de l'onglet Data avec le nom de la table 
    # Puis on cree le nom du dataframe qui contiendra la donnee
    row_data <- config$DBData$Data %>% 
      as.data.frame() %>% 
      dplyr::filter(DB_table == table_i) %>% 
      slice(1) 
    
    tab_name <- paste0(row_data["Data_type"],"_",row_data["Table_code"])  # p.ex. "A_pecheChalutFond" ou "G_gridCarroyage"
    
    # Importer la donnee en fonction du type (PostgreSQL, Shp, GPKG)
    if(row_data[,"Source"] == "PgAdmin"){       # Si la source est en SQL local
      args <- f_argDB(row_data[,"Table_code"],config$DBData$Data,config)
      temp <- f_importTableFromDBPostgresSQL(args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8])
    } else if(row_data[,"Source"] == "SHP") {   # Si la table est dans un fichier Shapefile
      filename <- paste0(moddir,"/INPUTS/", row_data[,"DB_table"])
      temp <- st_read(filename)
    } else if(row_data[,"Source"] == "GPKG") {  # Si la table est dans un Geopackage
      filename <- strsplit(row_data[,"DB_table"], "::")[[1]]
      temp <- st_read(
        dsn = paste0(moddir,"/INPUTS/", filename[1]),
        layer = filename[2]
      )
    } else {
      stop(paste(row_data[,"Source"], ": Source non prise en charge (developpements a venir)."))
    }
    # Enfin on met la table importee dans list_sauv avec le bon nom (grace a tab_name)
    list_sauv[[tab_name]] <- temp 
  }
  
  # Sauvegarde dans un fichier Rdata si l'option est cochee dans le fichier de parametres
  if(f_Int_to_Log(config$SauvRep$lsauv_DBtables) == TRUE) {
    filesauv <- paste0(sauvrepdir,"/init_tables_",datesauv,".Rdata")
    save(list_sauv,file=filesauv)
  }
  
} else {  # Sinon reprise des donnees sauvegardees si l'option est cochee
  filerepr <- paste0(sauvrepdir,"/init_tables_",config$SauvRep$daterepr,".Rdata")
  if(!file.exists(filerepr)) stop(paste0("Ligne 224, Etape 3 - le fichier .Rdata suivant n existe pas : ",filerepr))
  load(file = filerepr)
}


# ============================================================================= # 
# ETAPE 3 - Reformatage de G_grid (carroyage) ----------------------------------
# ============================================================================= # 
# Dans cette etape on recupere les noms de colonnes correspondant a l'ID de 
# maille, la surface, etc., et on reconstruit un dataframe avec des noms fixes
# Puis on filtre le carroyage pour ne garder que les cellules dans la zone d'analyse et avec surface de mer > 0
# On utilise le tout pour etablir la liste des ID et des surfaces de mer
#
# Fonctions :
# f_idFieldDBTable_from_nameField retourne l'indice de la colonne correspondant au type de champ (surfmer, grid...)

## Renommer les champs, reduire a zone d'etude ---------------------------------
i_SRM    <- f_idFieldDBTable_from_nameField("srm",st_drop_geometry(config$DBData$Data),list_sauv$G_grid,"grid") # colonne de la grille contenant la SRM de chaque maille
i_surfmer<- f_idFieldDBTable_from_nameField("surfmer",st_drop_geometry(config$DBData$Data),list_sauv$G_grid,"grid") # Colonne de la grille contenant la surface maritime
i_idz    <- f_idFieldDBTable_from_nameField("idmesh",st_drop_geometry(config$DBData$Data),list_sauv$G_grid,"grid") # Colonne de la grille contenant l'identifiant de maille

grid_z <- list_sauv$G_grid %>% 
  dplyr::select(
    srm = all_of(i_SRM),
    surfmer = all_of(i_surfmer),
    idmesh = all_of(i_idz),
    geometry
  ) %>% 
  dplyr::filter(srm %in% lcond_SRM, surfmer > 0.)

## Extraire indices et nb mailles ----------------------------------------------
nz           <- nrow(grid_z)      # nz = nombre de mailles

col_idz      <- grid_z$idmesh     # Ce sont les ID de maille
col_surfmer  <- grid_z$surfmer    # Ce sont les surfaces de mer de chq maille

G_grid <- list_sauv$G_grid

# ============================================================================= # 
# ETAPE 4 - Reformatage de E_habbenth (habitats) -------------------------------
# ============================================================================= # 
# Dans cette etape :
# Reduction des Habitats et sensibilites aux pressions et zones etudiees
# Traitement des "pressions agregees" : quand deux colonnes de pressions Press_1 et Press_2 correspondent a 
#   la meme "pression agregee" PrAG_01, alors l'outil calcule les impacts en prenant en compte la sensibilite
#   des habitats a PrAG_01 et met Press_1 et Press_2 de cote


# ================ CETTE PARTIE DEVRAIT ETRE DANS f.CONFIG ? ==================#
# ================       (a faire plus tard...)       =========================#
## Recuperer les indices issus de config ---------------------------------------
nj <- config$nj # Nombre de pressions dans la liste

# reduction du nombre de pressions si agregation de plusieurs pressions
# - import de toutes les pressions et caracteristiques (presentes dans mat A/P)
P_code <- config$P_code # Code des pressions
P_name <- config$P_name # Nom des pressions
P_modcalc <- config$P_modcalc # Mode de calcul des pressions
P_evalDEM <- config$P_evalDEM # Prises en compte des pressions dans l'evaluation

### Gestion des pressions agregees ---------------------------------------------
vec_P_ag <- na.omit(unique(config$P_code_ag[1:nj]))
for(ivec in seq_along(vec_P_ag)){
  # Ici on trouve les indices de ligne des pressions agregees
  # Puis on remplace les codes/noms de ces pressions par le code/nom de la pression
  #   agregee auxquelles ces pressions font reference
  vec_ip <- which(config$P_code_ag[1:nj]==vec_P_ag[ivec])
  P_code[vec_ip] <- config$P_code_ag[vec_ip]
  P_name[vec_ip] <- config$P_name_ag[vec_ip]
}

### Matrice des pressions retenues, suppr. doublons ----------------------------
mat <- cbind(P_code,P_name,P_modcalc,P_evalDEM)
mat <- mat[! duplicated(mat[,1]), ]
P_code     <- mat[,1]
P_name     <- mat[,2]
P_modcalc  <- mat[,3]
P_evalDEM  <- mat[,4]

nj <- length(P_code) #On corrige nj pour ne garder que le nombre de pressions retenues
# ============================================================================ #


## Renommer les champs, reduire a zone d'etude ---------------------------------
col_nums <- list()
col_nums$i_surfhab  <- f_idFieldDBTable_from_nameField("surfhab",config$DBData$Data,st_drop_geometry(list_sauv$E_habbenth_sensib),"habbenth_sensib")  # Colonne de la surface des habitats
col_nums$i_idzhab   <- f_idFieldDBTable_from_nameField("idmesh",config$DBData$Data,st_drop_geometry(list_sauv$E_habbenth_sensib),"habbenth_sensib")   # Colonne de l'identifiant de maille
col_nums$i_surfmesh <- f_idFieldDBTable_from_nameField("surfmesh",config$DBData$Data,st_drop_geometry(list_sauv$E_habbenth_sensib),"habbenth_sensib") # Colonne de la surface de cellule
col_nums$i_surfmer  <- f_idFieldDBTable_from_nameField("surfmer",config$DBData$Data,st_drop_geometry(list_sauv$E_habbenth_sensib),"habbenth_sensib")  # Colonne de la surface de mer dans la cellule
col_nums$i_habcode  <- f_idFieldDBTable_from_nameField("habcode",config$DBData$Data,st_drop_geometry(list_sauv$E_habbenth_sensib),"habbenth_sensib")  # Colonne du code d'habitat (EUNIS, HabRef...)

HabBenth_z <- list_sauv$E_habbenth_sensib %>% 
  dplyr::select(
    E_habcode = all_of(col_nums$i_habcode),
    E_idmesh = all_of(col_nums$i_idzhab),
    E_surfmesh = all_of(col_nums$i_surfmesh),
    E_surfhab = all_of(col_nums$i_surfhab),
    E_surfmer = all_of(col_nums$i_surfmer)
  ) %>% 
  dplyr::filter(E_idmesh %in% grid_z$idmesh)

HabBenth_z$E_surfhab[is.na(HabBenth_z$E_surfhab)] <- 0 
# HabBenth_z contient 5 col.: E_habcode, E_idmesh, E_surfmesh, E_surfhab, E_surfmer (+ geom)


## Conversion des surfaces d'habitats m2 -> km2 --------------------------------
unit_surfhab <- as.data.frame(config$DBData$Data) %>% 
  dplyr::filter(Data_type == "E" & Var_code == "surfhab")
unit_surfhab <- unit_surfhab$Unit[[1]]

if(unit_surfhab %in% c("m²", "m2", "m_2")){
  HabBenth_z$E_surfhab <- HabBenth_z$E_surfhab * 1e-06
  
  # Modifier l'unité dans config pour éviter de refaire la conversion par erreur
  config$DBData$Data[config$DBData$Data[,"Data_type"] == "E" & config$DBData$Data[,"Var_code"] == "surfhab", "Unit"] <- "Converted to km2"
}


## Joindre les sensibilites des habitats ---------------------------------------
# sensi_path = fichier ODS des sensibilites des enjeux aux pressions
if(config$Calc$agreg_sensib==1){
  Matrice_Sensi <- read_ods(sensi_path, sheet = "precaution") %>% 
    rename(code_hab = all_of(1)) %>% 
    dplyr::select(all_of(c("code_hab", P_code, paste0(P_code, "_icas"))))
} else if(config$Calc$agreg_sensib==2){
  Matrice_Sensi <- read_ods(sensi_path, sheet = "median") %>% 
    rename(code_hab = all_of(1)) %>%
    dplyr::select(all_of(c("code_hab", P_code, paste0(P_code, "_icas"))))
}

HabBenth_z <- HabBenth_z %>% 
  left_join(Matrice_Sensi, by = c("E_habcode" = "code_hab"))

##  Surface des habitats benthiques connus par maille --------------------------
# C'est la somme des surfaces de chaque polygone habitat present dans la maille

nb_poly    <- nrow(HabBenth_z)    # nombre de polygones d'habitat (un habitat par polygone, donc plusieurs par maille)

if(config$SauvRep$lrepr_hab_sensib == 0){ # Dans le cas ou on doit organiser les sensibilites (pas deja fait dans un RData)
  temp <- st_drop_geometry(HabBenth_z) %>%
    dplyr::select(
      idmesh_z = E_idmesh, 
      surfhab_z = E_surfhab, 
      surfmesh_z = E_surfmesh, 
      surfmer_z = E_surfmer
    ) %>% 
    group_by(idmesh_z) %>%
    summarise(
      surfhab_z  = sum(surfhab_z, na.rm = TRUE),
      surfmesh_z = first(surfmesh_z),
      surfmer_z  = first(surfmer_z)
    )
  
  lHabBenth_z<- as.data.frame(col_idz) %>%
    left_join(temp, by = c("col_idz" = "idmesh_z")) %>% 
    rename(idmesh_z = col_idz) %>% 
    as.list()
  lHabBenth_z$surfhab_z[is.na(lHabBenth_z$surfhab_z)] <- 0
  lHabBenth_z$P_codes_HabBenth <- colnames(as.matrix(st_drop_geometry(HabBenth_z)))
  
} else {
  filerepr_surfhab <- paste0(sauvrepdir,"/init_lHabBenth_z_",config$SauvRep$daterepr_hab_sensib,".Rdata")
  if(!file.exists(filerepr_surfhab)) stop(paste0("Ligne 377, Etape 4 - le fichier .Rdata suivant n existe pas : ",filerepr_surfhab))
  load(filerepr_surfhab)
}



# ## Verifier : Surface habitats d'une maille < surface de maille ----------------
# # Ici ça va pas : la partie "comparer à une bbox" est pas optimale. Faire autrement.
nbdec <- 6    # nombre de decimales (pour surfaces exprimees en km2)
depa  <- which(round(lHabBenth_z$surfhab_z, nbdec) > round(lHabBenth_z$surfmer_z, nbdec))
if(length(depa)>0){
  print(paste("ATTENTION : ",length(depa)," mailles sur ",nz," ont surfhab > surfmer"," AVEC MARGE ",nbdec," chiffres apres la virgule"))
  print(col_idz[depa])
}

## Rajuster surfaces d'habitats dans mailles surfhab > surfmesh ----------------
# "En attente d'une couche d'habitats plus propre", disait le commentaire d'origine

if(config$Calc$l_corr_surfhab){
  print("Reajustement des surfaces d'habitats pour mailles surfhab > surfmesh")
  lHabBenth_z$surfhab_z_NEW <- lHabBenth_z$surfhab_z
  lHabBenth_z$surfhab_z_NEW[lHabBenth_z$surfhab_z_NEW > config$surfmer_z] <- 0
  
  for(ipoly in 1:nb_poly){
    id <- which(lHabBenth_z$idmesh_z == st_drop_geometry(HabBenth_z)[[col_nums$i_idzhab]][[ipoly]])
    if(lHabBenth_z$surfhab_z[id] > config$surfmer_z[id]){
      # print(paste("# :: surface habitat du polygone corrigee OLD : ",ipoly,HabBenth_z[[i_surfhab]][[ipoly]]))   #Commenté - AE2025
      HabBenth_z[[col_nums$i_surfhab]][[ipoly]] <- HabBenth_z[[col_nums$i_surfhab]][[ipoly]] * config$surfmer_z[id] / lHabBenth_z$surfhab_z[id]
      # print(paste("#                                         NEW : ",ipoly,HabBenth_z[[i_surfhab]][[ipoly]]))   #Commenté - AE2025
      lHabBenth_z$surfhab_z_NEW[id] <- lHabBenth_z$surfhab_z_NEW[id]+ HabBenth_z[[col_nums$i_surfhab]][[ipoly]]
    }
  }
  lHabBenth_z$surfhab_z <- lHabBenth_z$surfhab_z_NEW
}


## identifier les pressions qui n'existent pas ----------------------------------
# if(config$Calc$agreg_sensib==1){
#   Matrice_Sensi <- read_ods(sensi_path, sheet = "precaution") %>% 
#     rename(code_hab = all_of(1)) %>% 
#     dplyr::select(all_of(c("code_hab", P_code, paste0(P_code, "_icas"))))
# } else if(config$Calc$agreg_sensib==2){
#   Matrice_Sensi <- read_ods(sensi_path, sheet = "median") %>% 
#     rename(code_hab = all_of(1)) %>% 
#     dplyr::select(all_of(c("code_hab", P_code, paste0(P_code, "_icas"))))
# }

codes_problematiques <- setdiff(tolower(P_code), tolower(lHabBenth_z$P_codes_HabBenth))
if(length(codes_problematiques) > 0) {
  print("ATTENTION : Ces pressions manquent dans les sensibilités des habitats (Etape 4) : ")
  stop(paste0(codes_problematiques," / "))
}


# ============================================================================= # 
# ETAPE 5 - Calcul sensibilité cumulee -----------------------------------------
# ============================================================================= # 
# Dans cette etape :
# On reconstruit un df d'habitats avec leurs sensibilites centree uniquement sur les pressions evaluees
# On limite le nombre de colonnes a l'essentiel
# On remplace les 99 par NA et les cellules avec trop peu de donnees aussi


# Methode qui exclut tous les noms de pressions (et agregees) qui ne sont pas avec demonstrateur == 1
Mat_Sensib_Pj <- HabBenth_z %>% 
  dplyr::select(E_habcode, E_idmesh, E_surfmesh, E_surfhab, E_surfmer, any_of(P_code)) %>% 
  dplyr::select(- any_of(P_code[which(P_evalDEM == 0)])) #Ici le num de ligne dans P_code renvoie au meme num dans P_evalDEM; c'est pas ideal 

pressions_manquantes <- setdiff(P_code[which(P_evalDEM == 1)], names(Mat_Sensib_Pj))
if (length(pressions_manquantes) >0) {
  Mat_Sensib_Pj[pressions_manquantes] <- 99
  print("#  ATTENTION : les codes des pressions suivantes n'ont pas d'equivalent dans la table habbenth_sensib : ")
  print(pressions_manquantes)
}

P_code_sel <- P_code[P_code %in% colnames(Mat_Sensib_Pj)] # Codes des pressions selectionnees

# On remplace les 99 par NA, puis on rajoute pour chaque pression une colonne "code pression"_nodata
# dans laquelle on met la surface des habitats dont la sensibilite est NA pour cette pression
# avant de tout grouper par ID de cellule (et surface de cellule)
Mat_Sensib_Pj <- Mat_Sensib_Pj %>% 
  mutate(across(all_of(P_code_sel), as.numeric)) %>%
  mutate(across(all_of(P_code_sel), ~ na_if(., 99))) %>%
  mutate(across(all_of(P_code_sel), ~ .x * E_surfhab / E_surfmer)) %>%
  mutate(across(all_of(P_code_sel), ~ ifelse(is.na(.x), E_surfhab, 0), .names = "{.col}_nodata")) %>%
  group_by(E_idmesh, E_surfmer) %>% 
  summarise(
    across(all_of(P_code_sel), sum, na.rm = T),
    across(ends_with("_nodata"), sum),
    .groups = "drop"
  )

seuil_nodata <- config$Calc$th_sensib/100

# Exclure les cellules où la "surface d'habitats sans info sur la sensibilite a une pression" depasse un certain seuil
Mat_Sensib_Pj <- Mat_Sensib_Pj %>% 
  mutate(across(all_of(P_code_sel),
           ~ {
             nd_val <- get(paste0(cur_column(), "_nodata"))
             ifelse(nd_val > seuil_nodata * E_surfmer, NA_real_, .x / E_surfmer)
           }
    )
  ) %>% 
  dplyr::select(- ends_with("_nodata"))



# ============================================================================= # 
# ETAPE 6 - Cartographie des activites -----------------------------------------
# ============================================================================= # 
# Dans cette etape :
# On cree le df act_table des jeux de donnees d'activites et codes d'activite correspondants
# On met chaque donnee dans une colonne du df A_zi, qu'on log-normalise
# On cree le df act_data_corresp qui montre les correspondances entre codes d'activite et jeux de donnees 
# (par exemple quand un meme jeu de donnees correspond a 2 codes d'activite differents)
# + calcul des indices IMA 1 et 2
# 
# champ_id_data_A : Nom du champ dans lequel on a mis l'ID de la cellule de carroyage, le meme pour tous les champs
# A_zi : table des intensites de chaque activite (non normalisee) par cellule de carroyage
# A_zi_lognorm : table des intensites log-normees des activites par cellule



ni <- config$ni                                             # nombre d activites dans la matrice
nbcol <- length(grep("Code",colnames(config$DBData$Data)))  # nombre de colonnes de codes a parcourir


act_table <- as.data.frame(config$DBData$Data) %>% 
  dplyr::filter(Data_type == "A") %>%
  dplyr::filter(if_any(c(Code1, Code2, Code3, Code4, Code5), ~ . %in% config$A_code)) %>% 
  mutate(codes = pmap(list(Code1, Code2, Code3, Code4, Code5), c)) %>%
  mutate(codes = map(codes, ~ .x[!is.na(.x)])) %>% 
  mutate(codes = map(codes, ~ .x[order(match(.x, config$A_code))])) %>% 
  mutate(codes = map_chr(codes, ~ str_c(.x, collapse = "...")))

# Ici act_table contient uniquement les activites de la liste avec un code pour
# celles qui partagent un jeu de donnees (ex.: "SenneDanoise...SenneEcossaise")

# Pour chaque ligne, on a une activite/un agregat d'activites
A_zi <- data.frame(idmesh = grid_z$idmesh)

for (donnee_i in seq_len(nrow(act_table))) {
  Ai_table <- list_sauv[[paste0("A_", act_table$Table_code[donnee_i])]] %>% 
    st_drop_geometry() %>% 
    dplyr::select(all_of(champ_id_data_A), all_of(act_table$Field[donnee_i])) %>% 
    dplyr::filter(.data[[champ_id_data_A]] %in% grid_z$idmesh)
  
  colnames(Ai_table) <- c("mesh", act_table$codes[donnee_i])
  
  A_zi <- A_zi %>% 
    left_join(Ai_table, by = c("idmesh"="mesh"))
  
  if(config$Outputs$lplot_Ai){
    colnames(Ai_table)[1] <- "mesh"
    title <- paste0("Intensite ", act_table$Data_code[donnee_i]," (",act_table$Unit[donnee_i],")")
    checkplot <- Plot_VARz_grid2D(Ai_table,grid_z,config, act_table$Code1[donnee_i],title)
  }
}

## Stockage des activites normalisees dans une matrice -------------------------
A_zi_lognorm <- A_zi %>%
  mutate(across(-idmesh, ~f_lognorm(.))) %>% 
  rename_with(~ paste0(., "_norm"), -idmesh) %>% 
  rename(mesh = idmesh)

## Table associant chq Act avec la donnee qui la contient (agreg. comprise) ----
act_data_corresp <- act_table %>% 
  dplyr::select(starts_with("code")) %>% 
  rename("data_col" = "codes") %>% 
  pivot_longer(cols = starts_with("Code"), names_to = "CodeNum", values_to = "Code") %>%
  dplyr::filter(!is.na(Code)) %>%
  dplyr::select(-"CodeNum") %>% 
  dplyr::relocate(Code) 

## Calcul des 2 Indices Multi-activites ----------------------------------------
# option 1 = denombrement des mailles dont Ai > 0.
# option 2 = somme des intensitees log_transformees et normalisees entre 0 et 1
IMA <- A_zi %>% 
  mutate(IMA_option1 = rowSums(dplyr::select(., -1) > 0,na.rm = T) ) %>% 
  dplyr::select(idmesh, IMA_option1) %>% 
  left_join(A_zi_lognorm, by = c("idmesh" = "mesh")) %>% 
  mutate(IMA_option2 = rowSums(across(-c(idmesh, IMA_option1)), na.rm = TRUE)) %>% 
  dplyr::select(idmesh, IMA_option1, IMA_option2)
IMA[,3] <- as.numeric(IMA[,3])/max(as.numeric(IMA[,3]), na.rm=T) 

# Creation du plot de l'option 1
VARz <- data.frame(
  mesh = IMA$idmesh, 
  IMA_option1 = as.numeric(IMA$IMA_option1)
)
title <- paste0("Indice multi-activites methode 1 ")
checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, "IMA_option1",title)


# Creation du plot de l'option 2
VARz <- data.frame(
  mesh = IMA$idmesh, 
  IMA_option2 = as.numeric(IMA$IMA_option2)
)
title <- paste0("Indice multi-activites methode 2 ")
checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, "IMA_option2",title)


# ============================================================================= # 
# ETAPE 7 - Cartographie des pressions -----------------------------------------
# ============================================================================= # 
# Dans cette etape :
# 
# 
# 

## Version reduite de matrice Activites-Pressions ------------------------------
MatAP <- as.data.frame(config$Mat_AP) 
colnames(MatAP) <- MatAP[1,]
colnames(MatAP)[1:4] <- c("Type","Code_A","Activite_nom","Phase")

MatAP <- MatAP %>% 
  dplyr::filter(Type == "Final") %>% 
  column_to_rownames(var = "Code_A") %>% 
  dplyr::select(-c("Type","Activite_nom","Phase")) %>% 
  as.matrix()

Mat_Pj_z <- data.frame(idmesh = grid_z$idmesh)

## Calcul des intensites de pressions a partir des activites -------------------
# Liste des donnees de pressions mesurees directement
data_P <- as.data.frame(config$DBData$Data) %>% 
  dplyr::filter(Data_type == "P") %>% 
  mutate(Tab_listsauv = paste0("P_", Table_code)) %>% 
  dplyr::select(Tab_listsauv, Field, starts_with("Code")) %>% #, data_col = P_all) %>%
  pivot_longer(cols = starts_with("Code"), names_to = "CodeNum", values_to = "Code") %>%
  dplyr::filter(!is.na(Code)) %>%
  dplyr::select(-"CodeNum") %>%
  dplyr::relocate(Code)

# if(any(duplicated(data_P$Code))){}

# Liste des pressions (apres agregation) visees par l'analyse 
# Note : laisse hors des pressions agregees toute pression mesuree directement
press_table <- data.frame(
  Code = config$P_code, 
  Name = config$P_name, 
  Eval_DEM = config$P_evalDEM, 
  ModCalc = config$P_modcalc,
  CodeAgrege = config$P_code_ag,
  NomAgrege = config$P_name_ag
) %>% 
  dplyr::filter(Eval_DEM == 1) %>%
  mutate(CodeAgrege = if_else(is.na(CodeAgrege) | ModCalc == 2, Code, CodeAgrege),
         NomAgrege = if_else(is.na(NomAgrege) | ModCalc == 2, Name, NomAgrege)) %>% 
  group_by(CodeAgrege) %>%
  mutate(CodeAll = list(Code)) %>%
  slice_head(n = 1) %>%
  ungroup()

# Mise en carroyage de l'intensite de chaque pression
ampli_max <- data.frame(CodeAgrege = press_table$CodeAgrege, ampli_max = 0)

for(pression_j in seq_along(press_table$CodeAgrege)){
  if(press_table$ModCalc[pression_j] == 1) {   # Pression calculee avec activites
    # Pour chaque pression finale (cad apres les agregations), on recupere :
    # Si la pression est individuelle, juste sa colonne dans la matrice AP
    # Si la pression est agregee (ex.: Abrasion = [Abras.surf. && Abras.prof.]), toutes les colonnes dans
    # la matrice AP avant de ne garder que la plus grande amplitude entre les deux
    MiniMatAP <- as.data.frame(MatAP) %>% 
      dplyr::select(all_of(press_table$CodeAll[[pression_j]])) %>%
      mutate(across(everything(), as.numeric)) %>% 
      rownames_to_column(var = "rowname") %>% 
      rowwise() %>%
      mutate(max = max(c(0,c_across(-rowname)), na.rm = TRUE)) %>%
      ungroup() %>% 
      dplyr::select(max, rowname)
    
    # On utilise ensuite la table de correspondance entre code d'activite et 
    # donnees d'intensite d'activite pour associer ces colonnes a l'amplitude de
    # la pression qu'elles emettent (donc pour la pression_j en cours)
    if(config$Calc$agreg_sensib==1){ # precaution
      amplitudes <- act_data_corresp %>% 
        left_join(MiniMatAP, by = c("Code" = "rowname")) %>% 
        group_by(data_col) %>% 
        summarise(amp = max(max)) %>% 
        ungroup()
    } else if(config$Calc$agreg_sensib==2){ # median
      amplitudes <- act_data_corresp %>% 
        left_join(MiniMatAP, by = c("Code" = "rowname")) %>% 
        group_by(data_col) %>% 
        summarise(amp = mean(max)) %>% 
        ungroup()
    }
    
    amplitudes <- setNames(amplitudes$amp, amplitudes$data_col)
    names(amplitudes) <- paste0(names(amplitudes), "_norm")
    
    ampli_max[pression_j, "ampli_max"] <- max(c(0,amplitudes), na.rm = T) # On remplit le vecteur d'amplitudes max
    
    Mat_PjAi_z <- A_zi_lognorm %>%
      mutate(across(
        .cols = names(amplitudes), 
        .fns = ~ .x * amplitudes[cur_column()]
      )) %>% 
      rowwise() %>%
      mutate(sum = sum(c_across(-mesh), na.rm = TRUE)) %>%
      ungroup() %>% 
      dplyr::select(mesh, sum)
    
    if(all(Mat_PjAi_z$sum == 0)) {Mat_PjAi_z$sum <- rep(NA, nrow(Mat_PjAi_z))}
    colnames(Mat_PjAi_z) <- c("idmesh", press_table$CodeAgrege[[pression_j]])
    
    if(config$Outputs$lplot_Pj){
      nameVARz <- paste0("Pj_multiAi_",press_table$CodeAgrege[pression_j])
      VARz <- Mat_PjAi_z %>% dplyr::filter(idmesh %in% Mat_Pj_z$idmesh) %>% rename("mesh" = "idmesh")
      title <- paste("Pression multi-activites :",press_table$NomAgrege[pression_j])
      checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)
    }
    
  } else if (press_table$ModCalc[pression_j] == 2) { # Pressions mesurees directement
    Mat_PjAi_z <- data.frame(idmesh = grid_z$idmesh)
    temp <- data_P[data_P$Code == press_table$CodeAgrege[pression_j], ]
    
    for(donnee_i in seq_along(temp[[1]])) { 
      Mat_PjAi_z <- Mat_PjAi_z %>%      # Le code barbare ci-dessous selectionne les colonnes appropriees dans list_sauv
        left_join(
          st_drop_geometry(list_sauv[[ temp[["Tab_listsauv"]][[donnee_i]] ]] [c(champ_id_data_A, temp[["Field"]][[donnee_i]] )]),
          by = setNames(champ_id_data_A, "idmesh") 
        )
    }
    
    Mat_PjAi_z <- Mat_PjAi_z %>%
      rowwise() %>%
      mutate(sum = sum(c_across(-idmesh))) %>%
      ungroup() %>% 
      dplyr::select(idmesh, sum)
    colnames(Mat_PjAi_z) <- c("idmesh", press_table$CodeAgrege[[pression_j]])
    
    ampli_max[pression_j, "ampli_max"] <- as.data.frame(MatAP) %>% 
      dplyr::select(all_of(press_table$CodeAll[[pression_j]])) %>%
      mutate(across(everything(), as.numeric)) %>% 
      summarise(max = max(c_across(everything()))) %>% 
      as.numeric()
    
    if(is.na(ampli_max[pression_j, "ampli_max"]) || ampli_max[pression_j, "ampli_max"] == 0){
      # Si on a une pression qui n'est pas dans la Matrice AP ou dont les valeurs sont 0/NA car toujours
      # mesuree directement ==> Amplitude = valeur d'amplitude la plus frequente
      ampli_max[pression_j, "ampli_max"] <- as.vector(MatAP[which(MatAP>0)]) %>% 
        na.omit() %>% table() %>%
        sort(decreasing = TRUE) %>%
        head(1) %>% names() %>%
        as.numeric() 
    }
    
    if(config$Outputs$lplot_Pj){
      nameVARz <- paste0("Pj_multiAi_",press_table$CodeAgrege[pression_j])
      VARz <- Mat_PjAi_z %>% dplyr::filter(idmesh %in% Mat_Pj_z$idmesh) %>% rename("mesh" = "idmesh")
      title <- paste("Pression multi-activites :",press_table$NomAgrege[pression_j])
      checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)
    }
  }
  # Ajout de l'intensite de cette pression au df complet
  Mat_Pj_z <- Mat_Pj_z %>% 
      left_join(Mat_PjAi_z, by="idmesh")
}


## Normalisation log -----------------------------------------------------------
Mat_Pj_norm_z <- Mat_Pj_z %>%
  mutate(across(-idmesh, ~f_lognorm(.))) %>% 
  rename_with(~ paste0(., "_norm"), -idmesh) %>% 
  rename(mesh = idmesh)

## Calcul des indices IPC ------------------------------------------------------
# Option 1: Nombre de pressions presentes par maille
IPC_z <- Mat_Pj_norm_z %>% 
  mutate(IPC_option1 = rowSums(dplyr::select(., -1) > 0,na.rm = T) ) %>% 
  mutate(IPC_option2 = rowSums(across(-c(mesh, IPC_option1)), na.rm = TRUE)) %>% 
  dplyr::select(mesh, IPC_option1, IPC_option2)

# production d'un fichier shp si l'option a ete parametree
VARz <- cbind.data.frame(grid_z$idmesh, as.numeric(IPC_z[,2])) ; colnames(VARz) <- c("mesh","IPC_option1")
title <- ("Indice de pressions cumulees Option 1")
checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, "IPC_option1",title)

VARz <- cbind.data.frame(grid_z$idmesh, as.numeric(IPC_z[,3])) ; colnames(VARz) <- c("mesh","IPC_option2")
title <- ("Indice de pressions cumulees Option 2")
checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, "IPC_option2",title)


## Ponderation par amplitude max -----------------------------------------------
ampli_max <- setNames(ampli_max$ampli_max, ampli_max$CodeAgrege)
names(ampli_max) <- paste0(names(ampli_max), "_norm")

# multiplier chaque colonne par son amplitude max
Mat_Pj_pondere <- Mat_Pj_norm_z %>% 
  mutate(across(
    .cols = names(ampli_max), 
    .fns = ~ .x * ampli_max[cur_column()]
  ))


# ============================================================================= # 
# ETAPE 8 - Risque d'exposition -----------------------------------------
# ============================================================================= # 
# Dans cette etape :
# REX (Risque d'exposition a une pression P) = pression * surface d'habitats dans la maille
# 
# 

Mat_REX_PjE1_z <- data.frame(idmesh = grid_z$idmesh) %>% 
  cbind(lHabBenth_z$surfhab_z) %>% 
  rename(surfhab = 2) %>% 
  left_join(Mat_Pj_z, by = "idmesh") %>% 
  mutate(across(-c(idmesh, surfhab), ~ .x * surfhab)) 

if(config$Outputs$lplot_REX_Pj){
  cols_to_plot <- setdiff(colnames(Mat_REX_PjE1_z), c("idmesh", "surfhab"))
  for (pression_j in cols_to_plot) {
    VARz <- data.frame(mesh = grid_z$idmesh, Mat_REX_PjE1_z[[pression_j]])
    title <- paste("REX E1 pression", P_name[which(P_code == pression_j)])
    Plot_VARz_grid2D(VARz, grid_z, config, pression_j, title)
  }
}
Mat_REX_PjE1_z <- Mat_REX_PjE1_z %>% 
  rename_with(~ paste0("REX_E1_", .), -c(idmesh, surfhab)) %>% 
  dplyr::select(-surfhab)

REXC_E1 <- data.frame(
  mesh = grid_z$idmesh,
  REXC_E1 = rowSums(Mat_REX_PjE1_z[, -1, drop = FALSE], na.rm = TRUE)
)

VARz <-  REXC_E1
nameVARz <- "REXC_E1"
title <- "REX pressions concommitantes sur E1"
checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)


# ============================================================================= # 
# ETAPE 9 - Risque d'effets cumules -----------------------------------------
# ============================================================================= # 
# Dans cette etape :
# REFC = Sensibilites totales des cellules aux pressions * Intensites ponderees des pressions
# Puis normalisation lineaire
# 

# La surface d'habitats est deja prise en compte dans calcul sensibilite cumulee
# Donc on ne prend pas l'exposition, mais Pj normalisee, pour calculer le risque d'effets REF_Pj 
# Normalisation des valeurs de sensibilite totale de cellule (par pression)
Mat_Sensib_Pj_in <- Mat_Sensib_Pj %>% 
  st_drop_geometry() %>% 
  dplyr::select(-E_surfmer) %>% 
  f_norm()

# Pour obtenir les effets de chaque pression on multiplie sensibilite totale normalisee (Mat_Sensib_Pj_in)
# a chaque pression par l'intensite normalisee ponderee (Mat_Pj_pondere) de chaque pression (donc colonne par colonne)
Mat_REF_PjE1_z <- Mat_Sensib_Pj_in %>%
  inner_join(Mat_Pj_pondere, by = c("E_idmesh" = "mesh")) %>%
  mutate(across(
    .cols = names(Mat_Sensib_Pj_in)[-1],                # Multiplier chaque colonne venant de MatSensibPj
    .fns = ~ . * get(paste0(cur_column(), "_norm"))  # par la colonne du meme nom avec le suffixe _norm
  )) %>%
  dplyr::select(names(Mat_Sensib_Pj_in)) %>% 
  rename_with(.fn = ~ paste0("ref_e1_", .x), .cols = -E_idmesh) %>%   # Renommer toutes les colonnes retenues
  rename(mesh = E_idmesh)


# Calcul du risque d'effet
REFC_E1 <- Mat_REF_PjE1_z %>%
  rowwise() %>%
  mutate(
    REFC_E1 = if_else(all(is.na(c_across(-mesh))), NA_real_, sum(c_across(-mesh), na.rm = TRUE))
  ) %>%
  ungroup() %>%
  dplyr::select(mesh, REFC_E1)

REFC_E1$REFC_E1 <- as.numeric(REFC_E1$REFC_E1)/max(as.numeric(REFC_E1$REFC_E1), na.rm=T)

# export sous forme de Geopackage
VARz <-  REFC_E1
nameVARz <- "REFC_E1"
title <- "REF pressions concomitantes sur E1"
checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)

# 
# # ============================================================================= # 
# # ETAPE 10 - Indice de confiance -----------------------------------------
# # ============================================================================= # 
# # Dans cette etape :
# # 
# # 
# # 
# 
# if(config$SauvRep$shape_ic) {
# 
#   ## 10.1: Indices de qualite des donnees activees -----------------------------
#   
#   if(config$SauvRep$lrepr_iq==0){
#     
#     # ouverture des tables et remplissage de la matrice A_iz_iq
#     A_zi_iq <- data.frame(idmesh = grid_z$idmesh)
#     
#     for (activite_i in rownames(act_table)) {
#       Ai_table <- list_sauv[[paste0("A_",act_table[activite_i,"Table_code"])]] %>% 
#         st_drop_geometry() %>% 
#         dplyr::select(all_of(c(champ_id_data_A, paste0(act_table[activite_i,"Field"], "_iq")))) %>% 
#         rename(idmesh = all_of(champ_id_data_A)) %>% 
#         dplyr::filter(idmesh %in% grid_z$idmesh)
#       
#       A_zi_iq <- A_zi_iq %>% 
#         left_join(Ai_table, by = "idmesh")
#       # Attention, si on reutilise la meme donnee 3 fois, on aura 3 fois la colonne nbh_sennedanoise_iq... Est-ce ce qu'on veut
#     }
#     
#     
#     if(config$SauvRep$lsauv_iq==1){
#       filesauv_iq <- paste0(sauvrepdir,"/IQ_",datesauv,".Rdata")
#       save(A_zi_iq, file = filesauv_iq)
#     }
#   } else {  
#     filerepr_iq <- paste0(sauvrepdir,"/IQ_",config$SauvRep$daterepr_iq,".Rdata")
#     load(filerepr_iq)
#   }
#   
#   
#   
#   
#   ## ---------------------------------------------------------------------------------------------.
#   ## DEUXIEME ETAPE: GESTION DE LA MATRICE ACTIVITE PRESSION ET DES INDICES DE CONFIANCES ASSOCIES 
#   ## ---------------------------------------------------------------------------------------------.
#   
#   # -- creation d'un dataframe Matrice AP pour faciliter les calculs
#   MatAP_iq <- config$Mat_AP[vec_lines,ideb_P:ifin_P]
#   colnames(MatAP_iq) <- list_Pcode_MatAP ; 
#   rownames(MatAP_iq) <- list_Acode_MatAP 
#   ni_MatAP <- dim(MatAP_iq) [1] ; nj_MatAP <- dim(MatAP_iq) [2] # Nombre de colonnes et de lignes de la matrice
#   for(i in 1:ni_MatAP){
#     for(j in 1:nj_MatAP){
#       if(MatAP_iq[i,j]=="non" & !is.na(MatAP_iq[i,j])) {
#         MatAP_iq[i,j] <- 0 #On remplace les tirets par des 0 (pour faciliter la lecture de la matrice)
#       }
#       if(MatAP_iq[i,j]=="oui" & !is.na(MatAP_iq[i,j])) {
#         MatAP_iq[i,j] <- 1 #De meme on remplace les x par des 1
#       } 
#       if(is.na(MatAP_iq[i,j]) | (MatAP_iq[i,j] != 0 & MatAP_iq[i,j] != 1)) {
#         MatAP_iq[i,j] <- NA # On met des NA au lieu de ND (que R ne sait pas interpreter)
#       }
#     }
#   }
#   MatAP_iq <- as.data.frame(MatAP_iq)
#   for(i in 1:dim(MatAP_iq)[2]){
#     MatAP_iq[,i] <- as.numeric(as.character(MatAP_iq[,i]))
#   }
#   
#   # Extraction des indices de confiance de la matrice A/P
#   vec_lines_IC <- which(config$Mat_AP[,1]=="IC") #numeros de ligne du feuillet matAP contenant les indices de confiance
#   MatAP_IC <- config$Mat_AP[vec_lines_IC,ideb_P:ifin_P]
#   colnames(MatAP_IC) <- list_Pcode_MatAP ; rownames(MatAP_IC) <- list_Acode_MatAP # On utilise les codes pressions et activites comme noms de ligne et de colonne
#   MatAP_IC[MatAP_IC=="ND"]<-0 # Lorsque l'indice de confiance est inconnu, on lui donne la valeur de 0
#   MatAP_IC[MatAP_iq=="ND"]<-0 # Lorsque le lien AP est inconnu, on donne a l'IC la valeur de 0
#   MatAP_IC <- as.data.frame(MatAP_IC) #conversion en data.frame
#   for(i in 1:dim(MatAP_IC)[2]){ #Boucle sur les pressions pour avoir des valeurs en format numerique
#     MatAP_IC[,i] <- as.numeric(as.character(MatAP_IC[,i]))
#   }
#   
#   
#   # Double boucle permettant de reduire les matrices creees ci-dessus aux pressions et activites utilisees dans l'analyse
#   for(i in 2:dim(A_zi)[2]){ # Boucle sur les activites
#     iA_code <- colnames(A_zi)[i] #extraction du code de l'activite
#     list_iA_ag <- config$A_code[which(config$Aiag==iA_code)]  # Code pour les activites agreges
#     n_act <- length(list_iA_ag) # Nombre d'activites agregees (si pas d'agregation, =1)
#     sub <- MatAP_iq[which(rownames(MatAP_iq) %in% list_iA_ag),] # Extraction dans la matrice AP des lignes reliees aux activites (plusieurs si agregation)
#     subIC <- MatAP_IC[which(rownames(MatAP_IC) %in% list_iA_ag),] # Idem avec la matrice des indices de confiance
#     sub_final <- as.data.frame(matrix(data=0, nrow=1, ncol=length(vec_P_code_eval), dimnames = list(iA_code, vec_P_code_eval))) # Vecteur qui contiendra la ligne finale de la matrice reduite
#     subIC_final <- as.data.frame(matrix(data=0, nrow=1, ncol=length(vec_P_code_eval), dimnames = list(iA_code, vec_P_code_eval))) # Idem pour la matrice des indices de confiance
#     for(j in 1:length(vec_P_code_eval)){ # Boucle sur les pressions prises en compte dans l'evaluation
#       iP_code <- vec_P_code_eval[j] # Code correspondant a j
#       if(iP_code %in% config$P_code) { # Si il ne s'agit pas d'une pression agregee
#         list_iP_ag <- P_code[which(P_code==iP_code)] # On ne conserve que le code d'origine
#       } else { # Sinon: cela signifie que l'on a affaire a une pression agregee
#         list_iP_ag <- config$P_code[which(config$P_code_ag==iP_code)] # Alors on enregistre tous les codes des pressions agregees
#       }
#       if(length(which(is.na(unlist(sub[,which(colnames(sub) %in% list_iP_ag)]))))==0){ # Si le lien AP est connu dans au moins un des couples AP (plusieurs couples si a ou p agregees)
#         sub_final[which(colnames(sub_final)==iP_code)] <- max(unlist(sub[,which(colnames(sub) %in% list_iP_ag)]), na.rm = T) # Les liens AP sont de 1 ou de 0. Ainsi, si au moins des liens est positif, on considere que le lien existe
#       } else {
#         #sub_final[which(colnames(sub_final)==iP_code)] <- NA 
#         sub_final[which(colnames(sub_final)==iP_code)] <- 0 # MAJ Lodier 02 04 2024 ; NA fait planter donc 0
#         # Si tous les liens sont inconnus, on attribue la valeur de NA
#       }     # Dans le cas de pressions ou d'activites agregees, on fait une moyenne des indices de confiance 
#       subIC_final[which(colnames(subIC_final)==iP_code)] <- mean(unlist(subIC[,which(colnames(subIC) %in% list_iP_ag)]))
#     }
#     if(i==2){
#       MatAP_IC_sub <- subIC_final
#       MatAP_sub <- sub_final
#     } else {
#       MatAP_sub <- rbind.data.frame(MatAP_sub, sub_final)
#       MatAP_IC_sub <- rbind.data.frame(MatAP_IC_sub, subIC_final)
#     }
#     rownames(MatAP_IC_sub)[i-1] <- iA_code
#   }
#   
#   ## ---------------------------------------------------------------------------------------------.
#   ## TROISIEME ETAPE: CALCUL D'UN INDICE DE CONFIANCE ASSOCIE A CHAQUE PRESSION 
#   ## ---------------------------------------------------------------------------------------------.
#   ##                                  ------                                        ##
#   ## --- Premiere sous etape: indice de confiance dans les relations theoriques --- ##
#   
#   
#   # creation et remplissage de la matrice contenant les IC de l'intensite de chaque pression dans chaque maille. 
#   ICAP_Pj <- cbind.data.frame(as.matrix(vec_zid), matrix(data=0, nrow=nz, ncol=dim(Mat_Pj_z)[2]-1))
#   colnames(ICAP_Pj) <- colnames(Mat_Pj_z)
#   print("Remplissage de la matrice contenant les IC de l'intensite de chaque pression dans chaque maille")
#   for(i in 1:nz){ # Boucle sur toutes les mailles
#     coef <- as.numeric(A_zi_lognorm[i,2:dim(A_zi_lognorm)[2]]) # On extrait l'intensite des differentes activite pour chaque maille
#     for(j in 2:dim(ICAP_Pj)[2]){ # Boucle sur les pressions
#       val <- MatAP_IC_sub[,which(colnames(MatAP_IC_sub)==colnames(ICAP_Pj)[j])] #Vecteur contenant les IC associe au lien entre les differentes activites et la pression j
#       if(sum(coef, na.rm=TRUE)==0){ # Si toutes les activites sont nulles
#         ICAP_Pj[i,j] <- 1 # Alors l'indice de confiance prend la valeur maximale
#       } else {
#         # Sinon fait une moyenne ponderee par l'intensite normalisee de l'activite
#         # On redivise aussi par 5 pour ramener l'IC entre 0 et 1
#         ICAP_Pj[i,j] <- sum(val*coef, na.rm=TRUE)/(5*sum(coef, na.rm=TRUE)) 
#       }
#     }
#   }
#   
#   ##                                       ------                                           ##
#   ## --- Deuxieme sous etape: indice de confiance dans la qualite des donnees activites --- ##
#   
#   
#   ## Calcul d'un indice de confiance dans la qualite des donnees. 
#   # creation de la matrice qui contiendra l'indice
#   ICQAP_Pj <- cbind.data.frame(as.matrix(vec_zid), matrix(data=1, nrow=nz, ncol=dim(Mat_Pj_z)[2]-1))
#   colnames(ICQAP_Pj) <- colnames(Mat_Pj_z)
#   ICQAP_Pj[,1] <- as.character(ICQAP_Pj[,1])
#   for(i in 2:dim(ICQAP_Pj)[2]){ #Boucle sur les pressions pour convertir au format numerique
#     ICQAP_Pj[,i] <- as.numeric(as.character(ICQAP_Pj[,i]))
#   }
#   # Boucle sur toutes les mailles pour remplir la matrice
#   for (i in 1:nz){
#     for(j in 2:dim(ICQAP_Pj)[2]){ # Boucle sur les pressions
#       if(sum(MatAP_sub[,j-1], na.rm=T) > 0){ # Si la pression est liee a au moins une activite --> moyenne ponderee des IQ
#         ICQAP_Pj[i, j] <- sum(MatAP_sub[,j-1]*A_zi_iq[i,2:dim(A_zi_iq)[2]], na.rm = T)/(5*sum(MatAP_sub[,j-1], na.rm=T))
#       }
#     }
#   }
#   
#   
#   ##                                             ------                                              ##
#   ## --- Troisieme sous etape: synthese --> calcul de l'indice de confiance dans chaque pression --- ##
#   
#   
#   IC_Pj <- cbind.data.frame(as.matrix(vec_zid), matrix(data=1, nrow=nz, ncol=dim(Mat_Pj_z)[2]-1)) # creation de la matrice qui contiendra l'indice
#   colnames(IC_Pj) <- colnames(Mat_Pj_z)
#   IC_Pj[,1] <- as.character(IC_Pj[,1])
#   for(i in 2:dim(IC_Pj)[2]){ #Boucle sur les pressions pour convertir au format numerique et calculer l'indice
#     IC_Pj[,i] <- as.numeric(as.character(IC_Pj[,i]))
#     IC_Pj[,i] <- ICAP_Pj[,i]*ICQAP_Pj[,i] # Dans tous les cas de figure, simple produit des deux indices precedemment calcules
#   }
#   
#   
#   ## ---------------------------------------------------------------------------------------------.
#   ## QUATRIEME ETAPE: CALCUL D'UN INDICE DE CONFIANCE DANS LA SENSIBILITE A CHAQUE PRESSION
#   ## ---------------------------------------------------------------------------------------------.
#   
#   ##                                                ------                                                 ##
#   ## --- Premiere sous etape: indice de confiance dans les relations theoriques habitats - sensibilite --- ##
#   
#   # creation de la matrice contenant l'information 
#   ICRTHP_Pj <- cbind.data.frame(as.matrix(vec_zid), matrix(data=0, nrow=nz, ncol=dim(Mat_Pj_z)[2]-1))
#   colnames(ICRTHP_Pj) <- colnames(Mat_Pj_z)
#   ICRTHP_Pj[,1] <- as.character(ICRTHP_Pj[,1])
#   for(i in 2:dim(ICRTHP_Pj)[2]){ #Boucle sur les pressions pour convertir au format numerique
#     ICRTHP_Pj[,i] <- as.numeric(as.character(ICRTHP_Pj[,i]))
#   }
#   id_mesh <- which(colnames(HabBenth_z)=="id2") #numero de colonne des identifiants dans la table habitats
#   col_IC <- paste(tolower(colnames(Mat_Pj_z)[2:dim(Mat_Pj_z)[2]]),"icas",sep="_") # vecteur contenant les noms de colonne des indices de confiance
#   col_sensib <- paste0(tolower(colnames(Mat_Pj_z)[2:dim(Mat_Pj_z)[2]]),suf_sensib) # vecteur contenant les noms de colonne des scores de sensibilites
#   print("calcul d'un indice de confiance pour les relations theoriques habitats - pression")
#   for(i in 1:nz){ # Boucle sur toutes les mailles
#     n_hab <- length(which(st_drop_geometry(HabBenth_z)[,id_mesh]==ICRTHP_Pj[i,1])) # Nombre d'habitats dans la maille
#     if(n_hab>0){ # Si il y a au moins un habitat dans la maille
#       for(j in 2:dim(ICRTHP_Pj)[2]){ # Boucle sur les pressions, on commence par extraire pur tous les habitats la surface des heb, la surface maritime, la sensibilite et l'indice de confiance
#         sub <- st_drop_geometry(HabBenth_z)[which(st_drop_geometry(HabBenth_z)[,id_mesh]==ICRTHP_Pj[i,1]),names(HabBenth_z)%in%c("surfmer","surfhab_cel",col_IC[j-1],col_sensib[j-1])]
#         sub[which(sub[,3]==99),4] <- 0 ##Si la sensibilite de l'habitat est inconnu, alors l'IC pour ce couple habitat pression  est nul
#         # Au lieu de diviser la somme ponderee par la somme des habitats, on divise par la surface maritime (attribue un zero sur les zones marines ou on ne connait pas l'habitat)
#         # Division par 5 pour ramener l'indice entre 0 et 1
#         ICRTHP_Pj[i,j] <- sum(sub[,2]*sub[,4])/(5*sub[1,1])
#       }
#     }
#   }
#   
#   
#   ##                                       ------                                          ##
#   ## --- Deuxieme sous etape: indice de confiance dans la qualite des donnees habitats --- ##
#   
#   # creation de la matrice contenant l'information 
#   ICQHP_Pj <- cbind.data.frame(as.matrix(vec_zid), matrix(data=0, nrow=nz, ncol=1))
#   colnames(ICQHP_Pj) <- c("mesh", "ICQHP")
#   ICQHP_Pj[,1] <- as.character(ICQHP_Pj[,1])
#   ICQHP_Pj[,2] <- as.numeric(ICQHP_Pj[,2]) #Boucle sur les pressions pour convertir au format numerique
#   print("calcul d'un indice de confiance pour la qualite des donnees habitats")
#   for(i in 1:nz){ # Boucle sur toutes les mailles
#     n_hab <- length(which(st_drop_geometry(HabBenth_z)[,id_mesh]==ICQHP_Pj[i,1])) # Nombre d'habitats dans la maille
#     if(n_hab>0){ # Si il y a au moins un habitat dans la maille
#       sub <- st_drop_geometry(HabBenth_z)[which(st_drop_geometry(HabBenth_z)[,id_mesh]==ICQHP_Pj[i,1]),which(colnames(HabBenth_z) == "hab_iq")] #on extrait les indices de qualite des habitats
#       ICQHP_Pj[i,2] <- sub[1]
#     }
#   }
#   
#   
#   
#   ##                                             ------                                              ##
#   ## --- Troisieme sous etape: synthese --> calcul de l'indice de confiance dans chaque pression --- ##
#   
#   ICHP_Pj <- cbind.data.frame(as.matrix(vec_zid), matrix(data=1, nrow=nz, ncol=dim(Mat_Pj_z)[2]-1)) # creation de la matrice qui contiendra l'indice
#   colnames(ICHP_Pj) <- colnames(Mat_Pj_z)
#   ICHP_Pj[,1] <- as.character(ICHP_Pj[,1])
#   for(i in 2:dim(ICHP_Pj)[2]){ #Boucle sur les pressions pour convertir au format numerique et calculer l'indice
#     ICHP_Pj[,i] <- as.numeric(as.character(ICHP_Pj[,i]))
#     ICHP_Pj[,i] <- (ICRTHP_Pj[,i]*ICQHP_Pj[,2])/5 # Dans tous les cas de figure, simple produit des deux indices precedemment calcules
#   }
#   
#   ## ---------------------------------------------------------------------------------------------.
#   ## CINQUIEME ETAPE: CALCUL D'UN INDICE DE CONFIANCE DANS LE RISQUE D'EFFET ASSOCIE A CHAQUE PRESSION
#   ## ---------------------------------------------------------------------------------------------.
#   
#   
#   # creation de la matrice contenant l'information 
#   ICREFC_Pj <- cbind.data.frame(as.matrix(vec_zid), matrix(data=NA, nrow=nz, ncol=dim(Mat_Pj_z)[2]-1))
#   colnames(ICREFC_Pj) <- colnames(Mat_Pj_z)
#   ICREFC_Pj[,1] <- as.character(ICREFC_Pj[,1])
#   for(j in 2:dim(ICREFC_Pj)[2]){ #Boucle sur les pressions 
#     ICREFC_Pj[,j] <- as.numeric(as.character(ICREFC_Pj[,j])) # Conversion au format numerique
#     
#     # Si la sensibilite est nulle et que la pression est non nulle, l'indice prend la valeur de l'ICHP_Pj
#     if(length(which(Mat_Sensib_Pj[,j]==0 & Mat_Pj_z[,j] > 0)) > 0){
#       ICREFC_Pj[which(Mat_Sensib_Pj[,j]==0 & Mat_Pj_z[,j] > 0),j] <- ICHP_Pj[which(Mat_Sensib_Pj[,j]==0 & Mat_Pj_z[,j] > 0),j]
#     }
#     
#     # Si la sensibilite est non nulle et que la pression est nulle, l'indice prend la valeur de l'IC_Pj
#     if (length(which(Mat_Sensib_Pj[,j] > 0 & Mat_Pj_z[,j] == 0)) > 0) {
#       ICREFC_Pj[which(Mat_Sensib_Pj[,j] > 0 & Mat_Pj_z[,j] == 0),j] <- IC_Pj[which(Mat_Sensib_Pj[,j] > 0 & Mat_Pj_z[,j] == 0),j]
#     }
#     
#     # Si la sensibilite est non nulle et que la pression est non nulle, on multiplie les deux indices
#     if (length(which(Mat_Sensib_Pj[,j] > 0 & Mat_Pj_z[,j] > 0)) > 0) {
#       ICREFC_Pj[which(Mat_Sensib_Pj[,j] > 0 & Mat_Pj_z[,j] > 0),j] <- IC_Pj[which(Mat_Sensib_Pj[,j] > 0 & Mat_Pj_z[,j] > 0),j] * ICHP_Pj[which(Mat_Sensib_Pj[,j] > 0 & Mat_Pj_z[,j] > 0),j]
#     }
#     
#     # Idem si les deux sont nuls
#     if(length(which(Mat_Sensib_Pj[,j] == 0 & Mat_Pj_z[,j] == 0)) > 0) {
#       ICREFC_Pj[which(Mat_Sensib_Pj[,j] == 0 & Mat_Pj_z[,j] == 0),j] <- IC_Pj[which(Mat_Sensib_Pj[,j] == 0 & Mat_Pj_z[,j] == 0),j] * ICHP_Pj[which(Mat_Sensib_Pj[,j] == 0 & Mat_Pj_z[,j] == 0),j]
#     }
#     
#   }
#   
#   
#   
#   
#   #4eme etape: calcul de l'IC global
#   print("Calcul de l'indice de confiance global")
#   IC <- as.matrix(c(1:nz)) # On initialise le vecteur qui contiendra le resultat
#   for(i in 1:nz){ # Boucle sur toutes les mailles
#     if(length(which(!is.na(Mat_REF_PjE1_z[i, 2:(dim(Mat_REF_PjE1_z)[2]-3)]))) > 0){ # Si au moins l'un des REFC_Pj est connu (et donc le REFC final n'est pas NA) #ajout lodier 070324 : le -3 vient retirer les champs calculés
#       sub <- which(!is.na(Mat_REF_PjE1_z[i, 2:(dim(Mat_REF_PjE1_z)[2])])) + 1 # On extrait les numeros de colonnes associes aux REFC non-NA
#       if(sum(Mat_REF_PjE1_z[i, sub], na.rm=T) == 0) IC[i] <- mean(as.numeric(ICREFC_Pj[i, sub]), na.rm=T) # Si tous les REFC sont nuls, on fait une moyenne non-ponderee
#       else IC[i] <- sum(ICREFC_Pj[i, sub]*Mat_REF_PjE1_z[i, sub], na.rm=T)/sum(Mat_REF_PjE1_z[i, sub], na.rm=T) # Si l'un des REFC_Pj est non nul, on fait une moyenne des ICREFC_Pj ponderes par les REFC_Pj
#     } else {IC[i] <- NA}
#   }
#   IC <- cbind.data.frame(vec_zid, IC) # On integre le resultat dans un data frame
#   colnames(IC) <- c("mesh","IC")
#   
#   # Cartographie de l'indice de confiance
#   VARz <-  IC
#   nameVARz <- "IC"
#   title <- "Indice de confiance"
#   checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)
#   
#   # export dans la base de donnees
#   if(config$SauvRep$export_ic)  f_export_table(con, config$SauvRep$export_scheme, grid_z, IC, "confiance")
#   
# }
# 
# 
# 


# ============================================================================= # 
# ETAPE 11 - Export des resultats en PostgreSQL --------------------------------
# ============================================================================= # 
# Dans cette etape :
# Export des resultats dans la base de donnees, dans le schema vise pour export
#

if(config$SauvRep$db_choice=="local"){
  con <- dbConnect(drv=dbDriver(config$DBConnectlocal$DBConnect_drv),
                   host=config$DBConnectlocal$DBConnect_host,
                   user=config$DBConnectlocal$DBConnect_user,
                   password=config$DBConnectlocal$DBConnect_password,
                   dbname=config$DBConnectlocal$DBConnect_dbname,
                   port=config$DBConnectlocal$DBConnect_port)
} else {
  con <- dbConnect(drv=dbDriver(config$DBConnectdist1$DBConnect_drv),
                   host=config$DBConnectdist1$DBConnect_host,
                   user=config$DBConnectdist1$DBConnect_user,
                   password=config$DBConnectdist1$DBConnect_password,
                   dbname=config$DBConnectdist1$DBConnect_dbname,
                   port=config$DBConnectdist1$DBConnect_port)
}


##### !!!!! ##### !!!!! ##### !!!!! ##### !!!!! ##### !!!!! ##### !!!!! #####
## A REMPLACER DES QUE LA PARTIE DES INDICES DE CONFIANCE EST RESTAUREE !!!!
data_index <- grid_z %>% 
  st_drop_geometry() %>% 
  dplyr::select(idmesh) %>% 
  left_join(REFC_E1, by = c('idmesh'='mesh')) %>% 
  left_join(IMA, by = "idmesh") %>% 
  left_join(IPC_z, by = c('idmesh'='mesh')) %>% 
  mutate(IC = NA)
##### !!!!! ##### !!!!! ##### !!!!! ##### !!!!! ##### !!!!! ##### !!!!! #####

# Ajustement des noms des champs
Mat_Pj_norm_z <- Mat_Pj_norm_z %>% rename(idmesh = mesh)
Mat_Sensib_Pj_ungeom	<- Mat_Sensib_Pj %>% rename(idmesh = E_idmesh) %>% st_drop_geometry()
Mat_REF_PjE1_z <- Mat_REF_PjE1_z %>% rename(idmesh = mesh)

if(config$SauvRep$export_db_tables|config$SauvRep$export_index_sql|config$SauvRep$export_ic){
  #Export de la table contenant les principaux indices
  if(config$SauvRep$export_index_sql){
    data_index2 <- data_index
    # Conversion en donnees numeriques de plusieurs colonnes qui etaient utilisees comme facteurs pour les graphiques
    data_index2[,3] <- as.numeric(as.character(data_index[,3]))
    data_index2[,4] <- as.numeric(as.character(data_index[,4]))
    data_index2[,5] <- as.numeric(as.character(data_index[,5]))
    f_export_table(con, config$SauvRep$export_scheme, grid_z, data_index2, "index")
  }
  #Export de la table contenant les pressions normalisees
  if(config$SauvRep$export_db_tables){
    f_export_table(con, config$SauvRep$export_scheme, grid_z, A_zi, "activites")
    f_export_table(con, config$SauvRep$export_scheme, grid_z, Mat_Pj_norm_z, "pressions")
    f_export_table(con, config$SauvRep$export_scheme, grid_z, Mat_Sensib_Pj_ungeom, "sensibilite")
    f_export_table(con, config$SauvRep$export_scheme, grid_z, Mat_REF_PjE1_z, "effet")
  }
}


## Fonction qui permettra d'afficher le temps de calcul
# printime <- function(time){
#   if(time<60) print(paste("Temps de calcul : ", floor(time), "s", sep=""))
#   if(time > 60 && time <3600){
#     min <- floor(time/60)
#     sec <- time - min*60
#     print(paste("Temps de calcul : ", min, "min ", floor(sec), "s", sep=""))
#   }
#   if(time>3600){
#     hrs <- floor(time/3600)
#     min <- floor((time-3600*hrs)/60)
#     sec <- time - 3600*hrs - 60*min
#     print(paste("Temps de calcul : ", hrs, "h ", min, "min ", floor(sec), "s", sep=""))
#   }
# }
# 
# printime(as.numeric((proc.time()-ptm)[3])) #Affichage du temps de calcul

stop("-------- That's all Folks ---------")



