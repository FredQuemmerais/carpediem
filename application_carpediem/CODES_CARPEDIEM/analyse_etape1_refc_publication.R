# ANALYSE DU RISQUE D'EFFETS CUMULES DES ACTIVITES ANTHROPIQUES SUR LES HABITATS BENTHIQUES
# ETAPE 1, ANALYSE PAR DEFAUT
                                                      
# Projet        : code développé dans le cadre du projet CARPEDIEM (2016-2018)
# Date          : 2022/10/06
# Version code  : 1.0
# Version R     : R-3.5.1
# Auteurs       : Alice Vanhoutte-Brunier, Julien Barrere
# Contributeur  : Frederic Quemmerais-Amice, Guilhem Autret (OFB)
# Contact       : frederic.quemmerais-amice@ofb.gouv.fr

# Office Français de la Biodiversite (https://www.ofb.gouv.fr/)

# Documentation : Guide d'utilisation des outils d'analyses de donnees du projet Carpediem:
#                 cartographie du risque d'effets cumules sur les habitats benthiques
#                 version 7.1 publique (octobre 2022), OFB, 149 pages.
#                 Julien Barrere, Frederic Quemmerais-Amice

# Citation      : Quemmerais-Amice Frédéric, Barrere Julien, La Rivière Marie, Contin Gabriel, Bailly Denis, 2020.
#                 A Methodology and Tool for Mapping the Risk of Cumulative Effects on Benthic Habitats.
#                 Frontiers in Marine Science. 7:569205. https://doi.org/10.3389/fmars.2020.569205

# Licence       : Ce programme est un logiciel libre diffusé sous les termes de la
#                 licence publique générale GNU (GNU General Public License, GNU GPL) version 3 ou toute version ultérieure.
#                 Vous pouvez consulter le guide rapide de la GNU GPL v3 sur https://www.gnu.org/licenses/quick-guide-gplv3.fr.html
#                 Vous pouvez redistribuer et/ou modifier le contenu de ce programme
#                 suivant les termes de la GNU GPL version 3 ou ultérieure telle que publiee par la
#                 Free Software Foundation.
#                 Consultez la GNU General Public License pour plus de details.

# Responsabilité : Ce programme est diffusé dans l'espoir qu'il sera utile.
#                 Les auteurs, les contributeurs et l'OFB n'offrent aucune garantie de fonctionnement et de résultat liée à l'utilisation de ce programme.
#                 Ils ne peuvent en cas être tenu responsables des interprétations et des conclusions qui pourraient être faites suite à l'utilisation de ce programme.
#

# Deroulement general de l'analyse (voir le guide d'utilisation) :                                                      
#     - Les parametrages de l'analyse sont renseignés dans un fichier Excel
#       contenu dans le dossier "PARAM" du dossier "CODES_CARPEDIEM".
#     - Dans le code, seuls les parametres "fileparam" et "userdir" correspondant respectivement
#       au nom du fichier .xlsx de parametrage et au chemin d'acces du dossier "CODES_CARPEDIEM" sont
#       a renseigner aux lignes 66 et 69.
#     - Lecture et import des tables de donnees sources contenues dans une base de donnees PostgreSQL (locale et/ou distante).
#     - Export des resultats dans la base de donnees, dans de nouvelles tables, dans un schema a specifier.

# ----------------------------------------------------------------------------------------------------
#              ETAPE 01 - LECTURE DE FONCTIONS DECRITES DANS DU CODE SOURCE
#              ETAPE 02 - LECTURE DES PARAMETRES DU FICHIER EXCEL ET ECRITURE DANS LA LISTE config
#              ETAPE 03 - IMPORT DES TABLES DE LA BASE DE DONNEES
#              ETAPE 04 - GESTION DES INDICES ET DIMENSIONS, stockage dans des listes     
#              ETAPE 05 - DONNEES HABITATS ET SENSIBILITE   
#              ETAPE 06 - CARTOGRAPHIE DES ACTIVITES
#              ETAPE 07 - CARTOGRAPHIE DES PRESSIONS
#              ETAPE 08 - CARTOGRAPHIE DU RISQUE D'EXPOSITION PRESSION / MULTI-PRESSIONS                                                           )
#              ETAPE 09 - CALCUL DU RISQUE D'EFFETS // RISQUE D'EFFETS CONCONMITANTS  
#              ETAPE 10 - REALISATION ET SAUVEGARDE DE GRAPHIQUES REPRESENTATIFS DES RESULTATS
#              ETAPE 11 - EXPORT DES RESULTATS SOUS FORME DE TABLES DANS LA BASE DE DONNEES 
#              ETAPE 12 - CALCUL D'UN INDICE DE CONFIANCE POUR LES RELATIONS THEORIQUES
# ----------------------------------------------------------------------------------------------------

rm(list=ls())
ptm <- proc.time()

# PARAMETRES DE CONFIGURATION A RENSEIGNER ------
# reprises et sauvegardes
# parametrage
fileparam         <- "fichier_parametrage_refc_2022_source_fonctionnel.xlsx"    # fichier de parametrage Excel
# utilisateur
# fichier utilise pour analyse publiee dans Frontiers in marine science = "fichier_parametrage_refc_plateau_20190409_v2.xlsx"
userdir           <- "D:/02_SIG/00_CODE_CARPEDIEM_BENTHOS_OPERATIONNEL_2022/"
# ------------------------------------------------------------------------------

print("# ------------------------------------------------------------------------------------------")
print("# EVALUATION DU RISQUE D'EFFETS CONCOMITANTS                                                ")
print("# DES ACTIVITES ANTHROPIQUES SUR LES COMPOSANTES ECOLOGIQUES MARINES                        ")
print("# ------------------------------------------------------------------------------------------")
print(" ")
print("# ------------------------------------------------------------------------------------------")
print("# Parametrage :                                                                             ")
print(paste("# Fichier de parametrage :   ",fileparam,sep=""))
print(paste("# Repertoire utilisateur :   ",userdir,sep=""))
print("# ------------------------------------------------------------------------------------------")
print(" ")


# Gestion de l'arborescence - ne fonctionne que si l'utilisateur a cree la meme arborescence dans le userdir
moddir     <- paste(userdir,"CODES_CARPEDIEM",sep="")     
indir      <- paste(moddir,"/INPUTS",sep="") 
outdir     <- paste(moddir,"/OUTPUTS",sep="") 
paramdir   <- paste(moddir,"/PARAM",sep="")
sauvrepdir <- paste(moddir,"/SAUVREP",sep="")
sigdir     <- paste(indir,"/SIG",sep="")

setwd(moddir)

# Chargement des librairies
#-- packages reinstalles sur R 3.4.1
library(rpostgis)
library(sp)
library(RPostgreSQL)  # pour lire DB et transfereer data.frames vers DB PostgreSQL
library(dplyr)
library(postGIStools) # pour charger des tables de la DB PostgreSQL 
library(rgeos)        
library(maptools) 
library(rgdal)
library(raster)
library(shapefiles)
library(PBSmapping)
library(lattice)
library(rJava)        # pour utiliser xlsx
library(xlsxjars)     # pour utiliser xlsx
library(xlsx)
library(ggplot2)      # pas utilisee pour l'instant
library(ggmap)        # fonds de cartes, en association avec gglopt2
library(ncdf4)
library(psy)          # statistiques ACP
require(RPostgreSQL)


#-- pas encore reinstalles sous R.3.4.1
library(grDevices)



print("# ")
print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 1 - LECTURE DE FONCTIONS DECRITES DANS DU CODE SOURCE                               ")
print("# ------------------------------------------------------------------------------------------")
source('f_import.R')                # code source pour importer les parametres / tables de DB
source('f_lec_Postgres_DB.R')       # code source de fonctions de lecture de BD Postgre  
source('./Rpostgis/Rpostgis.R')     # code source Nicolas Lambert - modifie par AVB
source('f_map_plot.R')              # code source fonctions de plot des cartes
source('f_spatial_data_analysis.R') # Fonctions ouverture couches SIG, ...

print("# ")
print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 2 - LECTURE DES PARAMETRES DU FICHIER EXCEL ET ECRITURE DANS LA LISTE config        ")
print("# ------------------------------------------------------------------------------------------")
file_path <- paste(paramdir,"/",fileparam,sep="")         # path et nom du fichier Excel qui contient les parametres
config    <- f_importConfig(file_path)                    # lecture du fichier Excel de parametrage



# Unites et conversions 
# - surface exprimees en km²
conv_m2tokm2 <- 1e-06

# Preparation sauvegarde des resultats
# - Creation d'une fonction pour convertir le binaire entre dans le fichier de parametrage en terme de SRM etudiees
read_srm <- function(x){
  out <- c()
  if(as.numeric(substr(x, 1, 1))==1) out <- c(out, "MED")
  if(as.numeric(substr(x, 2, 2))==1) out <- c(out, "GDG")
  if(as.numeric(substr(x, 3, 3))==1) out <- c(out, "CEL")
  if(as.numeric(substr(x, 4, 4))==1) out <- c(out, "MMN")
  return(out)
}
# - suffixe domaine evalue
lcond_SRM <- read_srm(config$TimeSpace$config_SRM)

# - suffixe domaine evalue
nchainchar <- length(lcond_SRM)
for(i in 1:nchainchar){
  if(i==1) {chainchar <- lcond_SRM[i]} 
  else {chainchar <- paste(chainchar,lcond_SRM[i], sep="_")}}

print("# ")
print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 3 - IMPORT DES TABLES DE LA BASE DE DONNEES                                         ")
print("# ------------------------------------------------------------------------------------------")
# Gestion des sauvegardes / reprises
datesauv <- paste(substr(date(),21,24),substr(date(),5,7),substr(date(),9,10),sep="") #suffixe du fichier de sauvegarde
lrepr_DBtables <- FALSE # On initialise la reprise des tables a FAUX
if(config$SauvRep$lrepr_DBtables==1)lrepr_DBtables <- TRUE # Si on a configure l'option, on passe a VRAI
lsauv_DBtables <- FALSE # On intialise la sauvegarde des tables a FAUX
if(config$SauvRep$lsauv_DBtables==1)lsauv_DBtables <- TRUE # Si on a parametre l'option, on passe a VRAI
daterepr <- config$SauvRep$daterepr

# Lecture ou reprise des tables de la DB
if(!lrepr_DBtables) {   # Import des tables de la DB si elles n'ont pas ete sauvegardees dans un fichier d'initialisation
  # -- les tables importees sont sauvegardees dans la liste list_sauv, creation de cette liste
  list_sauv <- list()
  print("IMPORT DES TABLES DE LA DB :")
  
  # nombre de jeux de donnees à importer : ntables
  # - calcul du nombre de tables à important en faisant une recherche du nombre de valeurs uniques dans le champ "DB_Table"
  # -- identification du numero de la colonne contenant les noms des tables
  icolDB_table <- which(colnames(config$DBData$Data)=="DB_table")
  # -- liste des noms des tables
  list_sauv$vtab_names <- c()
  # -- liste des tables a importer
  vtables <- unique(config$DBData$Data[,icolDB_table])
  # -- nombre de tables a importer
  ntables <- length(vtables)
  
  # import de toutes les tables decrites dans le feuillet Excel 'Data'
  for(i in 1:ntables){
    # reperage de la premiere ligne correspondant a la table i
    irow <- which(config$DBData$Data[,icolDB_table]==vtables[i])[1]
    # recuperation du nom de code de la table, lu dans colonne "Data_code"
    icolTable_code <- which(colnames(config$DBData$Data)=="Table_code")
    iTable_code <- config$DBData$Data[irow,icolTable_code]
    print(iTable_code)
    # -- nom de la variable recevant toutes les donnees de la table
    icolData_type <- which(colnames(config$DBData$Data)=="Data_type")
    tab_name <- paste(config$DBData$Data[irow,icolData_type],"_",config$DBData$Data[irow,icolTable_code],sep="")
    list_sauv$vtab_names <- c(list_sauv$vtab_names,tab_name)
    
    # - l'import est fonction du type de fichier : Table d'une BD spatiale ou fichier SIG
    if(config$DBData$Data[irow,which(colnames(config$DBData$Data)=="Source")]=="PgAdmin"){
      
      # -- recuperation des arguments de lecture de la table par la fonction f_argDB cree dans lec_postgresDB.R
      args <- f_argDB(iTable_code,config$DBData$Data,config)
      # import de la table par la fonction f_importTableFromDBPostgresSQL cree dans import.R
      temp <- f_importTableFromDBPostgresSQL(args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8])
      
    } else {  # fichier SIG
      filename <- paste(sigdir,"/ACTIVITES/",config$DBData$Data[irow,which(colnames(config$DBData$Data)=="DB_table")],sep="")
      temp <- readShapePoly(filename,proj4string = CRS("+proj=longlat +datum=WGS84"))
    }
    
    # -- ecriture de la table/couche SIG dans une variable ayant le nom renseigne dans tab_name
    eval(parse(text=paste(tab_name," <- temp",sep="",collapse=" ; ")))
    print(paste('... lecture de la variable ',tab_name, '   ... OK'))
    head(temp@data)
    eval(parse(text=paste("list_sauv$",tab_name,"<- temp",sep="",collapse=" ; ")))
  }
  
  # sauvegarde dans un fichier Rdata 
  if(lsauv_DBtables) {
    namedata <- "tables_"
    filesauv <- paste(sauvrepdir,"/init_",namedata,datesauv,".Rdata",sep="")
    save(list_sauv,file=filesauv)
    print(paste("Sauvegarde realisee dans le fichier : ",filesauv))
  }
  
}else{                   # reprise des tables sauvegardees
  namedata <- "tables_"
  filerepr <- paste(sauvrepdir,"/init_",namedata,daterepr,".Rdata",sep="")
  if(!file.exists(filerepr)) stop(paste("le fichier .Rdata suivant n existe pas : ",filerepr,sep=""))
  load(file=filerepr)
  # affectation des variables lues
  ndata <- length(list_sauv$vtab_names)
  for(idata in 1:ndata){
    iTable_code <- list_sauv$vtab_names[idata]
    paste("-- reprise de la table : ",iTable_code)
    temp <- eval(parse(text=paste(iTable_code," <- ","list_sauv$",iTable_code,sep="",collapse=" ; ")))
    print(paste('... recuperation de la table ',iTable_code, '   ... OK'))
    head(temp@data)
  }
  print(paste("Reprise de toutes les tables contenues dans le fichier ",filerepr," ... ok",sep=""))
}

print("# ")
print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 4 - GESTION DES INDICES ET DIMENSIONS, stockage dans des listes                     ")
print("# ------------------------------------------------------------------------------------------")
# dimensions spatiales, grille
# - lecture et indicage des mailles de la zone evaluee
grid_z  <- list()
nztot   <- dim(coordinates(G_grid))[1]
nz      <- 0 ; iz <- 0;

# - identification du champ de la table qui indique si la maille est dans la zone evaluee
#   appel a la fonction f_idFieldDBTable_from_nameField (lec_Postgres_DB.R) 
#   qui retourne l'indice de la colonne de la table correspondant au Var_code 
#   du feuillet 'Data' du fichier Excel de parametrage 
i_SRM    <- f_idFieldDBTable_from_nameField("srm",config$DBData$Data,G_grid@data,"grid") # colonne de la grille contenant la SRM de chaque maille
i_surfmer<- f_idFieldDBTable_from_nameField("surfmer",config$DBData$Data,G_grid@data,"grid") # Colonne de la grille contenant la surface maritime
i_idz    <- f_idFieldDBTable_from_nameField("idmesh",config$DBData$Data,G_grid@data,"grid") # Colonne de la grille contenant l'identifiant de maille

# Nom du fichier R de sauvegarde de la grille
filesauv_grid_z <- paste(sauvrepdir,"/init_grid_z_",chainchar,"_",datesauv,".Rdata",sep="")
config$filesauv_grid_z <- filesauv_grid_z

if(config$SauvRep$lrepr_grid_z==0){
  # lecture de la grille"
  print("# le fichier de configuration de la grille n existe pas, lancement de l analyse")
  varsave <- list()
  
  varsave$long_z <- array(NA,dim=(nz)) ;    varsave$lat_z <- array(NA,dim=(nz))
  varsave$longmax_z <- array(NA,dim=(nz)) ; varsave$latmax_z <- array(NA,dim=(nz))
  varsave$longmin_z <- array(NA,dim=(nz)) ; varsave$latmin_z <- array(NA,dim=(nz))
  
  # - selection des mailles qui correspondent a zone choisie et mailles en mer
  grid_z    <- subset(G_grid,G_grid@data@.Data[[i_SRM]] %in% lcond_SRM)
  grid_z    <- subset(grid_z,grid_z@data@.Data[[i_surfmer]] > 0.) # surface de mer superieure a 1
  
  nz        <- dim(grid_z)[1]
  
  # enregistrement des coordonnees de chaque maille de la grille
  for(z in 1:nz){
    print(paste("  - progression analyse de la grille : ",z,"/",nz))
    varsave$long_z[z]    <-  coordinates(grid_z)[z,1]  
    varsave$lat_z[z]     <-  coordinates(grid_z)[z,2]  
    varsave$longmax_z[z] <-  max(grid_z@polygons[[z]]@Polygons[[1]]@coords[,1])  
    varsave$longmin_z[z] <-  min(grid_z@polygons[[z]]@Polygons[[1]]@coords[,1])
    varsave$latmax_z[z]  <-  max(grid_z@polygons[[z]]@Polygons[[1]]@coords[,2])  
    varsave$latmin_z[z]  <-  min(grid_z@polygons[[z]]@Polygons[[1]]@coords[,2])  
  }
  
  print(paste("# >> sauvegarde de la grille realisee dans le fichier : ",filesauv_grid_z))
  if(config$SauvRep$lsauv_grid_z==1){save(nz,varsave,grid_z,file=filesauv_grid_z)}
  
}else{
  filerepr_grid_z <- paste(sauvrepdir,"/init_grid_z_",chainchar,"_",config$SauvRep$daterepr_grid_z,".Rdata",sep="")
  if(!file.exists(filerepr_grid_z)) stop(paste("le fichier .Rdata suivant n existe pas : ",filerepr_grid_z,sep=""))
  print(paste("# << reprise de la grille sauvegardee dans le fichier : ",filerepr_grid_z,sep=""))
  load(filerepr_grid_z)
}
nz        <- dim(grid_z)[1]


# - latitudes et longitures min et max

config$nz <- nz
config$long_z <- array(NA,dim=(nz)) ; config$lat_z <- array(NA,dim=(nz))
config$longmax_z <- array(NA,dim=(nz)) ; config$latmax_z <- array(NA,dim=(nz))
config$longmin_z <- array(NA,dim=(nz)) ; config$latmin_z <- array(NA,dim=(nz))

print(paste("# l'evaluation est realisee sur : ",nz," mailles",sep=""))

config$longmin <- extent(grid_z)[1];  config$longmax <- extent(grid_z)[2]; 
config$latmin <-  extent(grid_z)[3] ; config$latmax <-  extent(grid_z)[4]
print(paste("# les limites du domaine evalue sont : S:",config$latmin," N:",config$latmax," W:",config$longmin," E:",config$longmax,sep=""))

config$lat_z  <- varsave$lat_z ; config$latmin_z  <- varsave$latmin_z ;  config$latmax_z  <- varsave$latmax_z 
config$long_z <- varsave$long_z ;config$longmin_z <- varsave$longmin_z ; config$longmax_z <- varsave$longmax_z

col_idz      <- eval(parse(text=paste("grid_z$",colnames(head(grid_z))[i_idz],sep="",collapse=" ; ")))
col_surfmer  <- eval(parse(text=paste("grid_z$","surfmer",sep="",collapse=" ; ")))

rm(varsave)


print("# ")
print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 5 - DONNEES HABITATS ET SENSIBILITE                                                 ")
print("# ------------------------------------------------------------------------------------------")

# Habitats benthiques et sensibilite aux pressions : reduction de la table aux pressions et zone etudiees
# -------------------------------------------------------------------------------------------------
# - boucle sur les pressions etudiees : correspondance pression et choix dans feuillet de parametrage
#   Typo_pressions, colonne "demonstrateur" renseignee avec valeur 1
#   - identification colonne "demonstrateur" dans fichier Excel
nj <- config$nj


# reduction du nombre de pressions si agregation de plusieurs pressions
# - import de toutes les pressions et caracteristiques (presentes dans mat A/P)
P_code <- config$P_code # Code des pressions
P_name <- config$P_name # Nom des pressions
P_modcalc <- config$P_modcalc # Mode de calcul des pressions
P_evalDEM <- config$P_evalDEM # Prises en compte des pressions dans l'evaluation

# - pressions agregees
vec_P_ag <- unique(config$P_code_ag[1:nj],rm.na=TRUE)
vec_P_ag <- vec_P_ag[!is.na(vec_P_ag)]
#   - identification des pressions correspondant aux pressions agregees, changement des noms
for(ivec in 1:length(vec_P_ag)){
  vec_ip <- which(config$P_code_ag[1:nj]==vec_P_ag[ivec])
  P_code[vec_ip] <- config$P_code_ag[vec_ip]
  P_name[vec_ip] <- config$P_name_ag[vec_ip]
}
#   - cbind des caracteriqtiques des pressions puis suppression des lignes redondantes
mat <- cbind(P_code,P_name,P_modcalc,P_evalDEM)
mat <- mat[! duplicated(mat[,1]), ]
P_code <- mat[,1]; P_name <- mat[,2] ; P_modcalc <- mat[,3] ; P_evalDEM <- mat[,4]
nj <- length(P_code)

# Surface des habitats benthiques par maille : somme des surfaces de chaque polygone habitat present dans la maille
# -------------------------------------------------------------------------------------------------
# Identification de numeros de colonne cles dans la table des habitats benthiques
i_surfhab  <- f_idFieldDBTable_from_nameField("surfhab",config$DBData$Data,E_habbenth_sensib@data,"habbenth_sensib") # Colonne contenant la surface des habitats
i_idzhab   <- f_idFieldDBTable_from_nameField("idmesh",config$DBData$Data,E_habbenth_sensib@data,"habbenth_sensib") # Colonne contenant l'identifiant de maille
i_surfmesh <- f_idFieldDBTable_from_nameField("surfmesh",config$DBData$Data,E_habbenth_sensib@data,"habbenth_sensib") # Colonne contenant la surface maritime
i_surfmer <- which(colnames(E_habbenth_sensib@data)=="surfmer")
# - restriction de la table habitats/sensibilite aux seules mailles de la zone etudiee
HabBenth_z <- subset(E_habbenth_sensib, E_habbenth_sensib@data@.Data[[i_idzhab]]%in%grid_z@data@.Data[[i_idz]])

# - conversion des surfaces d'habitats m2 -> km2
HabBenth_z@data@.Data[[i_surfhab]] <- HabBenth_z@data@.Data[[i_surfhab]] * conv_m2tokm2

nb_poly    <- dim(HabBenth_z)[1]

# - creation d'une liste qui comporte des infos relatives a la table HabBenth_z
lHabBenth_z <- list()

# - sauvegarde/reprise de la table habitat/sensibilite 
filesauv_surfhab <- paste(sauvrepdir,"/init_lHabBenth_z_",chainchar,"_",datesauv,".Rdata",sep="")

if(config$SauvRep$lrepr_hab_sensib==0){
  # - cumul des surfaces d'habitats par maille
  print("#   identification de la ligne de la grid_z correspondant a la maille dans laquelle est situe le polygone")
  lHabBenth_z$surfhab_z  <- array(0.,dim=c(nz,1),dimnames=list(paste("z",1:nz,sep=""),paste("surfhab")))
  lHabBenth_z$surfmesh_z <- array(NA,dim=c(nz,1),dimnames=list(paste("z",1:nz,sep=""),paste("surfmesh")))
  lHabBenth_z$idmesh_z   <- as.matrix(col_idz) ; dimnames(lHabBenth_z$idmesh_z) <- list(paste("z",1:nz,sep=""),paste("mesh"))
  
  for(ipoly in 1:nb_poly){
    # - numero de la ligne correspondant a cet identifiant de maille
    id <- which(lHabBenth_z$idmesh_z==HabBenth_z@data@.Data[[i_idzhab]][[ipoly]])
    # - cumul de surface d'habitat
    lHabBenth_z$surfhab_z[id] <- lHabBenth_z$surfhab_z[id] + HabBenth_z@data@.Data[[i_surfhab]][[ipoly]]
    #surface de la maille (en km2)
    lHabBenth_z$surfmesh_z[id] <- HabBenth_z@data@.Data[[i_surfmesh]][[ipoly]]
    #surface maritime (en km2)
    lHabBenth_z$surfmer_z[id] <- HabBenth_z@data@.Data[[i_surfmer]][[ipoly]]
  }
  
  # - nom des colonnes de la table HabBenth_z
  lHabBenth_z$P_codes_HabBenth <- colnames(as.matrix(HabBenth_z@data))
  if(config$SauvRep$lsauv_hab_sensib==1){save(lHabBenth_z,file=filesauv_surfhab)}
} else {
  filerepr_surfhab <- paste(sauvrepdir,"/init_lHabBenth_z_",chainchar,"_",config$SauvRep$daterepr_hab_sensib,".Rdata",sep="")
  if(!file.exists(filerepr_surfhab)) stop(paste("le fichier .Rdata suivant n existe pas : ",filerepr_surfhab,sep=""))
  load(filerepr_surfhab)
}


# - verification que la surface des habitats dans la maille n'excede pas la surface de la maille
config$surfmer_z <- lHabBenth_z$surfmer_z
nbdec <- 6 # nombre de decimales (pour surfaces exprimees en km2)
depa        <- which(apply(as.matrix(c(1:nz)), c(1,2), function(x) (round(lHabBenth_z$surfhab_z[x],nbdec) > round(config$surfmer_z[x],nbdec))==TRUE))
if(length(depa)>0){
  print(paste("ATTENTION : ",length(depa)," mailles sur ",nz," ont surfhab > surfmer"," AVEC MARGE ",nbdec," chiffres apres la virgule"))
  for(idepa in 1:length(depa)){
    # - indice correspondant grid_z
    iz <- which(col_idz==lHabBenth_z$idmesh_z[depa[idepa]])
    if(config$lat_z[iz] < config$latmax && config$lat_z[iz] > config$latmin 
       && config$long_z[iz] < config$longmax && config$long_z[iz] > config$longmin )
    {
      print(paste(" SURFACE HAB > SURF MER DANS le perimetre  maille: ",iz,lHabBenth_z$idmesh_z[depa[idepa]],
                  round(lHabBenth_z$surfhab_z[depa[idepa]],nbdec),round(config$surfmer_z[depa[idepa]],nbdec),
                  "lat:",round(config$lat_z[iz],3),"long:",round(config$long_z[iz],3)))
    }
  }
}
# - reajustement des surfaces d'habitats par maille pour mailles surfhab > surfmesh en attente couche habitats "clean"
print("Reajustement des surfaces d'habitats pour mailles surfhab > surfmesh")
if(config$Calc$l_corr_surfhab){
  lHabBenth_z$surfhab_z_NEW <- lHabBenth_z$surfhab_z
  lHabBenth_z$surfhab_z_NEW[lHabBenth_z$surfhab_z_NEW > config$surfmer_z] <- 0
  
  for(ipoly in 1:nb_poly){
    id <- which(lHabBenth_z$idmesh_z==HabBenth_z@data@.Data[[i_idzhab]][[ipoly]])
    if(lHabBenth_z$surfhab_z[id] > config$surfmer_z[id]){
      print(paste("# :: surface habitat du polygone corrigee OLD : ",ipoly,HabBenth_z@data@.Data[[i_surfhab]][[ipoly]]))
      HabBenth_z@data@.Data[[i_surfhab]][[ipoly]] <- HabBenth_z@data@.Data[[i_surfhab]][[ipoly]] * config$surfmer_z[id] / lHabBenth_z$surfhab_z[id] 
      print(paste("#                                         NEW : ",ipoly,HabBenth_z@data@.Data[[i_surfhab]][[ipoly]]))
      lHabBenth_z$surfhab_z_NEW[id] <- lHabBenth_z$surfhab_z_NEW[id]+ HabBenth_z@data@.Data[[i_surfhab]][[ipoly]]
    }
  }
  lHabBenth_z$surfhab_z <- lHabBenth_z$surfhab_z_NEW
}




# - score de sensibilite cumule par pression et par maille  = surface de l'habitat sensible * score de sensibilite
# -------------------------------------------------------------------------------------------------
#
#   - creation de la dataframe spatiale : sensibilite cumulee a la pression j
# SensibC_j  <- grid_z 

# - Config TestSensib_1 : approche pecaution/mediane du seuil de sensibilite : 
# le suffixe du nom des pressions depend de l'option choisie pour le test de sensibilite 1
if(config$Calc$agreg_sensib==1){suf_sensib <- "_pre"; print("Mode d'agregation des scores de sensibilite = precaution")}
if(config$Calc$agreg_sensib==2){suf_sensib <- "_med"; print("Mode d'agregation des scores de sensibilite = median")}

# stockage des resultats de sensibilite cumulee
Mat_Sensib_Pj <- lHabBenth_z$idmesh_z

#  - Boucle sur les pressions : calcul du score de sensibilite cumulee
for(j in 1:nj){
  if(P_evalDEM[j]==1){  #reduction du nombre de pressions pour le demonstrateur
    
    i_idPcode <- c()
    # code de la pression
    P_code_HabBenth <- paste(tolower(P_code[j]),suf_sensib,sep="")
    # identification de la colonne correspondante a cette pression
    i_idPcode   <- which(lHabBenth_z$P_codes_HabBenth==P_code_HabBenth)
    
    if(length(i_idPcode)==0){
      print(paste("#  STOP : le code pression ",P_code_HabBenth," n a pas d equivalent dans la table habbenth_sensib : ",sep=""))
      stop(paste(lHabBenth_z$P_codes_HabBenth," / ",sep=""))
    }
    
    #creation des vecteurs qui contiennent les resultats
    # - score de sensibilite cumule
    varName <- paste("SensibC_",P_code[j],sep="")
    res   <- array(0.,dim=c(nz,1),dimnames=list(paste("z",1:nz,sep=""),varName))
    # - ratio de la surface d'habitats dans la maille sans valeur de sensibilite pour la pression j
    surf_nodata  <- array(0.,dim=c(nz,1),dimnames=list(paste("z",1:nz,sep=""),paste(varName,"_surfnodata",sep="")))
    ratio_nodata <- array(0.,dim=c(nz,1),dimnames=list(paste("z",1:nz,sep=""),paste(varName,"_ratinodata",sep="")))
    
    for(ipoly in 1:nb_poly){
      iz <- which(lHabBenth_z$idmesh_z==HabBenth_z@data@.Data[[i_idzhab]][[ipoly]])
      #identification de la maille dans laquelle est le polygone
      # - cas ou la valeur de sensibilite de l'habitat n'est pas connue : cumul de surface
      if(HabBenth_z@data@.Data[[i_idPcode]][[ipoly]]==99){
        #print(paste(" :: pas de valeur de sensibilite pour P: ",P_name[j]," poly: ",ipoly," = ",HabBenth_z@data@.Data[[i_idPcode]][[ipoly]],sep=""))
        surf_nodata[iz] <- surf_nodata[iz] + (HabBenth_z@data@.Data[[i_surfhab]][[ipoly]])
      }else{
        # - sinon on integre la valeur de sensibilite dans le calcul du cumul
        res[iz] <- res[iz] + (HabBenth_z@data@.Data[[i_surfhab]][[ipoly]]/HabBenth_z@data@.Data[[i_surfmer]][[ipoly]]) * HabBenth_z@data@.Data[[i_idPcode]][[ipoly]]  #surface * Sensib_j
      }
    }
    # pour les mailles ou la surface de nodata(sensibilite) >th_sensibi * surface d'habitats -> res = NA
    if(config$Calc$l_th_sensib){
      print(paste("# Prise en compte d'un seuil pour visualisation des resultats de sensibilite, seuil = ",config$Calc$th_sensib," %")) 
      vecid <- which(surf_nodata > 0.)
      ratio_nodata[vecid] <- (surf_nodata[vecid]/lHabBenth_z$surfhab_z[vecid]) * 100.
      if(config$Calc$l_th_sensib){
        res[which(ratio_nodata > config$Calc$th_sensib)] <- NA
    }
    }  
    Mat_Sensib_Pj <- cbind.data.frame(Mat_Sensib_Pj,res)
    colnames(Mat_Sensib_Pj)[dim(Mat_Sensib_Pj)[2]] <- varName
    
    # creation des figures   
    if(config$Outputs$lplot_sensibC_P){
      # - plot de la sensibilite cumulee
      nameVARz <- paste(varName,sep="")
      VARz <- cbind.data.frame(lHabBenth_z$idmesh_z,res)
      colnames(VARz) <- c("mesh",nameVARz)
      checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,
                                    paste("Sensibilite cumulee ? ",P_name[j],sep="")) 
    }
  }
}
## Correction de la matrice sensibilite: si la maille n'est pas dans la table gr_eco_hab, on lui attribue un NA pour la sensibilite (au lieu de zero)
for (i in 1:nz){
  if(!Mat_Sensib_Pj[i,1] %in% HabBenth_z@data@.Data[[i_idzhab]]){
    Mat_Sensib_Pj[i,2:(dim(Mat_Sensib_Pj)[2])] <- NA
  }
}

print("# ")
print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 6 - CARTOGRAPHIE DES ACTIVITES"                                                      )
print("# ------------------------------------------------------------------------------------------")

# nombre d activites dans la matrice
ni <- config$ni

# correspondance activites / donnees descriptives des activites / tables chargees

# - champs de la table contenue dans le feuillet 'Data'
i_Source      <- which(colnames(config$DBData$Data)=="Source")
i_Data_code   <- which(colnames(config$DBData$Data)=="Data_code")
i_Table_code  <- which(colnames(config$DBData$Data)=="Table_code")
i_Var_code	  <- which(colnames(config$DBData$Data)=="Var_code")
i_Data_type	  <- which(colnames(config$DBData$Data)=="Data_type")
i_DB_choice	  <- which(colnames(config$DBData$Data)=="DB_scheme")
i_DB_scheme	  <- which(colnames(config$DBData$Data)=="DB_scheme")
i_DB_table	  <- which(colnames(config$DBData$Data)=="DB_table")
i_Field       <- which(colnames(config$DBData$Data)=="Field")
i_Data_name   <- which(colnames(config$DBData$Data)=="Data_name")
i_Unit        <- which(colnames(config$DBData$Data)=="Unit")

nbcol <- length(grep("Code",colnames(config$DBData$Data)))  # nombre de colonnes de codes a parcourir

# creation du vecteur qui accueille le numero de la ligne du tableau 'Data' qui decrit l'intensite de l'activite
config$A_ilindata <- array(NA,dim=c(ni,1),dimnames=list(paste("i",1:ni,sep=""),c("ilindata")))

# creation du vecteur qui accueille le code activite agrege quand un jeu de donnees decrit plusieurs activites
config$Aiag <- array(NA,dim=c(ni,1),dimnames=list(paste("i",1:ni,sep=""),c("Aiag")))

# - verification si jeux de donnees existants pour les activites dans feuillet 'Data'
for(i in 1:ni){ # boucle sur les activites decrites dans le feuillet 'Typo_activites'
  print(paste("-----------------ACTIVITE ",config$A_code[i], "   //   ",config$A_name[i],"----------------------",sep=""))
  print(paste("# "))
  
  # - recherche du jeu de donnees correspondant sur l'intensite dans les colonnes Code# de la table config$DBData$Data
  for(icol in 1:nbcol){  # on parcourt les colonnes Code# pour trouver les codes activites
    
    vec_codesA <- config$DBData$Data[,which(colnames(config$DBData$Data)==paste("Code",icol,sep=""))]
    
    # evaluation du numero de la ligne du feuillet 'Data' ou est trouve le code activite de l'activite i 
    if(length(which(as.matrix(vec_codesA) == as.character(config$A_code[i]))) > 0) {   # au moins une correspondance du code activite trouvee dans colonne Code#
      vec_A_ilindata <- which(as.matrix(vec_codesA) == as.character(config$A_code[i]))  
      # rajout condition : il s'agit du champ relatif a l'intensite
      if(length(vec_A_ilindata) > 1){
        config$A_ilindata[i] <- vec_A_ilindata[which(config$DBData$Data[vec_A_ilindata,i_Var_code]=="intensity")]
      }else{
        config$A_ilindata[i] <- vec_A_ilindata
      }
      
    }else{
      print(paste("#    :: pas de correspondance dans colonne Code",icol,sep=""))
    }
    
  } # boucle sur les colonnes Code#
} # boucle sur les pressions

# - creation de la liste reduite d'activites d'apres donnees disponibles et correspondances Ai et Aiag

# -- assignement d'un code agrege pour chaque activite Ai
for(i in 1:ni){ # boucle sur les activites decrites dans le feuillet 'Typo_activites'
  if(!is.na(config$A_ilindata)[i]){
    # - code agrege = concatenation des codes activites correspondant a un meme jeu de donnees
    vec <- config$A_code[which(config$A_ilindata==config$A_ilindata[i])]
    config$Aiag[i] <- vec[1]
    if(length(vec)>1){
      for(ivec in 2:length(vec)){
        config$Aiag[i] <- paste(config$Aiag[i],'oui',sep="")
        config$Aiag[i] <- paste(config$Aiag[i],vec[ivec],sep="")
      }
    }
  }
}

# -- nombre d'activites sur lesquelles porte l'evaluation : 
Codes_i_ag <- unique(config$Aiag[!is.na(config$Aiag)])
ni_ag     <- length(Codes_i_ag)

A_zi <-c()
vec_A_zi_names <- c()

# ouverture des tables, remplissage de la matrice A_iz et plot des donnees
vec_A_zi_names <- c()
for(i in 1:ni_ag){
  print("#")
  print(paste("# enregistrement et trace de la variable activite : ",Codes_i_ag[i] ))
  # numero de ligne de la table 'data' correspondant a l'activite
  ilin <- config$A_ilindata[which(config$Aiag==Codes_i_ag[i])[1]]
  
  Ai_nametab <- paste("A_",config$DBData$Data[ilin,i_Table_code],sep="")
  
  # recuperation de la table
  Ai_table   <- eval(parse(text=paste("list_sauv$",Ai_nametab,sep="",collapse=" ; ")))
  
  print(paste("# -> cette activite a un jeu de donnees : ",paste("list_sauv$",Ai_nametab,sep="",collapse=" ; ")))
  print(paste("# ->                             champ  : ",config$DBData$Data[ilin,i_Field]))
  
  # - subset de la couche SIG
  Ai_z <- subset(Ai_table, eval(parse(text=paste("Ai_table$id2 %in% grid_z@data@.Data[[",i_idz,"]]",sep="",collapse=" ; "))))
  
  # enregistrement des indices des mailles de la table dans colonne "mesh"
  if(i==1){
    A_zi    <-  col_idz
    vec_zid <- A_zi
  }
  # indice du champ de la table 'Data' rassemblant les donnees activites
  i_idAi    <- which(colnames(Ai_z@data)==config$DBData$Data[ilin,i_Field])
  
  # enregistrement dans la matrice A_zi des donnees activites
  
  # -- comme il peut y avoir plusieurs lignes pour une meme maille : moyenne des valeurs
  # -- cette fonction ordonne aussi les valeurs d'activites selon l'ordre des mailles de vec_zid
  vec <-  matrix(data=0, nrow=nz, ncol=1)
  for (j in 1:nz){
    if (vec_zid[j] %in% Ai_z@data@.Data[[which(colnames(Ai_z@data)=="id2")]]){
      vec2 <- subset(Ai_z@data,Ai_z@data@.Data[[which(colnames(Ai_z@data)=="id2")]]==vec_zid[j]) 
      vec[j] <- mean(as.numeric(unlist(vec2[i_idAi])),na.rm=T)
    } 
  }
  
  nameVARz <- config$Aiag[which(config$Aiag==Codes_i_ag[i])[1]]
  VARz <- cbind.data.frame(vec_zid,as.matrix(vec)) ; colnames(VARz) <- c("mesh",nameVARz)
  
  # -- remplissage de la matrice intensite des activites A_zi
  A_zi <- cbind.data.frame(A_zi,VARz[,2])
  
  vec_A_zi_names <- c(vec_A_zi_names,config$DBData$Data[ilin,i_Data_code])
  # plot de l'intensite de l'activite
  if(config$Outputs$lplot_Ai){
    title <- paste("intensite ",config$DBData$Data[ilin,i_Data_code]," (",config$DBData$Data[ilin,i_Unit],")",sep="")
    checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)
  }
} # fin boucle sur les activites Ai_ag
vec <-  c("mesh",Codes_i_ag)
colnames(A_zi) <-vec

# -------------------------------------------------------------
print("# calcul des indices multi-activites")

f_lognorm <- function(in.VARz){
  #test fonction : in.VARz <- (A_zi[,i+1])
  
  #log-transformation log[X+1] 
  y <- apply(as.matrix(c(in.VARz)),c(1),function(x) log(x+1.))
  # normalisation entre 0 et 1
  imin   <- min(y,na.rm=TRUE)
  irange <- range(y,na.rm=TRUE)
  if(diff(irange)==0){
    out <- y
  } else {
    out <- (y-imin)/diff(irange)
  }
  return(out)
}

IMA <- vec_zid
IMA <- cbind.data.frame(IMA,array(0,dim=(nz)))


# methode 1 = denombrement des mailles dont Ai > 0.
nameVARz <- "IMA_option1"

for(i in 2:(ni_ag+1)){ # Boucle sur les activites prises en compte
  A_iz_sup0 <-  array(0,dim=(nz)) # Initialisation d'un vecteur avec autant de zeros que de mailles
  ind_sup0  <- which(A_zi[,i] > 0.) # Identification des mailles ou l'activite i est non nulle
  A_iz_sup0[ind_sup0] <- 1  # On ajoute 1 a ces mailles
  IMA[,2]   <- IMA[,2] + A_iz_sup0 # On additionne par rapport aux precedentes activites
}
colnames(IMA) <- c("mesh",nameVARz)

VARz <- cbind.data.frame(vec_zid, as.numeric(IMA[,2])) ; colnames(VARz) <- c("mesh",nameVARz)

title <- paste("Indice multi-activites methode 1 ",sep="")
checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)

# methode 2 = somme des intensitees log_transformees et normalisees
IMA <- cbind.data.frame(IMA,array(0,dim=(nz)))
nameVARz <- "IMA_option2"
for(i in 2:(ni_ag+1)){ # Boucle sur toutes les activites
  A_iz_norm <-  f_lognorm(A_zi[,i]) # On normalise les donnees de l'activite i
  A_iz_norm <- replace(A_iz_norm,is.na(A_iz_norm),0.) # On remplace les NA par des 0
  IMA[,3]   <- as.numeric(IMA[,3]) + as.matrix(A_iz_norm) # On somme de cette maniere toutes les activites normalisees
}

colnames(IMA)[3] <- c(nameVARz)
IMA[,3] <- as.numeric(IMA[,3])/max(as.numeric(IMA[,3]), na.rm=T) # On ramene le resultat entre 0 et 1
VARz <- cbind.data.frame(vec_zid, IMA[,3]) ; colnames(VARz) <- c("mesh",nameVARz)

title <- paste("Indice multi-activites methode 2 ",sep="")
checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)

# Stockage des activites normalisees dans une matrice
A_zi_lognorm <- as.matrix(vec_zid)
for(i in 2:dim(A_zi)[2]){
  A_zi_lognorm <- cbind.data.frame(A_zi_lognorm,f_lognorm(A_zi[,i]))
  colnames(A_zi_lognorm)[i] <- paste(colnames(A_zi)[i], "_lognorm", sep="")
}
colnames(A_zi_lognorm)[1] <- "mesh"

print("# ")
print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 7 - CARTOGRAPHIE DES PRESSIONS"                                                      )
print("# ------------------------------------------------------------------------------------------")
print("#  > import de la matrice activites-pressions")

# - indices des debuts et fins des donnees dans la matrice (titres compris)
ideb_A <- 3
ifin_A <- length(config$Mat_AP[,1])

ideb_P <- 5  
ifin_P <- length(config$Mat_AP[1,])

# -- identification des lignes correspondant a l'evaluation "Final"
vec_lines <- which(config$Mat_AP[,1]=="Final")

# - liste des activites dans la matrice
list_Acode_MatAP <- config$Mat_AP[vec_lines,2]
list_Aname_MatAP<-character(length=length(list_Acode_MatAP))
for (i in 1:length(list_Aname_MatAP)) {
  if (is.na(config$Mat_AP[vec_lines[i],4])){
    list_Aname_MatAP[i]=config$Mat_AP[vec_lines[i],3]
  } else {
    list_Aname_MatAP[i]=paste(config$Mat_AP[vec_lines[i],3],config$Mat_AP[vec_lines[i],4],sep="_")
  }
}

# - liste des pressions dans la matrice
list_Pcode_MatAP <- config$Mat_AP[1,ideb_P:ifin_P] 
list_Pname_MatAP <- config$Mat_AP[2,ideb_P:ifin_P] 

# -- creation de la matrice utilisee pour l'evaluation et indices des dimensions
MatAP <- config$Mat_AP[vec_lines,ideb_P:ifin_P]
colnames(MatAP) <- list_Pcode_MatAP ; rownames(MatAP) <- list_Acode_MatAP 
ni_MatAP <- dim(MatAP) [1] ; nj_MatAP <- dim(MatAP) [2]

# -------------------------------------------------
print(paste("#  > Cartographie des pressions"))
 
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


# initialisation des matrices resultats
# - pression Pj generee par l activite Ai

# boucle pour calculer les intensites des pressions a partir des donnees activites et de la matrice activite-presssion
vec_controle_P <- array(0,dim=(nj)) # vecteur qui va permettre de suivre quelles pressions aregees ont deja ete calculees
Mat_PjAi_z    <- c() # Matrice intermediaire qui sera utilisee uniquement pour le calcul
Mat_Pj_z      <- c() # Matrice qui contiendra les donnees pressions
compteur_ij   <- 0 
vec_P_name_eval <- c() # vecteur qui contiendra les noms des pressions evaluees
vec_P_code_eval <- c() # vecteur qui contiendra les codes des pressions evaluees
# debut de la boucle sur les pressions
for(j in 1:nj){
  id_Pcode_corres <- which(P_code==config$P_code[j]) # position(s) dans le vecteur P_code) de la pression j
  if(length(id_Pcode_corres)==0){   #pression agregee car non presente dans P_code
    id_Pcode_corres <- which(P_code==config$P_code_ag[j])[1] #on va donc chercher la position dans le vcteur P_code_ag
  }
  if(config$P_evalDEM[j]==1){  #reduction du nombre de pressions pour le demonstrateur
    
    if(!is.na(config$P_code_ag[j])){    # si la pression est agregee
      # verification que la pression agregee n a pas deja ete calculee
      if(max(vec_controle_P[which(config$P_code_ag==config$P_code_ag[j])])==1){vec_controle_P[j] <- 1} # si la pression a deja ete calculee, on lui attribue 1 dans le vecteur controle pour ne pas refaire le calcul
      list_iP_ag <- config$P_code[which(config$P_code_ag==config$P_code_ag[j])] # stockage de tous les codes concernes par l'agregation
    }else{        # si la pression n'est pas une pression agregee
      list_iP_ag  <- config$P_code[j] # On stocke le code d'origine dans lisi_iP_ag
      
    }
    if(vec_controle_P[j]==0){    # On ne refait pas le calcul pour les pressions agregees deja calculees
      if(config$P_modcalc[j]==1){ # si la pression doit etre calculee a partir de la matrice activit pression (pas une pression directe)
        compteur_AP <- 0 # compteur determinant le nombre d'activite qui va generer la pression
        print("# ")
        print(paste("#  >> pression : ",list_iP_ag,"    -------------------------",sep=""))
        # numeros de colonne de la matrice activite pression associe(s) a(aux) pression(s) etudiee(s)
        col_MatAP <- c()
        for(ivec in 1:length(list_iP_ag)){
          col_MatAP <- c(col_MatAP,which(colnames(MatAP)==list_iP_ag[ivec]))
        }
        
        Mat_PjAi_Pj_z <- c() # Matrice intermediaire qui contiendra les activites generant la pression j
        # boucle sur les activites pour calcul de PjAi_z
        for(i in 2: dim(A_zi)[2]){  # !! premiere colonne = identifiants de maille
          compteur_ij <- compteur_ij + 1 # compteur determinant le nombre de liens activites pressions pris en compte dans l'evaluation
          PjAi_z <- array(0,dim=c(nz,2),dimnames=list(paste("z",c(1:nz),sep=""),c("mesh","PjAi"))) ### Quelques lignes plus bas, sa valeur change sans qu'il ai pu servir. Suppression ? 
          
          # nom de l'activite
          iA_code <- colnames(A_zi)[i]
          print("# ")
          print(paste("#  >> activite : ",iA_code,sep=""))
          
          # recherche de l'indice correspondant dans matrice AP
          if(iA_code %in% config$Aiag){ # activite agregee
            list_iA_ag <- config$A_code[which(config$Aiag==iA_code)]
          }else{ # activite non agregee
            list_iA_ag <- iA_code
          }
          # rangs de la matrice activite pression associes a l'activite i (ou aux activites pour les agregees)
          row_MatAP <- c()
          for(ivec in 1:length(list_iA_ag)){
            row_MatAP <- c(row_MatAP,which(rownames(MatAP)==list_iA_ag[ivec]))
          }
          
          nameVARz <- paste(P_code[id_Pcode_corres],"_vs_",iA_code,sep="")
          PjAi_z <- array(NA,dim=c(nz,1)) ; colnames(VARz) <- c(nameVARz)
          # nature du lien entre l'actiite i et la pression j
          interaction_AP <- f_verif_inter_MatAP(row_MatAP,col_MatAP,MatAP,'oui')
          
          if(interaction_AP){ # si l'activite Ai genere la pression Pj ...
            print(paste("# l'activite Ai : ", vec_A_zi_names[i-1] ,"    genere la pression Pj : ", P_name[id_Pcode_corres],sep=""))
            
            # log-transformation et normalisation (f_lognorm codee dans main.R)
            PjAi_z <- f_lognorm(A_zi[,i])
            
            #Comptage du nombre d'activites ayant genere la pression j
            compteur_AP <- compteur_AP + 1
            
          }else{
            print(paste("# l'activite Ai : ", vec_A_zi_names[i-1] ,"     NE GENERE PAS la pression Pj : ", P_name[id_Pcode_corres],sep=""))
          }
          
          # plot de PjAi_z
          VARz <- cbind.data.frame(as.matrix(vec_zid),as.matrix(PjAi_z)) ; colnames(VARz) <- c("mesh",nameVARz)
          
          # -- remplissage de la matrice intensite des pressions Mat_PjAi
          if(length(Mat_Pj_z)==0 && i==2){
            Mat_PjAi_z <- VARz
            Mat_Pj_z   <- VARz[,1]
          }
          if(i==2){
            Mat_PjAi_Pj_z <- VARz
          }
          if(i==2 && length(Mat_Pj_z) > 0){
            Mat_PjAi_z    <- cbind.data.frame(Mat_PjAi_z,PjAi_z)
            colnames(Mat_PjAi_z)[compteur_ij+1]<- nameVARz
          }
          if(i>2){
            Mat_PjAi_z    <- cbind.data.frame(Mat_PjAi_z,PjAi_z)
            Mat_PjAi_Pj_z <- cbind.data.frame(Mat_PjAi_Pj_z,PjAi_z)
            
            colnames(Mat_PjAi_z)[compteur_ij+1]<- nameVARz
            colnames(Mat_PjAi_Pj_z)  [i]     <- nameVARz          
          }
          
          # plot de l'intensite de la pression Pj generee par l'activite Ai
          if(config$Outputs$lplot_PjAi && interaction_AP){
            title <- paste(P_name[j] ," generee par : ",vec_A_zi_names[i-1],sep="")
            checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)
          }
          
        } # fin boucle sur les activites -------------------
        
        print("#     ")
        print(paste("# Evaluation des pressions multi-activites - pression Pj : ",P_name[id_Pcode_corres],sep=""))
        
        nameVARz   <- paste("Pj_multiAi_",P_code[id_Pcode_corres],sep="")
        
        # somme : si aucune activite ne genere la pression, on rajoute une colonne de NA, sinon on fait une somme des pressions
        if(compteur_AP==0){
          Mat_Pj_z <- cbind.data.frame(Mat_Pj_z,array(0.,dim=nz))
          Mat_Pj_z[,dim(Mat_Pj_z)[2]]<-as.numeric(Mat_Pj_z[,dim(Mat_Pj_z)[2]])
        } else {Mat_Pj_z <- cbind.data.frame(Mat_Pj_z,      
                                             apply(as.matrix(c(1:nz)),1,function(x) sum(Mat_PjAi_Pj_z[x,2:dim(Mat_PjAi_Pj_z)[2]],na.rm=TRUE)))
        }
        
      }
      if(config$P_modcalc[j]==2){ #Dans ce cas de figure, on va chercher les donnees pressions directement dans la BD sans passer par la MatAP
        print(paste("#  >> pression : ",list_iP_ag,"    -------------------------",sep=""))
        print("Pression directe independante de la matrice activite pression")
        for(icol in 1:nbcol){ #Boucle sur les colonnes Code dans le feuillet Data
          vec_codesA <- config$DBData$Data[,which(colnames(config$DBData$Data)==paste("Code",icol,sep=""))] #Colonne du feuillet Data associee au numero de code icod
          if(length(which(as.matrix(vec_codesA) == as.character(config$P_code[j]))) > 0){ #Si la pression est contenue dans la colonne
            P_ilindata <- which(as.matrix(vec_codesA) == as.character(config$P_code[j])) #On enregistre le numero de ligne associe a la pression
            Pj_nametab <- paste("P_",config$DBData$Data[P_ilindata,i_Table_code],sep="") 
            Pj_table   <- eval(parse(text=paste("list_sauv$",Pj_nametab,sep="",collapse=" ; ")))
            print(paste("# -> cette pression a un jeu de donnees : ",paste("list_sauv$",Pj_nametab,sep="",collapse=" ; ")))
            print(paste("# ->                             champ  : ",config$DBData$Data[P_ilindata,i_Field]))
            Pj_z <- subset(Pj_table, eval(parse(text=paste("Pj_table$id2 %in% grid_z@data@.Data[[",i_idz,"]]",sep="",collapse=" ; "))))
            i_idPj    <- which(colnames(Pj_z@data)==config$DBData$Data[P_ilindata,i_Field])
            vec <-  matrix(data=0, nrow=nz, ncol=1)
            for (i in 1:nz){
              if (vec_zid[i] %in% Pj_z@data@.Data[[which(colnames(Pj_z@data)=="id2")]]){
                vec2 <- subset(Pj_z@data,Pj_z@data@.Data[[which(colnames(Pj_z@data)=="id2")]]==vec_zid[i]) 
                vec[i] <- mean(as.numeric(unlist(vec2[i_idPj])),na.rm=T)
              } 
            }
            nameVARz   <- paste("Pj_multiAi_",P_code[id_Pcode_corres],sep="")
            VARz <- cbind.data.frame(as.matrix(vec_zid),as.matrix(vec)) ; colnames(VARz) <- c("mesh",nameVARz)
            if(j==1) {Mat_Pj_z <- VARz}
            else {Mat_Pj_z <- cbind.data.frame(Mat_Pj_z, VARz[,2])}
            
          }else{
            print(paste("#    :: pas de correspondance dans colonne Code",icol,sep=""))
          }
        }
        
      }
      colnames(Mat_Pj_z)[dim(Mat_Pj_z)[2]] <- P_code[id_Pcode_corres]
      
      if(config$Outputs$lplot_Pj){
        VARz <- cbind.data.frame(as.matrix(Mat_Pj_z[,1]),as.matrix(Mat_Pj_z[,dim(Mat_Pj_z)[2]])) ; colnames(VARz) <- c("mesh",nameVARz)
        
        title <- paste("Pression multi-activites : ",P_name[id_Pcode_corres],sep="")
        checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)
        
      }
      vec_P_name_eval <- c(vec_P_name_eval,P_name[id_Pcode_corres])
      vec_P_code_eval <- c(vec_P_code_eval,P_code[id_Pcode_corres])
      
    } #boucle vec_controle_P
    
  } #condition reduction du nombre de pressions pour le demonstrateur
  
  if(length(list_iP_ag)>1) {vec_controle_P[j]<-1}
  
} #boucle pressions
colnames(Mat_Pj_z)[1]="mesh"

print("# ")
print("# -----------------------------------------------")
print("#  > Cartographie de l'indice de pressions cumulees : IPC")

# Option 1: Nombre de pressions presentes par maille
# on initialise la matrice qui contiendra les deux indices avec la colonne identifiants
IPC_z <- vec_zid
IPC_z <- cbind.data.frame(IPC_z,array(0,dim=nz)) # on initialise l'IPC1 avec des 0
nameVARz <- "IPC_option1"
# Application de la meme methode de calcul que pour l'IMA1
for(i in 2:dim(Mat_Pj_z)[2]){
  MPjz_sup0 <- array(0,dim=(nz)) # Vecteur initialise avec des 0
  ind_sup0 <- which(Mat_Pj_z[,i] > 0.) # identification des mailles ayant une valeur >0 pour la pression i
  MPjz_sup0[ind_sup0] <- 1 # rajout d'un 1 dans le vecteur pour chaque maille ayant une valeur sup a 0
  IPC_z[,2] <- IPC_z[,2] + MPjz_sup0 # on aditionne les presence / absence a chaque pression de la boucle
}
colnames(IPC_z) <- c("mesh",nameVARz)

# production d'un fichier shp si l'option a ete parametree
VARz <- cbind.data.frame(vec_zid, as.numeric(IPC_z[,2])) ; colnames(VARz) <- c("mesh",nameVARz)
title <- ("Indice de pressions cumulees Option 1")
checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)


##Option 2: Somme des intensites des pressions normalisees
nameVARz <- "IPC_option2"

Mat_Pj_lognorm_z <- as.matrix(vec_zid) ; colnames(Mat_Pj_lognorm_z) <- c("mesh")

# normalisation des pressions dans une nouvelle matrice
for(j in 2:dim(Mat_Pj_z)[2]){
  Mat_Pj_lognorm_z <- cbind.data.frame(Mat_Pj_lognorm_z,f_lognorm(Mat_Pj_z[,j]))
  colnames(Mat_Pj_lognorm_z)[j] <- paste(vec_P_code_eval[j-1],"_lognorm",sep="")
}

# Application de la meme methode de calcul que pour l'IMA2
IPC_z <- cbind.data.frame(IPC_z,
                          apply(as.matrix(c(1:nz)),1,
                                function(x) 
                                  sum(Mat_Pj_lognorm_z[x,2:dim(Mat_Pj_lognorm_z)[2]],na.rm=TRUE))
)
IPC_z[,3] <- as.numeric(IPC_z[,3])/max(as.numeric(IPC_z[,3]), na.rm=T) # On ramene le resultat entre 0 et 1

colnames(IPC_z)[3] <- c(nameVARz)

VARz <- cbind.data.frame(vec_zid, as.numeric(IPC_z[,3])) ; colnames(VARz) <- c("mesh",nameVARz)
title <- ("Indice de pressions cumulees Option 2")
checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)

print("# ")
print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 8 - CARTOGRAPHIE DU RISQUE D'EXPOSITION PRESSION / MULTI-PRESSIONS                              "                                                      )
print("# ------------------------------------------------------------------------------------------")
print("#  > cartographie du risque d'exposition a chaque pression")

# on initialise la matrice avec la colonne identifiants
Mat_REX_PjE1_z <- vec_zid

# boucle sur les pressions
for(j in 2:dim(Mat_Pj_z)[2]){
  # exposition = pression * surface d'habitats dans la maille
  Mat_REX_PjE1_z  <- cbind.data.frame(Mat_REX_PjE1_z ,Mat_Pj_z[,j]*lHabBenth_z$surfhab_z)
  
  # nom de la colonne
  colnames(Mat_REX_PjE1_z)[j] <- paste("REX_E1_",colnames(Mat_Pj_z)[j],sep="")
  
  # export de l'expositionn a chaque pression sous format shp si on a parametre l'option
  if(config$Outputs$lplot_REX_Pj){
    VARz <-  cbind.data.frame(vec_zid, Mat_REX_PjE1_z[,j])
    colnames(VARz) <-  c("mesh",paste("REX_E1_",colnames(Mat_Pj_z)[j],sep=""))
    nameVARz <- paste("REX_E1_",colnames(Mat_Pj_z)[j],sep="")
    title <- paste("REX E1 pression",vec_P_name_eval[j-1])
    checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)
  }
}

print("#  > cartographie du risque d'exposition multi-pressions")
REXC_E1 <- vec_zid
REXC_E1 <- cbind.data.frame(REXC_E1, apply(as.matrix(c(1:nz)),1,function(x) 
  sum(Mat_REX_PjE1_z[x,2:dim(Mat_REX_PjE1_z)[2]],na.rm=TRUE)))  
colnames(REXC_E1) <- c("mesh","REXC_E1")

VARz <-  REXC_E1
nameVARz <- "REXC_E1"
title <- "REX pressions concommitantes sur E1"
checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)


print("# ")
print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 9 - CALCUL DU RISQUE D'EFFETS // RISQUE D'EFFETS CONCONMITANTS                      ")
print("# ------------------------------------------------------------------------------------------")
print("#  > cartographie du risque d'effet de la pression Pj sur E1")
# comme la surface d'habitats est deja prise en compte dans calcul sensibilite cumulee, 
# on ne prend pas l'exposition pour calculer REF_Pj mais Pj normalisee
Mat_REF_PjE1_z <- vec_zid

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
ncol <- dim(Mat_Sensib_Pj)[2]
Mat_Sensib_Pj_in <- Mat_Sensib_Pj

# Normalisation des sensibilites
Mat_Sensib_Pj_in <- f_norm(Mat_Sensib_Pj)


# Boucle sur les pressions
for(j in 2:dim(Mat_Pj_lognorm_z)[2]){
  # identification du numero de colonne de la matrice sensibilite associee a la pression j
  j_Sensib <- which(colnames(Mat_Sensib_Pj)==paste("SensibC_",colnames(Mat_Pj_z)[j],sep=""))
  # Remplissage colonne par colonne de la nouvelle matrice : produit de la pression normalisee et de la sensibilite normalisee
  Mat_REF_PjE1_z  <- cbind.data.frame(Mat_REF_PjE1_z ,Mat_Pj_lognorm_z[,j]*Mat_Sensib_Pj_in[,j_Sensib])
  # nom de la colonne
  colnames(Mat_REF_PjE1_z)[j] <- paste("REF_E1_",colnames(Mat_Pj_lognorm_z)[j],sep="")
  
  # export de chaque risque d'effet par pression si l'option a ete choisie
  if(config$Outputs$lplot_REF_Pj){
    VARz <-  cbind.data.frame(vec_zid, Mat_REF_PjE1_z[,j])
    colnames(VARz) <-  c("mesh",paste("REF_E1_",colnames(Mat_Pj_z)[j],sep=""))
    nameVARz <- paste("REF_E1_",colnames(Mat_Pj_z)[j],sep="")
    title <- paste("REF E1 pression",vec_P_name_eval[j-1])
    checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)
  }
}
colnames(Mat_REF_PjE1_z)[1] <- "mesh"
print("#       ")
print("#  > cartographie du risque d'effets concomitants : REFC")
# Calcul du risque d'effet
REFC_E1 <- vec_zid # remplissage de la colonne identifiant
REFC_E1 <- cbind.data.frame(REFC_E1, array(NA, dim=nz)) # - initialisation de la colonne qui contiendra le risque d'effet
for (i in 1:nz) { # boucle sur toutes les mailles
  if (length(Mat_REF_PjE1_z[i,2:dim(Mat_REF_PjE1_z)[2]][!is.na(Mat_REF_PjE1_z[i,2:dim(Mat_REF_PjE1_z)[2]])])>0) { # si au moins un des risques d'effet par pression est connu
    REFC_E1[i,2] <- sum(Mat_REF_PjE1_z[i,2:dim(Mat_REF_PjE1_z)[2]][which(!is.na(Mat_REF_PjE1_z[i,2:dim(Mat_REF_PjE1_z)[2]]))]) # on fait la somme des risques d'effets par pression non exclus par le seuil
  }
}
# _ on ramene le resultat entre 0 et 1
REFC_E1[,2] <- as.numeric(REFC_E1[,2])/max(as.numeric(REFC_E1[,2]), na.rm=T)
# - changement du nom des colonnes
colnames(REFC_E1) <- c("mesh","REFC_E1")

# - export sous forme de fichier shp
VARz <-  REFC_E1
nameVARz <- "REFC_E1"
title <- "REF pressions concommitantes sur E1"
checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)





print("# ")
print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 10 - REALISATION ET SAUVEGARDE DE GRAPHIQUES REPRESENTATIFS DES RESULTATS           ")
print("# ------------------------------------------------------------------------------------------")

#Enregistrement du fichier de sortie
graphdir <- paste(outdir,"graphs",sep="/")

#ecriture d'une fonction permettant a partir d'un nombre x de categorie et d'un vecteur de diviser les valeurs du vacteur en x categorie (intervalles egaux) 
#La fonction renvoie un tableau contenant pour chaque intervalle le nombre total et le pourcentage d'elements du vecteur se trouvant dans cette categorie
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

print("# ----------------------------------------------------------------")
print("1ere SERIE DE GRAPHIQUES: HISTOGRAMMES DE DISTRIBUTION DES VALEURS")
print("# ----------------------------------------------------------------")

##Histogramme de repartition des valeurs pour l'IMA 1
IMA1_plot <- ggplot(IMA, aes(x=IMA[,2])) 
IMA1_plot <- IMA1_plot +  geom_bar(aes(y = (..count..)/sum(..count..)*100), fill="#959ce5")
IMA1_plot <- IMA1_plot + xlab("IMA1") + ylab("Pourcentage(%)") 
IMA1_plot <- IMA1_plot + theme_bw()+theme(axis.title = element_text(size=19), axis.text = element_text(size=15))
IMA1_plot <- IMA1_plot + scale_x_continuous(breaks=c(0:max(IMA[,2])))
#Export de l'histogramme
namefile <- "Histogramme_IMA1.jpeg"
if (!file.exists(paste(graphdir, namefile, sep="/"))){
  ggsave(namefile, plot = IMA1_plot, device = "jpeg" , path = graphdir, width=22.5, height=17, units=c("cm"))
  print(paste("Export du fichier-----",namefile))
} else {
  print(paste("le fichier-----",namefile,"--------existe deja"))
}

##Histogramme de repartition des valeurs par classe pour l'IMA 2
test<-prep_graph(IMA[,3],5)
IMA2_plot <- ggplot(data=test, aes(x=Cat, y=Pourcentage))+geom_bar(stat="identity", width=0.6, fill="#959ce5")
IMA2_plot <- IMA2_plot + theme_bw()+theme(axis.title = element_text(size=19), axis.text = element_text(size=15))
IMA2_plot <- IMA2_plot + xlab("IMA2") + ylab("Pourcentage(%)")
#Export de l'histogramme
namefile <- "Histogramme_IMA2.jpeg"
if (!file.exists(paste(graphdir, namefile, sep="/"))){
  ggsave(namefile, plot = IMA2_plot, device = "jpeg" , path = graphdir, width=22.5, height=17, units=c("cm"))
  print(paste("Export du fichier-----",namefile))
} else {
  print(paste("le fichier-----",namefile,"--------existe deja"))
}

##Histogramme de repartition des valeurs pour l'IPC 1
IPC1_plot <- ggplot(IPC_z, aes(x=IPC_z[,2])) 
IPC1_plot <- IPC1_plot +  geom_bar(aes(y = (..count..)/sum(..count..)*100), fill="#959ce5")
IPC1_plot <- IPC1_plot + xlab("IPC1") + ylab("Pourcentage(%)") 
IPC1_plot <- IPC1_plot + theme_bw()+theme(axis.title = element_text(size=19), axis.text = element_text(size=15))
IPC1_plot <- IPC1_plot + scale_x_continuous(breaks=c(0:max(IPC_z[,2])))
#Export de l'histogramme
namefile <- "Histogramme_IPC1.jpeg"
if (!file.exists(paste(graphdir, namefile, sep="/"))){
  ggsave(namefile, plot = IPC1_plot, device = "jpeg" , path = graphdir, width=22.5, height=17, units=c("cm"))
  print(paste("Export du fichier-----",namefile))
} else {
  print(paste("le fichier-----",namefile,"--------existe deja"))
}

##Histogramme de repartition des valeurs par classe pour l'IPC 2
test<-prep_graph(IPC_z[,3],5)
IPC2_plot <- ggplot(data=test, aes(x=Cat, y=Pourcentage))+geom_bar(stat="identity", width=0.6, fill="#959ce5")
IPC2_plot <- IPC2_plot + theme_bw()+theme(axis.title = element_text(size=19), axis.text = element_text(size=15))
IPC2_plot <- IPC2_plot + xlab("IPC2") + ylab("Pourcentage(%)")
#Export de l'histogramme
namefile <- "Histogramme_IPC2.jpeg"
if (!file.exists(paste(graphdir, namefile, sep="/"))){
  ggsave(namefile, plot = IPC2_plot, device = "jpeg" , path = graphdir, width=22.5, height=17, units=c("cm"))
  print(paste("Export du fichier-----",namefile))
} else {
  print(paste("le fichier-----",namefile,"--------existe deja"))
}

##Histogramme de repartition des valeurs par classe pour le REFC
test<-prep_graph(REFC_E1[which(!is.na(REFC_E1[,2])),2],5)
REFC_plot <- ggplot(data=test, aes(x=Cat, y=Pourcentage))+geom_bar(stat="identity", width=0.6, fill="#959ce5")
REFC_plot <- REFC_plot + theme_bw()+theme(axis.title = element_text(size=19), axis.text = element_text(size=15))
REFC_plot <- REFC_plot + xlab("REFC") + ylab("Pourcentage(%)")
REFC_plot <- REFC_plot + scale_x_discrete(limits = test[,1]) 
#Export de l'histogramme
namefile <- "Histogramme_REFC.jpeg"
if (!file.exists(paste(graphdir, namefile, sep="/"))){
  ggsave(namefile, plot = REFC_plot, device = "jpeg" , path = graphdir, width=22.5, height=17, units=c("cm"))
  print(paste("Export du fichier-----",namefile))
} else {
  print(paste("le fichier-----",namefile,"--------existe deja"))
}



print("# ------------------------------------------------------------")
print("2eme SERIE DE GRAPHIQUES: BOXPLOTS DES RELATIONS ENTRE INDICES")
print("# ------------------------------------------------------------")

##Rassemblement des 5 indices d'interet dans un meme dataframe
data_index <- cbind.data.frame(REFC_E1,IMA[,2:3],IPC_z[,2:3])
data_index$IMA_option1 <- as.factor(data_index$IMA_option1)
data_index$IPC_option1 <- as.factor(data_index$IPC_option1)
sub_data_index <- data_index[which(!is.na(data_index[,2])),]

#1er Boxplot: IMA1 en fonction de l'IMA2 (Somme intensites des activite en fonction du nombre d'activites)
IMA1_IMA2_plot <- ggplot(data_index, aes(x=IMA_option1, y=IMA_option2)) + geom_boxplot(color="#2f367f", fill="#dbdef6")
IMA1_IMA2_plot <- IMA1_IMA2_plot + xlab("IMA1 : nombre d'activites") + ylab("IMA2 : somme de l'intensite des activites")
IMA1_IMA2_plot <- IMA1_IMA2_plot + theme_bw() + scale_fill_brewer(palette="Blues")
IMA1_IMA2_plot <- IMA1_IMA2_plot + theme(axis.title = element_text(size=19), axis.text = element_text(size=15), legend.position='none')
#Export du Boxplot
namefile <- "Boxplot_IMA1_IMA2.jpeg"
if (!file.exists(paste(graphdir, namefile, sep="/"))){
  ggsave(namefile, plot = IMA1_IMA2_plot, device = "jpeg" , path = graphdir, width=22.5, height=17, units=c("cm"))
  print(paste("Export du fichier-----",namefile))
} else {
  print(paste("le fichier-----",namefile,"--------existe deja"))
}


#2eme Boxplot: IPC1 en fonction de l'IPC2 (Somme intensites des pressions en fonction du nombre de pressions)
IPC1_IPC2_plot <- ggplot(data_index, aes(x=IPC_option1, y=IPC_option2)) + geom_boxplot(color="#2f367f", fill="#dbdef6")
IPC1_IPC2_plot <- IPC1_IPC2_plot + xlab("IPC1 : nombre de pressions") + ylab("IPC2 : somme de l'intensite des pressions")
IPC1_IPC2_plot <- IPC1_IPC2_plot + theme_bw() + scale_fill_brewer(palette="Blues")
IPC1_IPC2_plot <- IPC1_IPC2_plot + theme(axis.title = element_text(size=19), axis.text = element_text(size=15), legend.position='none')
#Export du Boxplot
namefile <- "Boxplot_IPC1_IPC2.jpeg"
if (!file.exists(paste(graphdir, namefile, sep="/"))){
  ggsave(namefile, plot = IPC1_IPC2_plot, device = "jpeg" , path = graphdir, width=22.5, height=17, units=c("cm"))
  print(paste("Export du fichier-----",namefile))
} else {
  print(paste("le fichier-----",namefile,"--------existe deja"))
}



#3eme Boxplot: REFC en fonction de l'IMA1 (risque d'effets concomitants en fonction du nombre d'activites)
REFC_IMA1_plot <- ggplot(sub_data_index, aes(x=IMA_option1, y=REFC_E1)) + geom_boxplot(color="#2f367f", fill="#dbdef6")
REFC_IMA1_plot <- REFC_IMA1_plot + xlab("IMA1 : nombre d'activites") + ylab("REFC")
REFC_IMA1_plot <- REFC_IMA1_plot + theme_bw() + scale_fill_brewer(palette="Blues")
REFC_IMA1_plot <- REFC_IMA1_plot + theme(axis.title = element_text(size=19), axis.text = element_text(size=15), legend.position='none')
#Export du Boxplot
namefile <- "Boxplot_REFC_IMA1.jpeg"
if (!file.exists(paste(graphdir, namefile, sep="/"))){
  ggsave(namefile, plot = REFC_IMA1_plot, device = "jpeg" , path = graphdir, width=22.5, height=17, units=c("cm"))
  print(paste("Export du fichier-----",namefile))
} else {
  print(paste("le fichier-----",namefile,"--------existe deja"))
}



#4eme Boxplot: REFC en fonction de l'IPC1 (risque d'effets concomitants en fonction du nombre de pressions)
REFC_IPC1_plot <- ggplot(sub_data_index, aes(x=IPC_option1, y=REFC_E1)) + geom_boxplot(color="#2f367f", fill="#dbdef6")
REFC_IPC1_plot <- REFC_IPC1_plot + xlab("IPC1 : nombre de pressions") + ylab("REFC")
REFC_IPC1_plot <- REFC_IPC1_plot + theme_bw() + scale_fill_brewer(palette="Blues")
REFC_IPC1_plot <- REFC_IPC1_plot + theme(axis.title = element_text(size=19), axis.text = element_text(size=15), legend.position='none')
#Export du Boxplot
namefile <- "Boxplot_REFC_IPC1.jpeg"
if (!file.exists(paste(graphdir, namefile, sep="/"))){
  ggsave(namefile, plot = REFC_IPC1_plot, device = "jpeg" , path = graphdir, width=22.5, height=17, units=c("cm"))
  print(paste("Export du fichier-----",namefile))
} else {
  print(paste("le fichier-----",namefile,"--------existe deja"))
}



#5eme graphique: REFC en fonction de l'IPC2 (risque d'effets concomitants en fonction de l'intensite cumulee des pressions)
REFC_IPC2_plot <- ggplot(sub_data_index, aes(x=IPC_option2, y=REFC_E1))+geom_point()
REFC_IPC2_plot <- REFC_IPC2_plot + xlab("IPC2") + ylab("REFC")
REFC_IPC2_plot <- REFC_IPC2_plot + theme_bw()
REFC_IPC2_plot <- REFC_IPC2_plot + theme(axis.title = element_text(size=19), axis.text = element_text(size=15), legend.position='none')
#Export du Boxplot
namefile <- "Boxplot_REFC_IPC2.jpeg"
if (!file.exists(paste(graphdir, namefile, sep="/"))){
  ggsave(namefile, plot = REFC_IPC2_plot, device = "jpeg" , path = graphdir, width=22.5, height=17, units=c("cm"))
  print(paste("Export du fichier-----",namefile))
} else {
  print(paste("le fichier-----",namefile,"--------existe deja"))
}




print("# -----------------------------------------------------------------------")
print("3eme SERIE DE GRAPHIQUES: HISTOGRAMMES BASES SUR LES PRESSIONS DOMINANTES")
print("# -----------------------------------------------------------------------")


# creation d'une premiere matrice indiquant pour chaque maille quelle est la (ou les dans le cas d'egalites) pression(s) dominante(s)
Mat_P_domi <- as.data.frame(matrix(data = NA, nrow <- nz, ncol <- dim(Mat_Pj_z)[2]-1))
for(i in 1:nz){
  vec<-as.numeric(sort(Mat_Pj_lognorm_z[i,2:dim(Mat_Pj_lognorm_z)[2]], decreasing=T, na.last=TRUE))
  if(vec[1]>0){
    vec_Pdomi<-colnames(Mat_Pj_z)[which(Mat_Pj_lognorm_z[i,2:dim(Mat_Pj_lognorm_z)[2]]==vec[1])+1]
    for(j in 1:length(vec_Pdomi)){
      Mat_P_domi[i,j]=vec_Pdomi[j]
    }
  }
}
indic<-1
while (length(which(!is.na(Mat_P_domi[1:nz,indic])))>0){
  indic <- indic + 1
}
Mat_P_domi <- Mat_P_domi[,1:indic]
nPj <- dim(Mat_Pj_z)[2]-1

# creation d'une deuxieme matrice qui servira de base pour les graphiques
# Elle regroupe pour chaque pressions les informations statistiques necessaires au graphes ainsi que le nom des pressions
Mat_P_domi_stat <- as.data.frame(matrix(data = NA, nrow <- nPj, ncol <- 9))
colnames(Mat_P_domi_stat) <- c("Pj","Percentage","IPC_moy","IPC_sd","IMA_moy","IMA_sd","REFC_moy","REFC_sd","P_name")
Mat_P_domi_stat[,1] <- colnames(Mat_Pj_z)[2:dim(Mat_Pj_z)[2]]

for(i in 1:nPj){
  P_i <- Mat_P_domi_stat[i,1]
  ind_Pi_Domi <- array(0, dim=(nz))
  for (j in 1:dim(Mat_P_domi)[2]){
    ind_Pij_Domi <- array(0, dim=(nz))
    ind_Pi_Domi_col_i <- which(Mat_P_domi[,j]==P_i)
    ind_Pij_Domi[ind_Pi_Domi_col_i] <-1
    ind_Pi_Domi <- ind_Pi_Domi + ind_Pij_Domi
  }
  data_index_Pj_Domi <- data_index[which(ind_Pi_Domi==1),]
  sub_data_index_Pj_Domi <- data_index_Pj_Domi[which(!is.na(data_index_Pj_Domi[,2])),]
  Mat_P_domi_stat[i,2] <- round((dim(data_index_Pj_Domi)[1]/nz)*100,2)
  Mat_P_domi_stat[i,3] <- mean(data_index_Pj_Domi[,6])
  Mat_P_domi_stat[i,4] <- sd(data_index_Pj_Domi[,6])
  Mat_P_domi_stat[i,5] <- mean(data_index_Pj_Domi[,4])
  Mat_P_domi_stat[i,6] <- sd(data_index_Pj_Domi[,4])
  Mat_P_domi_stat[i,7] <- mean(sub_data_index_Pj_Domi[,2])
  Mat_P_domi_stat[i,8] <- sd(sub_data_index_Pj_Domi[,2])
}
Mat_P_domi_stat <- Mat_P_domi_stat[which(!is.na(Mat_P_domi_stat[,5])),]
for (i in 1:dim(Mat_P_domi_stat)[1]) {
  Mat_P_domi_stat[i,9] <- P_name[which(P_code==Mat_P_domi_stat[i,1])]
}

# Representation des 4 histogrammes sur un meme graphique
# Construction d'une matrice commpatible avec le facet_grid de ggplot2
Mat_P_domi_stat <- Mat_P_domi_stat[order(Mat_P_domi_stat[,2],decreasing=T), ]
P_domi_IMA <- cbind(Mat_P_domi_stat[,9],Mat_P_domi_stat[,5:6],array(" IMA2 moyen ", dim=dim(Mat_P_domi_stat)[1]),array(1, dim=dim(Mat_P_domi_stat)[1]))
colnames(P_domi_IMA) <- c("Pression","Moy","SD","Cat","facet_order")

P_domi_Percent <- as.data.frame(cbind(Mat_P_domi_stat[,9],Mat_P_domi_stat[,2],array(0., dim=dim(Mat_P_domi_stat)[1]),array(" Pourcentage de mailles par pression dominante ", dim=dim(Mat_P_domi_stat)[1])))
P_domi_Percent <- cbind(P_domi_Percent,array(4, dim=dim(Mat_P_domi_stat)[1]))
colnames(P_domi_Percent) <- c("Pression","Moy","SD","Cat","facet_order")

P_domi_IPC <- cbind(Mat_P_domi_stat[,9],Mat_P_domi_stat[,3:4],array(" IPC2 moyen ", dim=dim(Mat_P_domi_stat)[1]),array(2, dim=dim(Mat_P_domi_stat)[1]))
colnames(P_domi_IPC) <- c("Pression","Moy","SD","Cat","facet_order")

P_domi_REFC <- cbind(Mat_P_domi_stat[,9],Mat_P_domi_stat[,7:8],array(" REFC moyen ", dim=dim(Mat_P_domi_stat)[1]),array(3, dim=dim(Mat_P_domi_stat)[1]))
colnames(P_domi_REFC) <- c("Pression","Moy","SD","Cat","facet_order")

Mat_stat_bis <- rbind(P_domi_REFC,P_domi_IMA,P_domi_IPC,P_domi_Percent)
# Mat_stat_bis <- rbind(P_domi_REFC,P_domi_IPC,P_domi_IMA,P_domi_Percent)

Mat_stat_bis[,4] <- as.factor(Mat_stat_bis[,4])
Mat_stat_bis[,3] <- as.numeric(Mat_stat_bis[,3])
Mat_stat_bis[,2] <- as.numeric(Mat_stat_bis[,2])
Mat_stat_bis[,5] <- as.factor(Mat_stat_bis[,5])
# Realisation du graphe
Pdomi_plot <- ggplot(data=Mat_stat_bis, aes(x=Pression, y=Moy, fill=Cat))+geom_bar(stat="identity", width=0.6)+ facet_grid(facet_order~., scale="free")
Pdomi_plot <- Pdomi_plot + theme_bw()+theme(legend.position="bottom",legend.title = element_blank(), legend.text = element_text(size=12), strip.text = element_blank(), axis.title = element_blank(), axis.text.x = element_text(size=10, angle=70, hjust=1), axis.text.y = element_text(size=10))
Pdomi_plot <- Pdomi_plot + scale_fill_manual(values=c("#feda75","#fa7e1e","#d62976","#962fbf"))
# #feda75:IMA2 moyen,#fa7e1e:IPC2 moyen,#d62976: pourcentage maille par pression dominante,#962fbf: REFC moyen
Pdomi_plot <- Pdomi_plot + geom_errorbar(aes(ymin=Moy-SD, ymax=Moy+SD), width=.2, position=position_dodge(0.9))
Pdomi_plot <- Pdomi_plot + scale_x_discrete(limits=Mat_P_domi_stat[,9])
#Export du graphe
namefile <- "synthese_pressions_dominantes.jpeg"
if (!file.exists(paste(graphdir, namefile, sep="/"))){
  ggsave(namefile, plot = Pdomi_plot, device = "jpeg" , path = graphdir, width=22.5, height=23.5, units=c("cm"))
  print(paste("Export du fichier-----",namefile))
} else {
  print(paste("le fichier-----",namefile,"--------existe deja"))
}

print("# -----------------------------------------------------------------------")
print("4eme SERIE DE GRAPHIQUES: PRESENCE ET IMPACT DES ACTIVITES ET DES PRESSIONS")
print("# -----------------------------------------------------------------------")

# 1er Graphique: Histogramme indiquant pour chaque activite :
# le pourcentage de maille ou l'activite a un impact
# le pourcentage de mailles ou l'activite a un impact
# la contribution de l'activite au risque d'effet total
# 1ere etape: construction d'une matrice indiquant pour chaque maille la contribution de chaque activite au risque d'effet total
Contrib_Ai <- cbind.data.frame(as.matrix(vec_zid), matrix(data=0, nrow=nz, ncol=dim(A_zi)[2]-1))
colnames(Contrib_Ai) <- colnames(A_zi)
Contrib_Ai[,1] <- as.character(Contrib_Ai[,1]) #Colonne contenant l'identifiant des mailles
for(i in 2:dim(Contrib_Ai)[2]){ #Boucle sur les activites
  Contrib_Ai[,i] <- as.numeric(as.character(Contrib_Ai[,i]))
  name_Ai <- colnames(Contrib_Ai[i])
  for(j in 2:dim(Mat_Pj_z)[2]){ #Boucle sur les pressions
    name_Pj <- colnames(Mat_Pj_z)[j]
    if(name_Pj %in% config$P_code) {id_calc <- which(config$P_code==name_Pj)}
    if(name_Pj %in% config$P_code_ag) {id_calc <- which(config$P_code_ag==name_Pj)[1]}
    if(config$P_modcalc[id_calc]==1){
      name_PjAi <- paste(name_Pj, "_vs_", name_Ai, sep="") #nom de colonne de la matrice Mat_PjAi_z correspondant au couple activite pression
      active <- which(Mat_PjAi_z[,which(colnames(Mat_PjAi_z)==name_PjAi)]>0) 
      if (length(active)>0){ #Si aucune valeur positive dans la maille, a priori pas de lien activite pression et donc contribution au risque d'effet nulle
        PjAi <- array(0,dim=c(nz,1))
        PjAi[active] <- A_zi_lognorm[active,i]/Mat_Pj_z[active,j] #Le ratio Ai/Pj indique le pourcentage du REFC_Pj attribuable a Ai
        PjAi <- PjAi*as.numeric(as.character(Mat_REF_PjE1_z[,j])) #On multiplie par REFC_Pj pour avoir la contribution reelle
        PjAi[which(is.na(PjAi))] <- 0
        Contrib_Ai[,i] <- Contrib_Ai[,i]+PjAi #On somme ces contributions a chaque REFCPj pour avoir la part du REFC_total attribuable a Ai
      }
    }
  }
}
sub_th <- which(!is.na(REFC_E1[,2])) #On ne va faire les statistiques que sur les mailles non exclues par le seuil
# 2eme etape: creation de la matrice contenant les informations necessaires aux graphes
Stat_Ai <- as.data.frame(matrix(data = NA, nrow <- 3*(dim(A_zi)[2]-1), ncol <- 3))
Stat_Ai[,3] <- as.numeric(Stat_Ai[,3])
colnames(Stat_Ai) <- c("Activite","Statistique","Pourcentage")
for (i in 1:(dim(A_zi)[2]-1)){
  j <- 3*i - 2
  Stat_Ai[j,1] <- vec_A_zi_names[i]
  Stat_Ai[j+1,1] <- vec_A_zi_names[i]
  Stat_Ai[j+2,1] <- vec_A_zi_names[i]
  Stat_Ai[j,2] <- " presence sur la zone "
  Stat_Ai[j+1,2] <- " mailles impactees "
  Stat_Ai[j+2,2] <- " contribution au refc total "
  Stat_Ai[j,3] <- (length(which(A_zi[sub_th,i+1]>0))/length(sub_th))*100 #ratio du nombre de maille ou l'activite est positive (donc presente) sur le nombre de mailles dans la zone etudiee  
  Stat_Ai[j+1,3] <- (length(which(Contrib_Ai[sub_th,i+1]>0))/length(sub_th))*100 #ratio du nombre de maille ou la contribution de l'activite est positive (impact de l'activite) sur le nombre de mailles dans la zone d'etude. 
  Stat_Ai[j+2,3] <- (sum(Contrib_Ai[,i+1])/sum(Contrib_Ai[,2:dim(Contrib_Ai)[2]]))*100 #Somme des contributions dues a cette activite divisee par la somme de tous les risques d'effets.
}
# 3eme etape: creation du graphe
Ai_Bilan <- ggplot(Stat_Ai, aes(x=Activite, y=Pourcentage, fill=Statistique)) + geom_bar(stat="identity", position=position_dodge(), width = 0.7) + coord_flip()
Ai_Bilan <- Ai_Bilan + scale_x_discrete(limits = Stat_Ai[which(Stat_Ai[,2]==" contribution au refc total ")[order(Stat_Ai[which(Stat_Ai[,2]==" contribution au refc total "),3])], 1])
Ai_Bilan <- Ai_Bilan + scale_fill_manual(values=c("#4C6B4F","#56C46A","#A3E589"),  guide = guide_legend(reverse = TRUE))
Ai_Bilan <- Ai_Bilan + theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(size=12), axis.title = element_text(size = 12), axis.text = element_text(size=12))
# #4C6B4F=contribution au refc total, #56C46A=mailles impactees,#A3E589=presence sur la zone
# 4eme etape: Export du graphe
namefile <- "Bilan_activites.jpeg"
if (!file.exists(paste(graphdir, namefile, sep="/"))){
  ggsave(namefile, plot = Ai_Bilan, device = "jpeg" , path = graphdir, width=34, height=18, units=c("cm"))
  print(paste("Export du fichier-----",namefile))
} else {
  print(paste("le fichier-----",namefile,"--------existe deja"))
}

# 2eme Graphique: Histogramme indiquant pour chaque pression :
# le pourcentage de maille ou la pression a un impact
# le pourcentage de mailles ou la pression a un impact
# la contribution de la pression au risque d'effet total
# 1ere etape: construction et remplissage de la matrice qui contiendra l'ensemble des informations necessaire a la realisation du graphe
Stat_Pj <- as.data.frame(matrix(data = NA, nrow <- 3*(dim(Mat_Pj_z)[2]-1), ncol <- 3))
Stat_Pj[,3] <- as.numeric(Stat_Pj[,3])
colnames(Stat_Pj) <- c("Pression","Statistique","Pourcentage")
for (i in 1:(dim(Mat_Pj_z)[2]-1)){ #Boucle sur les pressions
  name_Pj <- paste("REF_E1_",colnames(Mat_Pj_z)[i+1],"_lognorm",sep="") #Colonne de la matrice Mat_REF_PjE1_z associee a la pression d'interet
  icol_REF <- which(colnames(Mat_REF_PjE1_z)==name_Pj)
  j <- 3*i - 2
  Stat_Pj[j,1] <- P_name[which(P_code==colnames(Mat_Pj_z)[i+1])]
  Stat_Pj[j+1,1] <- P_name[which(P_code==colnames(Mat_Pj_z)[i+1])]
  Stat_Pj[j+2,1] <- P_name[which(P_code==colnames(Mat_Pj_z)[i+1])]
  Stat_Pj[j,2] <- " presence sur la zone "
  Stat_Pj[j+1,2] <- " mailles impactees "
  Stat_Pj[j+2,2] <- " contribution au refc total "
  Stat_Pj[j,3] <- (length(which(Mat_Pj_z[sub_th,i+1]>0))/length(sub_th))*100 #ratio nb de mailles ou la pression est presente/ nb de mailles sur la zone d'etude
  Stat_Pj[j+1,3] <- (length(which(Mat_REF_PjE1_z[sub_th,icol_REF]>0))/length(sub_th))*100 #ratio nb de mailles ou REFC-Pj >0 (et donc ou la pression a un impact) / nb de mailles sur la zone d'etude
  Stat_Pj[j+2,3] <- (sum(Mat_REF_PjE1_z[sub_th,icol_REF], na.rm=T)/sum(Mat_REF_PjE1_z[sub_th,2:dim(Mat_REF_PjE1_z)[2]], na.rm=T))*100 #Somme des REFC_Pj sur somme des REFC
}
# 2eme etape: creation du graphe
Bilan_Pj <- ggplot(Stat_Pj, aes(x=Pression, y=Pourcentage, fill=Statistique)) + geom_bar(stat="identity", position=position_dodge(), width = 0.7) + coord_flip()
Bilan_Pj <- Bilan_Pj + scale_x_discrete(limits = Stat_Pj[which(Stat_Pj[,2]==" contribution au refc total ")[order(Stat_Pj[which(Stat_Pj[,2]==" contribution au refc total "),3])], 1])
Bilan_Pj <- Bilan_Pj + scale_fill_manual(values=c("#842658","#DD308D","#E894C1"),  guide = guide_legend(reverse = TRUE))
Bilan_Pj <- Bilan_Pj + theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(size=12), axis.title = element_text(size = 12), axis.text = element_text(size=12))
# #842658=contribution au refc total, #DD308D=mailles impactees,#E894C1=presence sur la zone
# 3eme etape: Export du graphe
namefile <- "Bilan_pressions.jpeg"
if (!file.exists(paste(graphdir, namefile, sep="/"))){
  ggsave(namefile, plot = Bilan_Pj, device = "jpeg" , path = graphdir, width=34, height=18, units=c("cm"))
  print(paste("Export du fichier-----",namefile))
} else {
  print(paste("le fichier-----",namefile,"--------existe deja"))
}

print("# ")
print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 11 - EXPORT DES RESULTATS SOUS FORME DE TABLES DANS LA BASE DE DONNEES               ")
print("# ------------------------------------------------------------------------------------------")

# creation d'une fonctin qui permet d'exporter une table dans la base de donnees en lui rajoutant un champ geom et un certain nombre de metadonnees
# in.con: connexion a la base de donnees
# in.scheme: schema dans lequel on souhaite exporter la table
# in.grid: la grille qui contient l'identifiant, le champ geom et les metadonnees associees a chaque maille
# table contenant les donnees a exporter. Elle doit contenir une colonne "mesh" avec les memes identifiants que dans la grille
# Nom que l'on souhaite donner a la table qui sera creee dans la base de donnees
export_table <- function(in.con, in.scheme, in.grid, in.table, in.nametab){
  print(paste("Export de la table-----", in.nametab, "-------dans le schema------",in.scheme, sep=""))
  index_export <- in.grid
  name_id <- colnames(head(index_export))[i_idz]
  New_Attribut <- merge(index_export@data,in.table,by.x=name_id,by.y="mesh",all.x=T,sort=FALSE)
  index_export@data <- New_Attribut 
  dbWriteSpatial(in.con, index_export, schemaname=in.scheme, tablename=in.nametab, replace=T)
  request <- paste("select UpdateGeometrySRID('",in.scheme,"', '",in.nametab,"', 'geom', 4326)", sep="")
  dbExecute(in.con, request)
}

if(config$SauvRep$export_db_tables|config$SauvRep$export_index_sql|config$SauvRep$export_ic){
  #Connexion avec la base de donnees
  if(config$SauvRep$db_choice=="local"){
    con <- dbConnect(drv=config$DBConnectlocal$DBConnect_drv,
                     host=config$DBConnectlocal$DBConnect_host,
                     user=config$DBConnectlocal$DBConnect_user,
                     password=config$DBConnectlocal$DBConnect_password,
                     dbname=config$DBConnectlocal$DBConnect_dbname,
                     port=config$DBConnectlocal$DBConnect_port)
  } else {
    con <- dbConnect(drv=config$DBConnectdist1$DBConnect_drv,
                     host=config$DBConnectdist1$DBConnect_host,
                     user=config$DBConnectdist1$DBConnect_user,
                     password=config$DBConnectdist1$DBConnect_password,
                     dbname=config$DBConnectdist1$DBConnect_dbname,
                     port=config$DBConnectdist1$DBConnect_port)
  }
  
  
  #Export de la table contenant les principaux indices
  if(config$SauvRep$export_index_sql){
    data_index2 <- data_index
    # Conversion en donnees numeriques de plusieurs colonnes qui etaient utilisees comme facteurs pour les graphiques
    data_index2[,3] <- as.numeric(as.character(data_index[,3]))
    data_index2[,4] <- as.numeric(as.character(data_index[,4]))
    data_index2[,5] <- as.numeric(as.character(data_index[,5]))
    export_table(con, config$SauvRep$export_scheme, grid_z, data_index2, "index")
  }
  #Export de la table contenant les pressions normalisees
  if(config$SauvRep$export_db_tables){
    export_table(con, config$SauvRep$export_scheme, grid_z, A_zi, "activites")
    export_table(con, config$SauvRep$export_scheme, grid_z, Mat_Pj_lognorm_z, "pressions")
    export_table(con, config$SauvRep$export_scheme, grid_z, Mat_Sensib_Pj, "sensibilite")
    export_table(con, config$SauvRep$export_scheme, grid_z, Mat_REF_PjE1_z, "effet")
  }
}


print("# ")
print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 12: CALCUL D'UN INDICE DE CONFIANCE "                                                      )
print("# ------------------------------------------------------------------------------------------")


if(config$SauvRep$shape_ic) {
  ## ---------------------------------------------------------------------------------------------
  ## PREMIERE ETAPE: EXTRACTION DES INDICES DE QUALITE DES DONNEES ACTIVITES
  ## ---------------------------------------------------------------------------------------------
  
  if(config$SauvRep$lrepr_iq==0){
    # Code pour extraire les indices de confiances
    # ouverture des tables et remplissage de la matrice A_iz8IQ
    for(i in 1:ni_ag){
      print("#")
      print(paste("# Extraction indices de qualite pour l'activite : ",Codes_i_ag[i] ))
      # numero de ligne de la table 'data' correspondant a l'activite
      ilin <- config$A_ilindata[which(config$Aiag==Codes_i_ag[i])[1]]
      # nom de la table
      Ai_nametab <- paste("A_",config$DBData$Data[ilin,i_Table_code],sep="")
      
      # nom du champ
      Ai_field <- paste(config$DBData$Data[ilin,i_Field], "_iq", sep = "")
      # recuperation de la table
      Ai_table   <- eval(parse(text=paste("list_sauv$",Ai_nametab,sep="",collapse=" ; ")))
      
      print(paste("# -> cette activite a un jeu de donnees : ",paste("list_sauv$",Ai_nametab,sep="",collapse=" ; ")))
      print(paste("# ->                             champ  : ",Ai_field))
      
      # - subset de la couche SIG
      Ai_z <- subset(Ai_table, eval(parse(text=paste("Ai_table$id2 %in% grid_z@data@.Data[[",i_idz,"]]",sep="",collapse=" ; "))))
      
      # enregistrement des indices des mailles de la table dans colonne "mesh"
      if(i==1){
        A_zi_iq    <-  col_idz
      }
      # indice du champ de la table 'Data' rassemblant les donnees activites
      i_idAi    <- which(colnames(Ai_z@data)==Ai_field)
      
      # enregistrement dans la matrice A_zi des donnees activites
      
      # -- comme il peut y avoir plusieurs lignes pour une meme maille : moyenne des valeurs
      # -- cette fonction ordonne aussi les valeurs d'activites selon l'ordre des mailles de vec_zid
      vec <-  matrix(data=5, nrow=nz, ncol=1)
      for (j in 1:nz){
        if (vec_zid[j] %in% Ai_z@data@.Data[[which(colnames(Ai_z@data)=="id2")]]){
          vec2 <- subset(Ai_z@data,Ai_z@data@.Data[[which(colnames(Ai_z@data)=="id2")]]==vec_zid[j]) 
          vec[j] <- mean(as.numeric(unlist(vec2[i_idAi])),na.rm=T)
        } 
      }
      
      nameVARz <- paste(config$Aiag[which(config$Aiag==Codes_i_ag[i])[1]], "_iq", sep = "")
      VARz <- cbind.data.frame(vec_zid,as.matrix(vec)) ; colnames(VARz) <- c("mesh",nameVARz)
      
      # -- remplissage de la matrice intensite des activites A_zi
      A_zi_iq <- cbind.data.frame(A_zi_iq,VARz[,2])
      
      
    } # fin boucle sur les activites Ai_ag
    vec <-  c("mesh",paste(Codes_i_ag, "iq", sep="_"))
    colnames(A_zi_iq) <- vec
    
    if(config$SauvRep$lsauv_iq==1){
      filesauv_iq <- paste(sauvrepdir,"/IQ_",chainchar,"_",datesauv,".Rdata",sep="")
      save(A_zi_iq, file = filesauv_iq)
    }
  } else {  
    filerepr_iq <- paste(sauvrepdir,"/IQ_",chainchar,"_",config$SauvRep$daterepr_iq,".Rdata",sep="")
    load(filerepr_iq)
  }
  
  
  
  
  ## ---------------------------------------------------------------------------------------------
  ## DEUXIEME ETAPE: GESTION DE LA MATRICE ACTIVITE PRESSION ET DES INDICES DE CONFIANCES ASSOCIES 
  ## ---------------------------------------------------------------------------------------------
  
  # -- creation d'un dataframe Matrice AP pour faciliter les calculs
  MatAP <- config$Mat_AP[vec_lines,ideb_P:ifin_P]
  colnames(MatAP) <- list_Pcode_MatAP ; rownames(MatAP) <- list_Acode_MatAP 
  ni_MatAP <- dim(MatAP) [1] ; nj_MatAP <- dim(MatAP) [2] # Nombre de colonnes et de lignes de la matrice
  for(i in 1:ni_MatAP){
    for(j in 1:nj_MatAP){
      if(MatAP[i,j]=="non" & !is.na(MatAP[i,j])) {
        MatAP[i,j] <- 0 #On remplace les tirets par des 0 (pour faciliter la lecture de la matrice)
      }
      if(MatAP[i,j]=="oui" & !is.na(MatAP[i,j])) {
        MatAP[i,j] <- 1 #De meme on remplace les x par des 1
      } 
      if(is.na(MatAP[i,j]) | (MatAP[i,j] != 0 & MatAP[i,j] != 1)) {
        MatAP[i,j] <- NA # On met des NA au lieu de ND (que R ne sait pas interpreter)
      }
    }
  }
  MatAP <- as.data.frame(MatAP)
  for(i in 1:dim(MatAP)[2]){
    MatAP[,i] <- as.numeric(as.character(MatAP[,i]))
  }
  
  # Extraction des indices de confiance de la matrice A/P
  vec_lines_IC <- which(config$Mat_AP[,1]=="IC") #numeros de ligne du feuiillet matAP contenant les indices de confiance
  MatAP_IC <- config$Mat_AP[vec_lines_IC,ideb_P:ifin_P]
  colnames(MatAP_IC) <- list_Pcode_MatAP ; rownames(MatAP_IC) <- list_Acode_MatAP # On utilise les codes pressions et activites comme noms de ligne et de colonne
  MatAP_IC[MatAP_IC=="ND"]<-0 # Lorsque l'indice de confiance est inconnu, on lui donne la valeur de 0
  MatAP_IC[MatAP=="ND"]<-0 # Lorsque le lien AP est inconnu, on donne a l'IC la valeur de 0
  MatAP_IC <- as.data.frame(MatAP_IC) #conversion en data.frame
  for(i in 1:dim(MatAP_IC)[2]){ #Boucle sur les pressions pour avoir des valeurs en format numerique
    MatAP_IC[,i] <- as.numeric(as.character(MatAP_IC[,i]))
  }
  
  
  # Double boucle permettant de reduire les matrices creees ci-dessus aux pressions et activites utilisees dans l'analyse
  for(i in 2:dim(A_zi)[2]){ # Boucle sur les activites
    iA_code <- colnames(A_zi)[i] #extraction du code de l'activite
    list_iA_ag <- config$A_code[which(config$Aiag==iA_code)]  # Code pour les activites agreges
    n_act <- length(list_iA_ag) # Nombre d'activites agregees (si pas d'agregation, =1)
    sub <- MatAP[which(rownames(MatAP) %in% list_iA_ag),] # Extraction dans la matrice AP des lignes reliees aux activites (plusieurs si agregation)
    subIC <- MatAP_IC[which(rownames(MatAP_IC) %in% list_iA_ag),] # Idem avec la matrice des indices de confiance
    sub_final <- as.data.frame(matrix(data=0, nrow=1, ncol=length(vec_P_code_eval), dimnames = list(iA_code, vec_P_code_eval))) # Vecteur qui contiendra la ligne finale de la matrice reduite
    subIC_final <- as.data.frame(matrix(data=0, nrow=1, ncol=length(vec_P_code_eval), dimnames = list(iA_code, vec_P_code_eval))) # Idem pour la matrice des indices de confiance
    for(j in 1:length(vec_P_code_eval)){ # Boucle sur les pressions prises en compte dans l'evaluation
      iP_code <- vec_P_code_eval[j] # Code correspondant a j
      if(iP_code %in% config$P_code) { # Si il ne s'agit pas d'une pression agregee
        list_iP_ag <- P_code[which(P_code==iP_code)] # On ne conserve que le code d'origine
      } else { # Sinon: cela signifie que l'on a affaire a une pression agregee
        list_iP_ag <- config$P_code[which(config$P_code_ag==iP_code)] # Alors on enregistre tous les codes des pressions agregees
      }
      if(length(which(is.na(unlist(sub[,which(colnames(sub) %in% list_iP_ag)]))))==0){ # Si le lien AP est connu dans au moins un des couples AP (plusieurs couples si a ou p agregees)
        sub_final[which(colnames(sub_final)==iP_code)] <- max(unlist(sub[,which(colnames(sub) %in% list_iP_ag)]), na.rm = T) # Les liens AP sont de 1 ou de 0. Ainsi, si au moins des liens est positif, on considere que le lien existe
      } else {
        sub_final[which(colnames(sub_final)==iP_code)] <- NA # Si tous les liens sont inconnus, on attribue la valeur de NA
      }     # Dans le cas de pressions ou d'activites agregees, on fait une moyenne des indices de confiance 
      subIC_final[which(colnames(subIC_final)==iP_code)] <- mean(unlist(subIC[,which(colnames(subIC) %in% list_iP_ag)]))
    }
    if(i==2){
      MatAP_IC_sub <- subIC_final
      MatAP_sub <- sub_final
    } else {
      MatAP_sub <- rbind.data.frame(MatAP_sub, sub_final)
      MatAP_IC_sub <- rbind.data.frame(MatAP_IC_sub, subIC_final)
    }
    rownames(MatAP_IC_sub)[i-1] <- iA_code
  }
  
  ## ---------------------------------------------------------------------------------------------
  ## TROISIEME ETAPE: CALCUL D'UN INDICE DE CONFIANCE ASSOCIE A CHAQUE PRESSION 
  ## ---------------------------------------------------------------------------------------------
  
  ##                                  ------                                        ##
  ## --- Premiere sous etape: indice de confiance dans les relations theoriques --- ##
  
  
  # creation et remplissage de la matrice contenant les IC de l'intensite de chaque pression dans chaque maille. 
  ICAP_Pj <- cbind.data.frame(as.matrix(vec_zid), matrix(data=0, nrow=nz, ncol=dim(Mat_Pj_z)[2]-1))
  colnames(ICAP_Pj) <- colnames(Mat_Pj_z)
  print("Remplissage de la matrice contenant les IC de l'intensite de chaque pression dans chaque maille")
  for(i in 1:nz){ # Boucle sur toutes les mailles
    coef <- as.numeric(A_zi_lognorm[i,2:dim(A_zi_lognorm)[2]]) # On extrait l'intensite des differentes activite pour chaque maille
    for(j in 2:dim(ICAP_Pj)[2]){ # Boucle sur les pressions
      val <- MatAP_IC_sub[,which(colnames(MatAP_IC_sub)==colnames(ICAP_Pj)[j])] #Vecteur contenant les IC associe au lien entre les differentes activites et la pression j
      if(sum(coef, na.rm=TRUE)==0){ # Si toutes les activites sont nulles
        ICAP_Pj[i,j] <- 1 # Alors l'indice de confiance prend la valeur maximale
      } else {
        # Sinon fait une moyenne ponderee par l'intensite normalisee de l'activite
        # On redivise aussi par 5 pour ramener l'IC entre 0 et 1
        ICAP_Pj[i,j] <- sum(val*coef, na.rm=TRUE)/(5*sum(coef, na.rm=TRUE)) 
      }
    }
  }
  
  ##                                       ------                                           ##
  ## --- Deuxieme sous etape: indice de confiance dans la qualite des donnees activites --- ##
  
  
  ## Calcul d'un indice de confiance dans la qualite des donnees. 
  # creation de la matrice qui contiendra l'indice
  ICQAP_Pj <- cbind.data.frame(as.matrix(vec_zid), matrix(data=1, nrow=nz, ncol=dim(Mat_Pj_z)[2]-1))
  colnames(ICQAP_Pj) <- colnames(Mat_Pj_z)
  ICQAP_Pj[,1] <- as.character(ICQAP_Pj[,1])
  for(i in 2:dim(ICQAP_Pj)[2]){ #Boucle sur les pressions pour convertir au format numerique
    ICQAP_Pj[,i] <- as.numeric(as.character(ICQAP_Pj[,i]))
  }
  # Boucle sur toutes les mailles pour remplir la matrice
  for (i in 1:nz){
    for(j in 2:dim(ICQAP_Pj)[2]){ # Boucle sur les pressions
      if(sum(MatAP_sub[,j-1], na.rm=T) > 0){ # Si la pression est liee a au moins une activite --> moyenne ponderee des IQ
        ICQAP_Pj[i, j] <- sum(MatAP_sub[,j-1]*A_zi_iq[i,2:dim(A_zi_iq)[2]], na.rm = T)/(5*sum(MatAP_sub[,j-1], na.rm=T))
      }
    }
  }
  
  
  ##                                             ------                                              ##
  ## --- Troisieme sous etape: synthese --> calcul de l'indice de confiance dans chaque pression --- ##
  
  
  IC_Pj <- cbind.data.frame(as.matrix(vec_zid), matrix(data=1, nrow=nz, ncol=dim(Mat_Pj_z)[2]-1)) # creation de la matrice qui contiendra l'indice
  colnames(IC_Pj) <- colnames(Mat_Pj_z)
  IC_Pj[,1] <- as.character(IC_Pj[,1])
  for(i in 2:dim(IC_Pj)[2]){ #Boucle sur les pressions pour convertir au format numerique et calculer l'indice
    IC_Pj[,i] <- as.numeric(as.character(IC_Pj[,i]))
    IC_Pj[,i] <- ICAP_Pj[,i]*ICQAP_Pj[,i] # Dans tous les cas de figure, simple produit des deux indices precedemment calcules
  }
  
  
  ## ---------------------------------------------------------------------------------------------
  ## QUATRIEME ETAPE: CALCUL D'UN INDICE DE CONFIANCE DANS LA SENSIBILITE A CHAQUE PRESSION
  ## ---------------------------------------------------------------------------------------------
  
  ##                                                ------                                                 ##
  ## --- Premiere sous etape: indice de confiance dans les relations theoriques habitats - sensibilite --- ##
  
  # creation de la matrice contenant l'information 
  ICRTHP_Pj <- cbind.data.frame(as.matrix(vec_zid), matrix(data=0, nrow=nz, ncol=dim(Mat_Pj_z)[2]-1))
  colnames(ICRTHP_Pj) <- colnames(Mat_Pj_z)
  ICRTHP_Pj[,1] <- as.character(ICRTHP_Pj[,1])
  for(i in 2:dim(ICRTHP_Pj)[2]){ #Boucle sur les pressions pour convertir au format numerique
    ICRTHP_Pj[,i] <- as.numeric(as.character(ICRTHP_Pj[,i]))
  }
  id_mesh <- which(colnames(HabBenth_z@data)=="id2") #numero de colonne des identifiants dans la table habitats
  col_IC <- paste(tolower(colnames(Mat_Pj_z)[2:dim(Mat_Pj_z)[2]]),"icas",sep="_") # vecteur contenant les noms de colonne des indices de confiance
  col_sensib <- paste(tolower(colnames(Mat_Pj_z)[2:dim(Mat_Pj_z)[2]]),suf_sensib,sep="") # vecteur contenant les noms de colonne des scores de sensibilites
  print("calcul d'un indice de confiance pour les relations theoriques habitats - pression")
  for(i in 1:nz){ # Boucle sur toutes les mailles
    n_hab <- length(which(HabBenth_z@data[,id_mesh]==ICRTHP_Pj[i,1])) # Nombre d'habitats dans la maille
    if(n_hab>0){ # Si il y a au moins un habitat dans la maille
      for(j in 2:dim(ICRTHP_Pj)[2]){ # Boucle sur les pressions, on commence par extraire pur tous les habitats la surface des heb, la surface maritime, la sensibilite et l'indice de confiance
        sub <- HabBenth_z@data[which(HabBenth_z@data[,id_mesh]==ICRTHP_Pj[i,1]),names(HabBenth_z@data)%in%c("surfmer","surfhab_cel",col_IC[j-1],col_sensib[j-1])]
        sub[which(sub[,3]==99),4] <- 0 ##Si la sensibilite de l'habitat est inconnu, alors l'IC pour ce couple habitat pression  est nul
        # Au lieu de diviser la somme ponderee par la somme des habitats, on divise par la surface maritime (attribue un zero sur les zones marines ou on ne connait pas l'habitat)
        # Division par 5 pour ramener l'indice entre 0 et 1
        ICRTHP_Pj[i,j] <- sum(sub[,2]*sub[,4])/(5*sub[1,1])
      }
    }
  }
  
  
  ##                                       ------                                          ##
  ## --- Deuxieme sous etape: indice de confiance dans la qualite des donnees habitats --- ##
  
  # creation de la matrice contenant l'information 
  ICQHP_Pj <- cbind.data.frame(as.matrix(vec_zid), matrix(data=0, nrow=nz, ncol=1))
  colnames(ICQHP_Pj) <- c("mesh", "ICQHP")
  ICQHP_Pj[,1] <- as.character(ICQHP_Pj[,1])
  ICQHP_Pj[,2] <- as.numeric(ICQHP_Pj[,2]) #Boucle sur les pressions pour convertir au format numerique
  print("calcul d'un indice de confiance pour la qualite des donnees habitats")
  for(i in 1:nz){ # Boucle sur toutes les mailles
    n_hab <- length(which(HabBenth_z@data[,id_mesh]==ICQHP_Pj[i,1])) # Nombre d'habitats dans la maille
    if(n_hab>0){ # Si il y a au moins un habitat dans la maille
      sub <- HabBenth_z@data[which(HabBenth_z@data[,id_mesh]==ICQHP_Pj[i,1]),which(colnames(HabBenth_z@data) == "hab_iq")] #on extrait les indices de qualite des habitats
      ICQHP_Pj[i,2] <- sub[1]
    }
  }
  
  
  
  ##                                             ------                                              ##
  ## --- Troisieme sous etape: synthese --> calcul de l'indice de confiance dans chaque pression --- ##
  
  ICHP_Pj <- cbind.data.frame(as.matrix(vec_zid), matrix(data=1, nrow=nz, ncol=dim(Mat_Pj_z)[2]-1)) # creation de la matrice qui contiendra l'indice
  colnames(ICHP_Pj) <- colnames(Mat_Pj_z)
  ICHP_Pj[,1] <- as.character(ICHP_Pj[,1])
  for(i in 2:dim(ICHP_Pj)[2]){ #Boucle sur les pressions pour convertir au format numerique et calculer l'indice
    ICHP_Pj[,i] <- as.numeric(as.character(ICHP_Pj[,i]))
    ICHP_Pj[,i] <- (ICRTHP_Pj[,i]*ICQHP_Pj[,2])/5 # Dans tous les cas de figure, simple produit des deux indices precedemment calcules
  }
  
  ## ---------------------------------------------------------------------------------------------
  ## CINQUIEME ETAPE: CALCUL D'UN INDICE DE CONFIANCE DANS LE RISQUE D'EFFET ASSOCIE A CHAQUE PRESSION
  ## ---------------------------------------------------------------------------------------------
  
  
  # creation de la matrice contenant l'information 
  ICREFC_Pj <- cbind.data.frame(as.matrix(vec_zid), matrix(data=NA, nrow=nz, ncol=dim(Mat_Pj_z)[2]-1))
  colnames(ICREFC_Pj) <- colnames(Mat_Pj_z)
  ICREFC_Pj[,1] <- as.character(ICREFC_Pj[,1])
  for(j in 2:dim(ICREFC_Pj)[2]){ #Boucle sur les pressions 
    ICREFC_Pj[,j] <- as.numeric(as.character(ICREFC_Pj[,j])) # Conversion au format numerique
    
    # Si la sensibilite est nulle et que la pression est non nulle, l'indice prend la valeur de l'ICHP_Pj
    if(length(which(Mat_Sensib_Pj[,j]==0 & Mat_Pj_z[,j] > 0)) > 0){
      ICREFC_Pj[which(Mat_Sensib_Pj[,j]==0 & Mat_Pj_z[,j] > 0),j] <- ICHP_Pj[which(Mat_Sensib_Pj[,j]==0 & Mat_Pj_z[,j] > 0),j]
    }
    
    # Si la sensibilite est non nulle et que la pression est nulle, l'indice prend la valeur de l'IC_Pj
    if (length(which(Mat_Sensib_Pj[,j] > 0 & Mat_Pj_z[,j] == 0)) > 0) {
      ICREFC_Pj[which(Mat_Sensib_Pj[,j] > 0 & Mat_Pj_z[,j] == 0),j] <- IC_Pj[which(Mat_Sensib_Pj[,j] > 0 & Mat_Pj_z[,j] == 0),j]
    }
    
    # Si la sensibilite est non nulle et que la pression est non nulle, on multiplie les deux indices
    if (length(which(Mat_Sensib_Pj[,j] > 0 & Mat_Pj_z[,j] > 0)) > 0) {
      ICREFC_Pj[which(Mat_Sensib_Pj[,j] > 0 & Mat_Pj_z[,j] > 0),j] <- IC_Pj[which(Mat_Sensib_Pj[,j] > 0 & Mat_Pj_z[,j] > 0),j] * ICHP_Pj[which(Mat_Sensib_Pj[,j] > 0 & Mat_Pj_z[,j] > 0),j]
    }
    
    # Idem si les deux sont nuls
    if(length(which(Mat_Sensib_Pj[,j] == 0 & Mat_Pj_z[,j] == 0)) > 0) {
      ICREFC_Pj[which(Mat_Sensib_Pj[,j] == 0 & Mat_Pj_z[,j] == 0),j] <- IC_Pj[which(Mat_Sensib_Pj[,j] == 0 & Mat_Pj_z[,j] == 0),j] * ICHP_Pj[which(Mat_Sensib_Pj[,j] == 0 & Mat_Pj_z[,j] == 0),j]
    }
    
  }
  
  
  
  
  #4eme etape: calcul de l'IC global
  print("Calcul de l'indice de confiance global")
  IC <- as.matrix(c(1:nz)) # On initialise le vecteur qui contiendra le resultat
  for(i in 1:nz){ # Boucle sur toutes les mailles
    if(length(which(!is.na(Mat_REF_PjE1_z[i, 2:dim(Mat_REF_PjE1_z)[2]]))) > 0){ # Si au moins l'un des REFC_Pj est connu (et donc le REFC final n'est pas NA)
      sub <- which(!is.na(Mat_REF_PjE1_z[i, 2:dim(Mat_REF_PjE1_z)[2]])) + 1 # On extrait les numeros de colonnes associes aux REFC non-NA
      if(sum(Mat_REF_PjE1_z[i, sub], na.rm=T) == 0) IC[i] <- mean(as.numeric(ICREFC_Pj[i, sub]), na.rm=T) # Si tous les REFC sont nuls, on fait une moyenne non-ponderee
      else IC[i] <- sum(ICREFC_Pj[i, sub]*Mat_REF_PjE1_z[i, sub], na.rm=T)/sum(Mat_REF_PjE1_z[i, sub], na.rm=T) # Si l'un des REFC_Pj est non nul, on fait une moyenne des ICREFC_Pj ponderes par les REFC_Pj
    } else {IC[i] <- NA}
  }
  IC <- cbind.data.frame(vec_zid, IC) # On integre le resultat dans un data frame
  colnames(IC) <- c("mesh","IC")
  
  # Cartographie de l'indice de confiance
  VARz <-  IC
  nameVARz <- "IC"
  title <- "Indice de confiance"
  checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)
  
  # export dans la base de donnees
  if(config$SauvRep$export_ic)  export_table(con, config$SauvRep$export_scheme, grid_z, IC, "confiance")
  
}



## Fonction qui permettra d'afficher le temps de calcul
printime <- function(time){
  if(time<60) print(paste("Temps de calcul : ", floor(time), "s", sep=""))
  if(time > 60 && time <3600){
    min <- floor(time/60)
    sec <- time - min*60
    print(paste("Temps de calcul : ", min, "min ", floor(sec), "s", sep=""))
  }
  if(time>3600){
    hrs <- floor(time/3600)
    min <- floor((time-3600*hrs)/60)
    sec <- time - 3600*hrs - 60*min
    print(paste("Temps de calcul : ", hrs, "h ", min, "min ", floor(sec), "s", sep=""))
  }
}

printime(as.numeric((proc.time()-ptm)[3])) #Affichage du temps de calcul

stop("-------- That's all Folks ---------")
