# ANALYSE DU RISQUE D'EFFETS CUMULES DES ACTIVITES ANTHROPIQUES SUR LES HABITATS BENTHIQUES
# ETAPE 2, ANALYSE PAR SIMULATION DE MONTE-CARLO

# Projet        : code développé dans le cadre du projet CARPEDIEM (2016-2018)
# Date          : 2022/10/06
# Version code  : 1.0
# Version R     : R-3.5.1
# Auteurs       : Alice Vanhoutte-Brunier, Julien barrere
# Contributeur  : Frederic Quemmerais-Amice (frederic.quemmerais-amice@ofb.gouv.fr)

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
#                 Ils ne peuvent en cas être tenu responsable des interprétations et des conclusions qui pourraient être faites suite à l'utilisation de ce programme.
#

# Deroulement generale de l'analyse (voir le guide d'utilisation) :
#     - Il est imperatif d'avoir réalisé l'analyse étape 1 (code R "analyse_etape1_refc_publication_v1.R") au préalable.
#     - Les parametrages de l'analyses sont renseignés dans un fichier Excel
#       contenu dans le dossier "PARAM" du dossier "CODES_CARPEDIEM". Reprendre le fichier de paramétrage utilisé pour l'étape 1 et
#       renseigner l'onglet "simulation_monte_carlo".
#     - dans le code, seuls les parametres "fileparam" et "userdir" correspondant respectivement
#       au nom du fichier .xlsx de parametrage et au chemin d'acces du dossier "CODES_CARPEDIEM" sont
#       a renseigner aux lignes x et y.
#     - Lecture et import des tables de donnees sources contenues dans une base de donnees PostgreSQL (locale et/ou distante).
#     - Export des resultats dans la base de donnees, dans de nouvelles tables, dans un schema a specifier.

# -----------------------------------------------------------------------------
#              ETAPE 1 - IMPORT DES DONNEES ACTIVITES ET HABITATS
#              ETAPE 2 - CODAGE DES FONCTIONS ASSOCIEES AUX FACTEURS
#              ETAPE 3 - REALISATION DES SIMULATIONS
# -----------------------------------------------------------------------------

rm(list=ls())
ptm <- proc.time()

print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 1 - IMPORT DES DONNEES ACTIVITES ET HABITATS                            ")
print("# ------------------------------------------------------------------------------------------")


# PARAMETRES DE CONFIGURATION POUVANT ?TRE MODIFIES PAR TOUT UTILISATEUR ------
# reprises et sauvegardes
# parametrage
fileparam         <- "fichier_parametrage_refc_2022_source_fonctionnel.xlsx"    # fichier de param?trage Excel
# utilisateur
userdir           <- "D:/02_SIG/00_CODE_CARPEDIEM_BENTHOS_OPERATIONNEL_2022/"
# ------------------------------------------------------------------------------


print("# ------------------------------------------------------------------------------------------")
print("# Parametrage :                                                                             ")
print(paste("# Fichier de parametrage :   ",fileparam,sep=""))
print(paste("# Repertoire utilisateur :   ",userdir,sep=""))
print("# ------------------------------------------------------------------------------------------")
print(" ")


# Gestion de l'arborescence - ne fonctionne que si l'utilisateur a cr?e la m?me arborescence dans le userdir
moddir     <- paste(userdir,"CODES_CARPEDIEM",sep="")     
indir      <- paste(moddir,"/INPUTS",sep="") 
outdir     <- paste(moddir,"/OUTPUTS",sep="") 
paramdir   <- paste(moddir,"/PARAM",sep="")
sauvrepdir <- paste(moddir,"/SAUVREP",sep="")
sigdir     <- paste(indir,"/SIG",sep="")

setwd(moddir)

# Chargement des librairies
#-- packages r?install?s sur R 3.4.1
library(rpostgis)
library(sp)
library(RPostgreSQL)  # pour lire DB et transf?r?er data.frames vers DB PostgreSQL
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
library(ggplot2)      # pas utilis?e pour l'instant
library(ggmap)        # fonds de cartes, en association avec gglopt2
library(ncdf4)
library(psy)          # statistiques ACP
require(RPostgreSQL)


#-- pas encore r?install?s sous R.3.4.1
library(grDevices)


print("# ")
source('f_import.R')                # code source pour importer les param?tres / tables de DB
source('f_lec_Postgres_DB.R')       # code source de fonctions de lecture de BD Postgre  
source('./Rpostgis/Rpostgis.R')     # code source Nicolas Lambert - modifi? par AVB
source('f_map_plot.R')              # code source fonctions de plot des cartes
source('f_spatial_data_analysis.R') # Fonctions ouverture couches SIG, ...

print("# LECTURE DES PARAMETRES DU FICHIER EXCEL ET ECRITURE DANS LA LISTE config        ")
file_path <- paste(paramdir,"/",fileparam,sep="")         # path et nom du fichier Excel qui contient les param?tres
config    <- f_importConfig(file_path)                    # lecture du fichier Excel de param?trage



# Unit?s et conversions 
# - surface exprim?es en km?
conv_m2tokm2 <- 1e-06

# Pr?paration sauvegarde des r?sultats
# - Cr?ation d'une fonction pour convertir le binaire entr? dans le fichier de param?trage en terme de SRM ?tudi?es
read_srm <- function(x){
  out <- c()
  if(as.numeric(substr(x, 1, 1))==1) out <- c(out, "MED")
  if(as.numeric(substr(x, 2, 2))==1) out <- c(out, "GDG")
  if(as.numeric(substr(x, 3, 3))==1) out <- c(out, "CEL")
  if(as.numeric(substr(x, 4, 4))==1) out <- c(out, "MMN")
  return(out)
}
# - suffixe domaine ?valu?
lcond_SRM <- read_srm(config$TimeSpace$config_SRM)

# - suffixe domaine evalu?
nchainchar <- length(lcond_SRM)
for(i in 1:nchainchar){
  if(i==1) {chainchar <- lcond_SRM[i]} 
  else {chainchar <- paste(chainchar,lcond_SRM[i], sep="_")}}


# - Seuils pour les statistiques des simulations
thres_stat <- c() #On initialise un vecteur vide
for(i in 1:3){ #On peut renseigner dans le fichier de param?trage jusqu'? 3 seuils diff?rents
  eval(parse(text=paste("val <- config$simul$thres", i,sep="",collapse=" ; "))) # On va chercher les valeurs configur?es
  if(val > 0) thres_stat <- c(thres_stat, val) # 0 indique qu'on prend en compte un seuil de moins: on ne garde donc que les valeurs superieures ? 0
}

print("# IMPORT DES TABLES DE LA BASE DE DONNEES                                       ")
# Gestion des sauvegardes / reprises
datesauv <- paste(substr(date(),21,24),substr(date(),5,7),substr(date(),9,10),sep="") #suffixe du fichier de sauvegarde
lrepr_DBtables <- FALSE
if(config$SauvRep$lrepr_DBtables==1)lrepr_DBtables <- TRUE
lsauv_DBtables <- FALSE
if(config$SauvRep$lsauv_DBtables==1)lsauv_DBtables <- TRUE
daterepr <- config$SauvRep$daterepr

# Lecture ou reprise des tables de la DB
if(!lrepr_DBtables) {   # Import des tables de la DB si elles n'ont pas ?t? sauvegard?es dans un fichier d'initialisation
  # -- les tables import?es sont sauvegard?es dans la liste list_sauv, creation de cette liste
  list_sauv <- list()
  print("IMPORT DES TABLES DE LA DB :")
  
  # nombre de jeux de donn?es ? importer : ntables
  # - calcul du nombre de tables ? important en faisant une recherche du nombre de valeurs uniques dans le champ "DB_Table"
  # -- identification du num?ro de la colonne contenant les noms des tables
  icolDB_table <- which(colnames(config$DBData$Data)=="DB_table")
  # -- liste des noms des tables
  list_sauv$vtab_names <- c()
  # -- liste des tables ? importer
  vtables <- unique(config$DBData$Data[,icolDB_table])
  # -- nombre de tables ? importer
  ntables <- length(vtables)
  
  # import de toutes les tables d?crites dans le feuillet Excel 'Data'
  for(i in 1:ntables){
    # rep?rage de la premi?re ligne correspondant ? la table i
    irow <- which(config$DBData$Data[,icolDB_table]==vtables[i])[1]
    # r?cup?ration du nom de code de la table, lu dans colonne "Data_code"
    icolTable_code <- which(colnames(config$DBData$Data)=="Table_code")
    iTable_code <- config$DBData$Data[irow,icolTable_code]
    print(iTable_code)
    # -- nom de la variable recevant toutes les donn?es de la table
    icolData_type <- which(colnames(config$DBData$Data)=="Data_type")
    tab_name <- paste(config$DBData$Data[irow,icolData_type],"_",config$DBData$Data[irow,icolTable_code],sep="")
    list_sauv$vtab_names <- c(list_sauv$vtab_names,tab_name)
    
    # - l'import est fonction du type de fichier : Table d'une BD spatiale ou fichier SIG
    if(config$DBData$Data[irow,which(colnames(config$DBData$Data)=="Source")]=="PgAdmin"){
      
      # -- r?cup?ration des arguments de lecture de la table par la fonction f_argDB cr??e dans lec_postgresDB.R
      args <- f_argDB(iTable_code,config$DBData$Data,config)
      # import de la table par la fonction f_importTableFromDBPostgresSQL cr??e dans import.R
      temp <- f_importTableFromDBPostgresSQL(args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8])
      
    } else {  # fichier SIG
      filename <- paste(sigdir,"/ACTIVITES/",config$DBData$Data[irow,which(colnames(config$DBData$Data)=="DB_table")],sep="")
      temp <- readShapePoly(filename,proj4string = CRS("+proj=longlat +datum=WGS84"))
    }
    
    # -- ?criture de la table/couche SIG dans une variable ayant le nom renseign? dans tab_name
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
    print(paste("Sauvegarde realis?e dans le fichier : ",filesauv))
  }
  
}else{                   # reprise des tables sauvegard?es
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
    print(paste('... r?cup?ration de la table ',iTable_code, '   ... OK'))
    head(temp@data)
  }
  print(paste("Reprise de toutes les tables contenues dans le fichier ",filerepr," ... ok",sep=""))
}

print("# GESTION DES INDICES ET DIMENSIONS, stockage dans des listes                     ")
# dimensions spatiales, grille
# - lecture et indi?age des mailles de la zone evaluee
grid_z  <- list()
nztot   <- dim(coordinates(G_grid))[1]
nz      <- 0 ; iz <- 0;

# - identification du champ de la table qui indique si la maille est dans la zone evaluee
#   appel ? la fonction f_idFieldDBTable_from_nameField (lec_Postgres_DB.R) 
#   qui retourne l'indice de la colonne de la table correspondant au Var_code 
#   du feuillet 'Data' du fichier Excel de parametrage 
i_SRM    <- f_idFieldDBTable_from_nameField("srm",config$DBData$Data,G_grid@data,"grid")
i_surfmer<- f_idFieldDBTable_from_nameField("surfmer",config$DBData$Data,G_grid@data,"grid")
i_idz    <- f_idFieldDBTable_from_nameField("idmesh",config$DBData$Data,G_grid@data,"grid")

filesauv_grid_z <- paste(sauvrepdir,"/init_grid_z_",chainchar,"_",datesauv,".Rdata",sep="")
config$filesauv_grid_z <- filesauv_grid_z

if(config$SauvRep$lrepr_grid_z==0){
  # lecture de la grille"
  print("# le fichier de configuration de la grille n existe pas, lancement de l analyse")
  varsave <- list()
  
  varsave$long_z <- array(NA,dim=(nz)) ;    varsave$lat_z <- array(NA,dim=(nz))
  varsave$longmax_z <- array(NA,dim=(nz)) ; varsave$latmax_z <- array(NA,dim=(nz))
  varsave$longmin_z <- array(NA,dim=(nz)) ; varsave$latmin_z <- array(NA,dim=(nz))
  
  # - s?lection des mailles qui correspondent a zone choisie et mailles en mer
  grid_z    <- subset(G_grid,G_grid@data@.Data[[i_SRM]] %in% lcond_SRM)
  grid_z    <- subset(grid_z,grid_z@data@.Data[[i_surfmer]] > 0.) # surface de mer sup?rieure ? 1
  
  nz        <- dim(grid_z)[1]
  
  for(z in 1:nz){
    print(paste("  - progression analyse de la grille : ",z,"/",nz))
    varsave$long_z[z]    <-  coordinates(grid_z)[z,1]  
    varsave$lat_z[z]     <-  coordinates(grid_z)[z,2]  
    varsave$longmax_z[z] <-  max(grid_z@polygons[[z]]@Polygons[[1]]@coords[,1])  
    varsave$longmin_z[z] <-  min(grid_z@polygons[[z]]@Polygons[[1]]@coords[,1])
    varsave$latmax_z[z]  <-  max(grid_z@polygons[[z]]@Polygons[[1]]@coords[,2])  
    varsave$latmin_z[z]  <-  min(grid_z@polygons[[z]]@Polygons[[1]]@coords[,2])  
  }
  
  print(paste("# >> sauvegarde de la grille realis?e dans le fichier : ",filesauv_grid_z))
  if(config$SauvRep$lsauv_grid_z==1){save(nz,varsave,grid_z,file=filesauv_grid_z)}
  
}else{
  filerepr_grid_z <- paste(sauvrepdir,"/init_grid_z_",chainchar,"_",config$SauvRep$daterepr_grid_z,".Rdata",sep="")
  if(!file.exists(filerepr_grid_z)) stop(paste("le fichier .Rdata suivant n existe pas : ",filerepr_grid_z,sep=""))
  print(paste("# << reprise de la grille sauvegard?e dans le fichier : ",filerepr_grid_z,sep=""))
  load(filerepr_grid_z)
}
nz        <- dim(grid_z)[1]


# - latitudes et longitures min et max

config$nz <- nz
config$long_z <- array(NA,dim=(nz)) ; config$lat_z <- array(NA,dim=(nz))
config$longmax_z <- array(NA,dim=(nz)) ; config$latmax_z <- array(NA,dim=(nz))
config$longmin_z <- array(NA,dim=(nz)) ; config$latmin_z <- array(NA,dim=(nz))

print(paste("# l'evaluation est r?alis?e sur : ",nz," mailles",sep=""))

config$longmin <- extent(grid_z)[1];  config$longmax <- extent(grid_z)[2]; 
config$latmin <-  extent(grid_z)[3] ; config$latmax <-  extent(grid_z)[4]
print(paste("# les limites du domaine ?valu? sont : S:",config$latmin," N:",config$latmax," W:",config$longmin," E:",config$longmax,sep=""))

config$lat_z  <- varsave$lat_z ; config$latmin_z  <- varsave$latmin_z ;  config$latmax_z  <- varsave$latmax_z 
config$long_z <- varsave$long_z ;config$longmin_z <- varsave$longmin_z ; config$longmax_z <- varsave$longmax_z

col_idz      <- eval(parse(text=paste("grid_z$",colnames(head(grid_z))[i_idz],sep="",collapse=" ; ")))
col_surfmer  <- eval(parse(text=paste("grid_z$","surfmer",sep="",collapse=" ; ")))

rm(varsave)



# Habitats benthiques et sensibilit? aux pressions : r?duction de la table aux pressions et zone ?tudi?es
# -------------------------------------------------------------------------------------------------
# - boucle sur les pressions ?tudi?es : correspondance pression et choix dans feuillet de parametrage
#   Typo_pressions, colonne "demonstrateur" renseign?e avec valeur 1
#   - identification colonne "d?monstrateur" dans fichier Excel
nj <- config$nj


# r?duction du nombre de pressions si agr?gation de plusieurs pressions
# - import de toutes les pressions et caract?ristiques (pr?sentes dans mat A/P)
P_code <- config$P_code
P_name <- config$P_name
P_modcalc <- config$P_modcalc
P_evalDEM <- config$P_evalDEM

# - pressions agreg?es
vec_P_ag <- unique(config$P_code_ag[1:nj],rm.na=TRUE)
vec_P_ag <- vec_P_ag[!is.na(vec_P_ag)]
#   - identification des pressions correspondant aux pressions agr?g?es, changement des noms
for(ivec in 1:length(vec_P_ag)){
  vec_ip <- which(config$P_code_ag[1:nj]==vec_P_ag[ivec])
  P_code[vec_ip] <- config$P_code_ag[vec_ip]
  P_name[vec_ip] <- config$P_name_ag[vec_ip]
}
#   - cbind des caract?riqtiques des pressions puis suppression des lignes redondantes
mat <- cbind(P_code,P_name,P_modcalc,P_evalDEM)
mat <- mat[! duplicated(mat[,1]), ]
P_code <- mat[,1]; P_name <- mat[,2] ; P_modcalc <- mat[,3] ; P_evalDEM <- mat[,4]
nj <- length(P_code)


# Surface des habitats benthiques par maille : somme des surfaces de chaque polygone habitat pr?sent dans la maille
# -------------------------------------------------------------------------------------------------
i_surfhab  <- f_idFieldDBTable_from_nameField("surfhab",config$DBData$Data,E_habbenth_sensib@data,"habbenth_sensib")
i_idzhab   <- f_idFieldDBTable_from_nameField("idmesh",config$DBData$Data,E_habbenth_sensib@data,"habbenth_sensib")
i_surfmesh <- f_idFieldDBTable_from_nameField("surfmesh",config$DBData$Data,E_habbenth_sensib@data,"habbenth_sensib")
i_surfmer <- which(colnames(E_habbenth_sensib@data)=="surfmer")
# - restriction de la table habitats/sensibilit? aux seules mailles de la zone etudi?e
HabBenth_z <- subset(E_habbenth_sensib, E_habbenth_sensib@data@.Data[[i_idzhab]]%in%grid_z@data@.Data[[i_idz]])

# - conversion des surfaces d'habitats m2 -> km2
HabBenth_z@data@.Data[[i_surfhab]] <- HabBenth_z@data@.Data[[i_surfhab]] * conv_m2tokm2

nb_poly    <- dim(HabBenth_z)[1]

# - creation d'une liste qui comporte des infos relatives ? la table HabBenth_z
lHabBenth_z <- list()

# - sauvegarde/reprise de la table habitat/sensibilit? 
filesauv_surfhab <- paste(sauvrepdir,"/init_lHabBenth_z_",chainchar,"_",datesauv,".Rdata",sep="")


if(config$SauvRep$lrepr_hab_sensib==0){
  # - cumul des surfaces d'habitats par maille
  print("#   identification de la ligne de la grid_z correspondant ? la maille dans laquelle est situ? le polygone")
  lHabBenth_z$surfhab_z  <- array(0.,dim=c(nz,1),dimnames=list(paste("z",1:nz,sep=""),paste("surfhab")))
  lHabBenth_z$surfmer_z <- array(NA,dim=c(nz,1),dimnames=list(paste("z",1:nz,sep=""),paste("surfmer")))
  lHabBenth_z$surfmesh_z <- array(NA,dim=c(nz,1),dimnames=list(paste("z",1:nz,sep=""),paste("surfmesh")))
  lHabBenth_z$idmesh_z   <- as.matrix(col_idz) ; dimnames(lHabBenth_z$idmesh_z) <- list(paste("z",1:nz,sep=""),paste("mesh"))
  
  for(ipoly in 1:nb_poly){
    # - num?ro de la ligne correspondant ? cet identifiant de maille
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

# - verification que la surface des habitats dans la maille n'exc?de pas la surface de la maille
config$surfmer_z <- lHabBenth_z$surfmer_z
nbdec <- 6 # nombre de d?cimales (pour surfaces exprim?es en km?)
depa        <- which(apply(as.matrix(c(1:nz)), c(1,2), function(x) (round(lHabBenth_z$surfhab_z[x],nbdec) > round(config$surfmer_z[x],nbdec))==TRUE))
if(length(depa)>0){
  print(paste("ATTENTION : ",length(depa)," mailles sur ",nz," ont surfhab > surfmer"," AVEC MARGE ",nbdec," chiffres apr?s la virgule"))
  for(idepa in 1:length(depa)){
    # - indice correspondant grid_z
    iz <- which(col_idz==lHabBenth_z$idmesh_z[depa[idepa]])
    if(config$lat_z[iz] < config$latmax && config$lat_z[iz] > config$latmin 
       && config$long_z[iz] < config$longmax && config$long_z[iz] > config$longmin )
    {
      print(paste(" SURFACE HAB > SURF MER DANS le perim?tre  maille: ",iz,lHabBenth_z$idmesh_z[depa[idepa]],
                  round(lHabBenth_z$surfhab_z[depa[idepa]],nbdec),round(config$surfmer_z[depa[idepa]],nbdec),
                  "lat:",round(config$lat_z[iz],3),"long:",round(config$long_z[iz],3)))
    }
  }
}
# - r?ajustement des surfaces d'habitats par maille pour mailles surfhab > surfmesh en attente couche habitats "clean"
print("R?ajustement des surfaces d'habitats pour mailles surfhab > surfmesh")
if(config$Calc$l_corr_surfhab){
  lHabBenth_z$surfhab_z_NEW <- lHabBenth_z$surfhab_z
  lHabBenth_z$surfhab_z_NEW[lHabBenth_z$surfhab_z_NEW > config$surfmer_z] <- 0
  
  for(ipoly in 1:nb_poly){
    id <- which(lHabBenth_z$idmesh_z==HabBenth_z@data@.Data[[i_idzhab]][[ipoly]])
    if(lHabBenth_z$surfhab_z[id] > config$surfmer_z[id]){
      print(paste("# :: surface habitat du polygone corrig?e OLD : ",ipoly,HabBenth_z@data@.Data[[i_surfhab]][[ipoly]]))
      HabBenth_z@data@.Data[[i_surfhab]][[ipoly]] <- HabBenth_z@data@.Data[[i_surfhab]][[ipoly]] * config$surfmer_z[id] / lHabBenth_z$surfhab_z[id] 
      print(paste("#                                         NEW : ",ipoly,HabBenth_z@data@.Data[[i_surfhab]][[ipoly]]))
      lHabBenth_z$surfhab_z_NEW[id] <- lHabBenth_z$surfhab_z_NEW[id]+ HabBenth_z@data@.Data[[i_surfhab]][[ipoly]]
    }
  }
  lHabBenth_z$surfhab_z <- lHabBenth_z$surfhab_z_NEW
}



# Reprise des donn?es activit?s sauvegard?es
if(config$simul$lrepr_activite==1){
  filerepr_activite <- paste(sauvrepdir,"/activite_",chainchar,"_",config$simul$daterepr_activite,".Rdata",sep="")
  load(filerepr_activite)
}


print("# CARTOGRAPHIE DES ACTIVITES")

# nombre d activites dans la matrice
ni <- config$ni

# correspondance activit?s / donn?es descriptives des activit?s / tables charg?es

# - champs de la table contenue dans le feuillet 'Data'
i_Source      <- which(colnames(config$DBData$Data)=="Source")
i_Data_code   <- which(colnames(config$DBData$Data)=="Data_code")
i_Table_code  <- which(colnames(config$DBData$Data)=="Table_code")
i_Var_code    <- which(colnames(config$DBData$Data)=="Var_code")
i_Data_type	  <- which(colnames(config$DBData$Data)=="Data_type")
i_DB_choice	  <- which(colnames(config$DBData$Data)=="DB_scheme")
i_DB_scheme	  <- which(colnames(config$DBData$Data)=="DB_scheme")
i_DB_table	  <- which(colnames(config$DBData$Data)=="DB_table")
i_Field       <- which(colnames(config$DBData$Data)=="Field")
i_Data_name   <- which(colnames(config$DBData$Data)=="Data_name")
i_Unit        <- which(colnames(config$DBData$Data)=="Unit")

nbcol <- length(grep("Code",colnames(config$DBData$Data)))  # nombre de colonnes de codes ? parcourir

# cr?ation du vecteur qui accueille le num?ro de la ligne du tableau 'Data' qui d?crit l'intensit? de l'activit?
config$A_ilindata <- array(NA,dim=c(ni,1),dimnames=list(paste("i",1:ni,sep=""),c("ilindata")))

# cr?ation du vecteur qui accueille le code activit? agr?g? quand un jeu de donn?es d?crit plusieurs activit?s
config$Aiag <- array(NA,dim=c(ni,1),dimnames=list(paste("i",1:ni,sep=""),c("Aiag")))

# - v?rification si jeux de donn?es existants pour les activit?s dans feuillet 'Data'
for(i in 1:ni){ # boucle sur les activit?s d?crites dans le feuillet 'Typo_activit?s'
  print(paste("-----------------ACTIVITE ",config$A_code[i], "   //   ",config$A_name[i],"----------------------",sep=""))
  print(paste("# "))
  
  # - recherche du jeu de donn?es correspondant sur l'intensit? dans les colonnes Code# de la table config$DBData$Data
  for(icol in 1:nbcol){  # on parcourt les colonnes Code# pour trouver les codes activit?s
    
    vec_codesA <- config$DBData$Data[,which(colnames(config$DBData$Data)==paste("Code",icol,sep=""))]
    
    # ?valuation du numero de la ligne du feuillet 'Data' o? est trouv? le code activit? de l'activit? i 
    if(length(which(as.matrix(vec_codesA) == as.character(config$A_code[i]))) > 0) {   # au moins une correspondance du code activit? trouv?e dans colonne Code#
      vec_A_ilindata <- which(as.matrix(vec_codesA) == as.character(config$A_code[i]))  
      # rajout condition : il s'agit du champ relatif ? l'intensit?
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

# - cr?ation de la liste reduite d'activites d'apr?s donn?es disponibles et correspondances Ai et Aiag

# -- assignement d'un code agr?g? pour chaque activit? Ai
for(i in 1:ni){ # boucle sur les activites decrites dans le feuillet 'Typo_activites'
  if(!is.na(config$A_ilindata)[i]){
    # - code agrege = concatenation des codes activit?s correspondant ? un m?me jeu de donn?es
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

# -- nombre d'activit?s sur lesquelles porte l'?valuation : 
Codes_i_ag <- unique(config$Aiag[!is.na(config$Aiag)])
ni_ag     <- length(Codes_i_ag)

if(config$simul$lrepr_activite==0){
  A_zi <-c()
  vec_A_zi_names <- c()
  
  # ouverture des tables, remplissage de la matrice A_iz 
  vec_A_zi_names <- c()
  for(i in 1:ni_ag){
    print("#")
    print(paste("# enregistrement et trac? de la variable activit? : ",Codes_i_ag[i] ))
    # num?ro de ligne de la table 'data' correspondant ? l'activit?
    ilin <- config$A_ilindata[which(config$Aiag==Codes_i_ag[i])[1]]
    
    Ai_nametab <- paste("A_",config$DBData$Data[ilin,i_Table_code],sep="")
    
    # r?cup?ration de la table
    Ai_table   <- eval(parse(text=paste("list_sauv$",Ai_nametab,sep="",collapse=" ; ")))
    
    print(paste("# -> cette activit? a un jeu de donn?es : ",paste("list_sauv$",Ai_nametab,sep="",collapse=" ; ")))
    print(paste("# ->                             champ  : ",config$DBData$Data[ilin,i_Field]))
    
    # - subset de la couche SIG
    Ai_z <- subset(Ai_table, eval(parse(text=paste("Ai_table$id2 %in% grid_z@data@.Data[[",i_idz,"]]",sep="",collapse=" ; "))))
    
    # enregistrement des indices des mailles de la table dans colonne "mesh"
    if(i==1){
      A_zi    <-  col_idz
      vec_zid <- A_zi
    }
    # indice du champ de la table 'Data' rassemblant les donn?es activit?s
    i_idAi    <- which(colnames(Ai_z@data)==config$DBData$Data[ilin,i_Field])
    
    # enregistrement dans la matrice A_zi des donn?es activit?s
    
    # -- comme il peut y avoir plusieurs lignes pour une m?me maille : moyenne des valeurs
    # -- cette fonction ordonne aussi les valeurs d'activit?s selon l'ordre des mailles de vec_zid
    vec <-  matrix(data=0, nrow=nz, ncol=1)
    for (i in 1:nz){
      if (vec_zid[i] %in% Ai_z@data@.Data[[which(colnames(Ai_z@data)=="id2")]]){
        vec2 <- subset(Ai_z@data,Ai_z@data@.Data[[which(colnames(Ai_z@data)=="id2")]]==vec_zid[i]) 
        vec[i] <- mean(as.numeric(unlist(vec2[i_idAi])),na.rm=T)
      } 
    }
    
    nameVARz <- config$Aiag[which(config$Aiag==Codes_i_ag[i])[1]]
    VARz <- cbind.data.frame(vec_zid,as.matrix(vec)) ; colnames(VARz) <- c("mesh",nameVARz)
    
    # -- remplissage de la matrice Intensit? des activit?s A_zi
    A_zi <- cbind.data.frame(A_zi,VARz[,2])
    
    vec_A_zi_names <- c(vec_A_zi_names,config$DBData$Data[ilin,i_Data_code])
    
  } # fin boucle sur les activit?s Ai_ag
  vec <-  c("mesh",Codes_i_ag)
  colnames(A_zi) <-vec
  
} else {
  A_zi <- sauv_activite$A_zi
  vec_A_zi_names <- sauv_activite$vec 
  vec_zid <- sauv_activite$vec_zid
}

# Fonction de normalisation
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
# Stockage des activit?s normalis?es dans une matrice
A_zi_lognorm <- as.matrix(vec_zid)
for(i in 2:dim(A_zi)[2]){
  A_zi_lognorm <- cbind.data.frame(A_zi_lognorm,f_lognorm(A_zi[,i]))
  colnames(A_zi_lognorm)[i] <- paste(colnames(A_zi)[i], "_lognorm", sep="")
}
colnames(A_zi_lognorm)[1] <- "mesh"

## Import de la matrice activit?s pressions
ideb_A <- 3
ifin_A <- length(config$Mat_AP[,1])
ideb_P <- 5  
ifin_P <- length(config$Mat_AP[1,])
vec_lines <- which(config$Mat_AP[,1]=="Final")

# - liste des activit?s dans la matrice
list_Acode_MatAP <- config$Mat_AP[vec_lines,2]
list_Aname_MatAP<-character(length=length(list_Acode_MatAP))
# - liste des pressions dans la matrice
list_Pcode_MatAP <- config$Mat_AP[1,ideb_P:ifin_P] 
list_Pname_MatAP <- config$Mat_AP[2,ideb_P:ifin_P] 
# Rassemblement du nom de l'activt? et de la phase lorsqu'une phase est pr?cis?e.
for (i in 1:length(list_Aname_MatAP)) {
  if (is.na(config$Mat_AP[vec_lines[i],4])){
    list_Aname_MatAP[i]=config$Mat_AP[vec_lines[i],3]
  } else {
    list_Aname_MatAP[i]=paste(config$Mat_AP[vec_lines[i],3],config$Mat_AP[vec_lines[i],4],sep="_")
  }
}


# -- creation de la matrice utilis?e pour l'?valuation et indices des dimensions
MatAP <- config$Mat_AP[vec_lines,ideb_P:ifin_P]
colnames(MatAP) <- list_Pcode_MatAP ; rownames(MatAP) <- list_Acode_MatAP 
ni_MatAP <- dim(MatAP) [1] ; nj_MatAP <- dim(MatAP) [2] # Nombre de colonnes et de lignes de la matrice
for(i in 1:ni_MatAP){
  for(j in 1:nj_MatAP){
    if(MatAP[i,j]=="non" & !is.na(MatAP[i,j])) {
      MatAP[i,j] <- 0 #On remplace les tirets par des 0 (pour faciliter le lecture de la matrice)
    }
    if(MatAP[i,j]=="oui" & !is.na(MatAP[i,j])) {
      MatAP[i,j] <- 1 #De meme on remplace les x par des 1
    } 
    if(is.na(MatAP[i,j]) | (MatAP[i,j] != 0 & MatAP[i,j] != 1)) {
      MatAP[i,j] <- NA # On met des NA au lieu de ND (que R ne sait pas interpr?ter)
    }
  }
}
MatAP <- as.data.frame(MatAP)
for(i in 1:dim(MatAP)[2]){
  MatAP[,i] <- as.numeric(as.character(MatAP[,i]))
}

# Extraction des indices de confiance de la matrice A/P
vec_lines_IC <- which(config$Mat_AP[,1]=="IC")
MatAP_IC <- config$Mat_AP[vec_lines_IC,ideb_P:ifin_P]
colnames(MatAP_IC) <- list_Pcode_MatAP ; rownames(MatAP_IC) <- list_Acode_MatAP 
MatAP_IC[MatAP_IC=="ND"]<-0
MatAP_IC[is.na(MatAP)]<-0
MatAP_IC <- as.data.frame(MatAP_IC)
for(i in 1:dim(MatAP_IC)[2]){
  MatAP_IC[,i] <- as.numeric(as.character(MatAP_IC[,i]))
}



##Boucle des pressions r?duite pour obtenir uniquement le nom des pressions que l'on va int?grer dans l'analyse
vec_controle_P <- array(0,dim=(nj))
vec_P_name_eval <- c()
vec_P_code_eval <- c()
for(j in 1:nj){
  id_Pcode_corres <- which(P_code==config$P_code[j])
  if(length(id_Pcode_corres)==0){   #pression agr?g?e
    id_Pcode_corres <- which(P_code==config$P_code_ag[j])[1]
  }
  if(config$P_evalDEM[j]==1){  #reduction du nombre de pressions pour le d?monstrateur
    
    if(!is.na(config$P_code_ag[j])){    # pression agr?g?e
      # verification que la pression agregee n a pas d?j? ?t? calcul?e
      if(max(vec_controle_P[which(config$P_code_ag==config$P_code_ag[j])])==1){vec_controle_P[j] <- 1} ###N?cessit? de mettre vec_controle_P[j] <- 1 apr?s avoir r?alis? le calcul sur la premi?re pression agr?g?e, sinon boucle inutile
      list_iP_ag <- config$P_code[which(config$P_code_ag==config$P_code_ag[j])]
      #title_nameP <- config$P_code_ag[j]
    }else{                               # pression non agr?g?e
      list_iP_ag  <- config$P_code[j]
      #title_nameP <- config$P_code[j]      
    }
    if(vec_controle_P[j]==0){           # ?vitons de refaire le calcul pour pressions agr?g?es d?j? calcul?es
      vec_P_name_eval <- c(vec_P_name_eval,P_name[id_Pcode_corres])
      vec_P_code_eval <- c(vec_P_code_eval,P_code[id_Pcode_corres])  
    } #boucle vec_controle_P
  } 
  if(length(list_iP_ag)>1) {vec_controle_P[j]<-1}
}

# Double boucle permettant de r?duire les matrices cr??es ci-dessus aux pressions et activit?s utilis?es dans l'analyse
for(i in 2:dim(A_zi)[2]){
  iA_code <- colnames(A_zi)[i] #extraction du code de l'activit?
  list_iA_ag <- config$A_code[which(config$Aiag==iA_code)]  #
  n_act <- length(list_iA_ag)
  sub <- MatAP[which(rownames(MatAP) %in% list_iA_ag),]
  subIC <- MatAP_IC[which(rownames(MatAP_IC) %in% list_iA_ag),]
  sub_final <- as.data.frame(matrix(data=0, nrow=1, ncol=length(vec_P_code_eval), dimnames = list(iA_code, vec_P_code_eval)))
  subIC_final <- as.data.frame(matrix(data=0, nrow=1, ncol=length(vec_P_code_eval), dimnames = list(iA_code, vec_P_code_eval)))
  for(j in 1:length(vec_P_code_eval)){
    iP_code <- vec_P_code_eval[j]
    if(iP_code %in% config$P_code) {
      list_iP_ag <- P_code[which(P_code==iP_code)]
    } else {
      list_iP_ag <- config$P_code[which(config$P_code_ag==iP_code)]
    }
    # Dans le cas de pressions ou d'activit?s agr?g?es, on fait une moyenne des indices de confiance 
    # Si l'un des couples activit?s agr?g?es/pressions agr?g?es
    if(length(which(is.na(unlist(sub[,which(colnames(sub) %in% list_iP_ag)]))))==0){
      sub_final[which(colnames(sub_final)==iP_code)] <- max(unlist(sub[,which(colnames(sub) %in% list_iP_ag)]), na.rm = T)
    } else {
      sub_final[which(colnames(sub_final)==iP_code)] <- NA
    }
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


# Code pour extraire les indices de confiances
# ouverture des tables et remplissage de la matrice A_zi_IQ

if(config$SauvRep$lrepr_iq==0){
  
  if(config$factor$X5 == 1){
    print("IMPORT DES INDICES DE CONFIANCE DANS LA QUALITE DES DONNEES")
    for(i in 1:ni_ag){
      print("#")
      print(paste("# Extraction indices de qualit? pour l'activit? : ",Codes_i_ag[i] ))
      # num?ro de ligne de la table 'data' correspondant ? l'activit?
      ilin <- config$A_ilindata[which(config$Aiag==Codes_i_ag[i])[1]]
      # nom de la table
      Ai_nametab <- paste("A_",config$DBData$Data[ilin,i_Table_code],sep="")
      
      # nom du champ
      Ai_field <- paste(config$DBData$Data[ilin,i_Field], "_iq", sep = "")
      # r?cup?ration de la table
      Ai_table   <- eval(parse(text=paste("list_sauv$",Ai_nametab,sep="",collapse=" ; ")))
      # - subset de la couche SIG
      Ai_z <- subset(Ai_table, eval(parse(text=paste("Ai_table$id2 %in% grid_z@data@.Data[[",i_idz,"]]",sep="",collapse=" ; "))))
      
      # enregistrement des indices des mailles de la table dans colonne "mesh"
      if(i==1){
        A_zi_iq    <-  col_idz
      }
      # indice du champ de la table 'Data' rassemblant les donn?es activit?s
      i_idAi    <- which(colnames(Ai_z@data)==Ai_field)
      
      # enregistrement dans la matrice A_zi des donn?es activit?s
      
      # -- comme il peut y avoir plusieurs lignes pour une m?me maille : moyenne des valeurs
      # -- cette fonction ordonne aussi les valeurs d'activit?s selon l'ordre des mailles de vec_zid
      vec <-  matrix(data=5, nrow=nz, ncol=1)
      for (j in 1:nz){
        if (vec_zid[j] %in% Ai_z@data@.Data[[which(colnames(Ai_z@data)=="id2")]]){
          vec2 <- subset(Ai_z@data,Ai_z@data@.Data[[which(colnames(Ai_z@data)=="id2")]]==vec_zid[j]) 
          vec[j] <- mean(as.numeric(unlist(vec2[i_idAi])),na.rm=T)
        } 
      }
      nameVARz <- paste(config$Aiag[which(config$Aiag==Codes_i_ag[i])[1]], "_iq", sep = "")
      VARz <- cbind.data.frame(vec_zid,as.matrix(vec)) ; colnames(VARz) <- c("mesh",nameVARz)
      # -- remplissage de la matrice Intensit? des activit?s A_zi
      A_zi_iq <- cbind.data.frame(A_zi_iq,VARz[,2])    
    } # fin boucle sur les activit?s Ai_ag
    vec <-  c("mesh",paste(Codes_i_ag, "iq", sep="_"))
    colnames(A_zi_iq) <- vec
  }
  if(config$SauvRep$lsauv_iq==1){
    filesauv_iq <- paste(sauvrepdir,"/IQ_",chainchar,"_",datesauv,".Rdata",sep="")
    save(A_zi_iq, file = filesauv_iq)
  }
} else {  
  filerepr_iq <- paste(sauvrepdir,"/IQ_",chainchar,"_",config$SauvRep$daterepr_iq,".Rdata",sep="")
  load(filerepr_iq)
}

## Cartographie des pressions directes, stock?es dans la matrice Pj_direct_z
Pj_direct_z <- lHabBenth_z$idmesh_z
for(j in 1:dim(MatAP_sub)[2]){
  P <- colnames(MatAP_sub)[j]
  if(P %in% config$P_code) id_config <- which(config$P_code == P)
  if(P %in% config$P_code_ag) id_config <- which(config$P_code_ag == P)[1]
  Mat_PjAi_z    <- lHabBenth_z$idmesh_z
  if(config$P_modcalc[id_config]==2){
    print(paste("#  >> pression : ",P,"    -------------------------",sep=""))
    print("Pression directe ind?pendante de la matrice activit? pression")
    for(icol in 1:nbcol){ #Boucle sur les colonnes Code dans le feuillet Data
      vec_codesA <- config$DBData$Data[,which(colnames(config$DBData$Data)==paste("Code",icol,sep=""))] #Colonne du feuillet Data associ?e au num?ro de code icod
      if(length(which(as.matrix(vec_codesA) == as.character(P))) > 0){ #Si la pression est contenue dans la colonne
        P_ilindata <- which(as.matrix(vec_codesA) == as.character(P)) #On enregistre le num?ro de ligne associ? ? la pression
        Pj_nametab <- paste("P_",config$DBData$Data[P_ilindata,i_Table_code],sep="") 
        Pj_table   <- eval(parse(text=paste("list_sauv$",Pj_nametab,sep="",collapse=" ; "))) #Sauvegarde des donn?es import?es
        print(paste("# -> cette pression a un jeu de donn?es : ",paste("list_sauv$",Pj_nametab,sep="",collapse=" ; ")))
        print(paste("# ->                             champ  : ",config$DBData$Data[P_ilindata,i_Field]))
        Pj_z <- subset(Pj_table, eval(parse(text=paste("Pj_table$id2 %in% grid_z@data@.Data[[",i_idz,"]]",sep="",collapse=" ; ")))) #On extrait seulement les mailles contenues dans la grille
        i_idPj    <- which(colnames(Pj_z@data)==config$DBData$Data[P_ilindata,i_Field]) # Num?ro de colonne de la table contenant le champ d'int?r?t
        vec <-  matrix(data=0, nrow=nz, ncol=1) # On initialise avec des z?ros: permet d'avoir d?j? 0 pour les mailles non contenues dans la table
        for (i in 1:nz){ # Boucle sur toutes les mailles
          if (vec_zid[i] %in% Pj_z@data@.Data[[which(colnames(Pj_z@data)=="id2")]]){ # Si la maille est contenue dans la table de la pression
            vec2 <- subset(Pj_z@data,Pj_z@data@.Data[[which(colnames(Pj_z@data)=="id2")]]==vec_zid[i]) # On extrait toutes les lignes de la table associ?es ? cette maille
            vec[i] <- mean(as.numeric(unlist(vec2[i_idPj])),na.rm=T) # Dans le cas ou on a plusieurs valeur de pression pour la m?me maille, on fait une moyenne
          } 
        }
        Pj_direct_z <- cbind.data.frame(Pj_direct_z, vec) # On int?gre le vecteur au reste du tableau des pressions
        colnames(Pj_direct_z)[dim(Pj_direct_z)[2]] <- P
      }
    }
  }
}
colnames(Pj_direct_z)[1] <- "mesh"


# Cr?ation d'un tableau permettant d'acc?l?rer le temps de calcul de la sensibilit?
# Autant de ligne que dans la table des habitats benthiques
# La premi?re colonne (n_id) indique le num?ro de ligne dans vec_zid associ? ? la maille dans laquelle se trouve l'habitat
# La deuxi?me colonne (ratio_hab) donne le ratio entre la surface de l'habitat et la surface maritime de la maille
n_id <- array(NA,dim=nb_poly) # On initialise les deux colonnes
ratio_hab <- array(NA,dim=nb_poly)
for(ipoly in 1:nb_poly){ # Boucle sur tous les habitats
  n_id[ipoly] <- which(lHabBenth_z$idmesh_z==HabBenth_z@data@.Data[[i_idzhab]][[ipoly]]) # on r?cup?re le num?ro de ligne de la maille
  ratio_hab[ipoly] <- (HabBenth_z@data@.Data[[i_surfhab]][[ipoly]]/HabBenth_z@data@.Data[[i_surfmer]][[ipoly]]) # et le ratio
}
# Conversion des deux colonnes au format num?rique
n_id <- as.numeric(n_id)
ratio_hab <- as.numeric(ratio_hab)
# Rassemblement dans un m?me tableau pour faciliter l'utilisation
speed_sensib <- cbind.data.frame(n_id, ratio_hab)


print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 2 - CODAGE DES FONCTIONS ASSOCIEES AUX FACTEURS                           ")
print("# ------------------------------------------------------------------------------------------")

## 1er FACTEUR: Le mode de cumul des pressions qui peut ?tre additif, antagoniste ou synergique. 
# Codage d'une pression qui ? partir d'un vecteur num?rique renvoie un cumul antagoniste des valeurs contenues dans le vecteur. 
antagoniste <- function(vec){
  n <- length(vec)
  out <- 0
  for (i in 1:n){
    vec_i <- vec[order(vec, decreasing = T)][i]
    out <- out + vec_i*((n-i+1)/n)
  }
  return(out)
}
# Codage d'une pression qui ? partir d'un vecteur num?rique renvoie un cumul synergique des valeurs contenues dans le vecteur. 
synergique <- function(vec){
  n <- length(vec)
  out <- 0
  for (i in 1:n){
    vec_i <- vec[order(vec, decreasing = T)][i]
    out <- out + vec_i*((n+i-1)/n)
  }
  return(out)
}


##2?me FACTEUR: Le lien activit? pression avec 4 options: 
#Une relation lin?aire (pas besoin de fonction, il suffit d'?crire Activit?=pression)
#Une relation "optimiste": 
optimiste <- function(x) (((2/(2-x))^7)-1)/((2^7)-1)
#Une relation "pessimiste": 
pessimiste <- function(x) (2^7-(2/(1+x))^7)/((2^7)-1)
#Une relation bas?e sur un seuil (courbe logistique): 
logistique <- function(x) 1/(1+exp(-15*(x-0.5)))


## 3?me FACTEUR: Erreurs dans la matrice sensibilit?
#Fonction qui permet d'obtenir l'inverse d'une fonction
inverse = function (f, lower, upper) {
  function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper)[1]
}
# Codage d'une fonction qui ? partir d'une valeur de sensibilit? (entre 0 et 3) et d'un indice de confiance associ? ? cette valeur renvoie une valeur al?atoire prenant en compte l'incertitude autour du r?sultat
f_sensib <- function(val, disp, maxval){
  f <- function(x) x/(1-x) #Cette fonction permet de connaitre le rapport entre les deux param?tres de la loi b?ta pour pouvoir centrer la distribution b?ta al?atoire sur la valeur d'origine
  if (val==99 | disp==0){
    a=1
    b=1
  }
  else{
    val <- val/maxval #On ram?ne la valeur de sensibilit? entre 0 et 1 pour pouvoir utiliser la loi de probabilit? b?ta
    if(val==0){
      a=1
      b=(5^disp)*0.5
    }
    if(val==1){
      a=(5^disp)*0.5
      b=1
    }
    if(val<0.5 && val>0){
      a=f(val)*(5^disp)*0.5
      b=(5^disp)*0.5
    }
    if(val<1 && val>0.5){
      a=(5^disp)*0.5
      b=(5^disp)*0.5/f(val)
    }
    if(val==0.5){
      a=(5^disp)*0.5
      b=(5^disp)*0.5
    }
  }
  Beta_inverse = inverse(function(x) pbeta(x, shape1=a, shape2=b), 0, 1)
  out <- maxval*Beta_inverse(runif(1, 0, 1))[[1]]
  return(out)
}

# Cr?ation d'une fonction alternative pour quand on ne dispose pas d'indice de confiance
# Renvoie soit une valeur al?atoire entre 0 et 3 lorsqu'on ne connait pas la sensibilit?
f_sensib2 <- function(val){
  if(val==99) out <- round(runif(1, min=0, max=5), digits=2)
  else out <- val
  return(out)
}

## 4?me FACTEUR: erreur dans la matrice activit?-pression
# Fonction qui ? partir d'un lien activit? pression et d'un indice de confiance pour lien g?n?re al?atoirement un lien A/P
# Le lien AP g?n?r? a d'autant plus de chance de correspondre ? la valeur d'origine que l'indice de confiance est ?lev?. 
f_AP <- function(ap, ic){
  p <- 0.5 + 0.1*ic
  if(ap==0 | ic==0){
    return(rbinom(n=1, size=1, prob=1-p))
  } else {
    return(rbinom(n=1, size=1, prob=p))
  }
}


## 5?me FACTEUR: erreurs dans les donn?es activit?s
# En attente des indices de qualit? des donn?es

## 6?me FACTEUR: Fonction de distance qui peut s'?tendre jusqu'? 3 mailles (pour l'instant)
# Cr?ation d'une matrice contenant les latitudes et logitudes extr?mes de chacune des mailles
grid_test <- cbind.data.frame(vec_zid, config$longmax_z, config$longmin_z, config$latmax_z, config$latmin_z)
colnames(grid_test) <- c("mesh","longmax","longmin","latmax","latmin")
grid_test[,1] <- as.character(grid_test[,1])
# Codage d'une fonction qui a une maille donn?e associe toutes les mailles adjacentes
ID_around_z <- function(id, grid){
  id_z <-  which(grid$mesh==id) #On identifie le num?ro de ligne de la maille d'entr?e
  ID <- c() #Cr?ation d'un vecteur vide dans lequel on va stocker les diff?rentes mailles adjacentes
  ##Utilisation des latitudes et longitudes extr?mes pour identifier les 8 mailles adjacentes
  if(dim(grid[which(grid$longmax==grid$longmin[id_z] & grid$latmin==grid$latmin[id_z]),])[1]==1){ #si il existe une maille de type
    ID <- c(ID, grid[which(grid$longmax==grid$longmin[id_z] & grid$latmin==grid$latmin[id_z]),1])
  }
  if(dim(grid[which(grid$longmax==grid$longmin[id_z] & grid$latmin==grid$latmax[id_z]),])[1]==1){
    ID <- c(ID, grid[which(grid$longmax==grid$longmin[id_z] & grid$latmin==grid$latmax[id_z]),1])
  }
  if(dim(grid[which(grid$longmax==grid$longmin[id_z] & grid$latmax==grid$latmin[id_z]),])[1]==1){
    ID <- c(ID, grid[which(grid$longmax==grid$longmin[id_z] & grid$latmax==grid$latmin[id_z]),1])
  }
  if(dim(grid[which(grid$longmin==grid$longmin[id_z] & grid$latmax==grid$latmin[id_z]),])[1]==1){
    ID <- c(ID, grid[which(grid$longmin==grid$longmin[id_z] & grid$latmax==grid$latmin[id_z]),1])
  }
  if(dim(grid[which(grid$longmin==grid$longmax[id_z] & grid$latmax==grid$latmin[id_z]),])[1]==1){
    ID <- c(ID, grid[which(grid$longmin==grid$longmax[id_z] & grid$latmax==grid$latmin[id_z]),1])
  }
  if(dim(grid[which(grid$longmin==grid$longmax[id_z] & grid$latmax==grid$latmax[id_z]),])[1]==1){
    ID <- c(ID, grid[which(grid$longmin==grid$longmax[id_z] & grid$latmax==grid$latmax[id_z]),1])
  }
  if(dim(grid[which(grid$longmin==grid$longmax[id_z] & grid$latmin==grid$latmax[id_z]),])[1]==1){
    ID <- c(ID, grid[which(grid$longmin==grid$longmax[id_z] & grid$latmin==grid$latmax[id_z]),1])
  }
  if(dim(grid[which(grid$longmin==grid$longmin[id_z] & grid$latmin==grid$latmax[id_z]),])[1]==1){
    ID <- c(ID, grid[which(grid$longmin==grid$longmin[id_z] & grid$latmin==grid$latmax[id_z]),1])
  }
  return(ID)
}
# Codage d'une fonction qui associe ? une matrice ayant les identifiants de maille en premi?re colonne une 2?me matrice
# qui contient les num?ros de ligne dans la matrice de toutes les mailles adjacentes ? chaque maille
ID_around_zbis <- function(grille, mat){
  n = dim(mat)[1] 
  out <- cbind.data.frame(mat[,1], matrix(data=0, nrow=n, ncol=8)) #La premi?re colonne contient les id de maille, il y a ensuite 8 autres colonnes correspondant aux 8 mailles adjacentes potentielles
  for(i in 1:n){ #On r?alise la boucle sur toutes les mailles de la matrice
    print(paste("recherche des mailles adjacentes - maille ",i, "/", n, sep=""))
    id <- mat[i,1] #On identifie la maille i
    z_adj <- ID_around_z(id, grille) #On identifie les mailles adjacentes ? la maille i
    n_zadj <- as.numeric(length(which(mat[,1] %in% z_adj))) #Nombre de mailles ajacentes ? la maille i dans la matrice d'entr?e
    if(n_zadj > 0){ #Si il y a au moins une maille ajacente
      for (k in 1:n_zadj) out[i,k+1] <- which(mat[,1] %in% z_adj)[k] # On remplit le tableau avec les num?ros de lignes des mailles adjacentes
    }
  }
  return(out)
}
# Codage qui en fournissant une matrice activit? et des num?ros de lignes des mailles adjacentes renvoie la m?me matrice activit? mais avec une fonction de distance d'une maille
F_dist <- function(mat, mat_ID){
  A_zi_sub2 <- mat #Cr?ation de la matrice de sortie qui est ? l'origine identique ? la m?rice d'entr?e
  n_Ai <- dim(mat)[2] #Nombre de mailles dans la matrice
  dist_j <- which(mat_ID[,2] > 0)
  for(j in 1:length(dist_j)){
    i <- dist_j[j]
    IDZ <- as.numeric(mat_ID[i,which(mat_ID[i,2:9]>0)+1]) #Vecteur contenant les num?ros de ligne de ces maillles adjacentes
    sup0 <- which(mat[i, 2:n_Ai] == 0)+1 #activit?s nulles dans la maille
    if(length(sup0) > 0) { #Si au moins une activit? est nulle dans la maille, on rajoute la moiti? du maximum des mailles adjacentes
      A_zi_sub2[i, sup0] <- unlist(apply(as.matrix(c(1:length(sup0))), 1, function(x) 0.5*max(mat[IDZ, sup0[x]])))
    }
  }
  return(A_zi_sub2)
}

# Fonction de distance alternative, optimale quand tr?s peu de mailles ont des activit?s nulles
F_dist2 <- function(mat, mat_ID){
  A_zi_sub2 <- mat #Cr?ation de la matrice de sortie qui est ? l'origine identique ? la m?rice d'entr?e
  nz_sub <- dim(mat)[1] #Nombre de mailles dans la matrice
  for(j in 2:dim(mat)[2]){
    dist_j <- which(mat[,j]==0 & mat_ID[,2]>0) #On cherche les mailles ayant la valeur d'activit? ?gale ? 0 et des mailles adjacentes
    if (length(dist_j > 0)){
      A_zi_sub2[dist_j, j] <- unlist(apply(as.matrix(c(1:length(dist_j))), 1, function(x) 0.5*max(mat[as.numeric(mat_ID[dist_j[x], which(mat_ID[dist_j[x], 2:9] > 0)+1]), j])))
    }
  }
  return(A_zi_sub2)
}

# Codage d'une fonction qui applique n fois la fonction de distance sur une matrice activit? et qui stocke chaque matrices dans une liste
Distance <- function(mat, mat_ID, n){
  out <- list()
  out[[1]] <- mat #Premier ?l?ment de la liste: la matrice d'origine
  for(i in 1:n){
    if(i==1){
      print(paste("Calcul fonction de distance sur 1 maille"), sep="")
      out[[i+1]] <- F_dist(out[[i]], mat_ID) #On applique ? chaque fois la fonction de distance et on stocke la nouvelle matrice dans la liste
    }
    else{
      print(paste("Calcul fonction de distance sur ", i, " mailles"), sep="")
      out[[i+1]] <- F_dist2(out[[i]], mat_ID) #La fonction de distance 2 est optimale ? partir de la 2?me maille car moins d'activit?s nulles
    }
  }
  return(out)
}

# Utilisation des fonctions cod?es ci-dessus pour calculer les num?ros de mailles adjacents de la matrice activit?
A_zi_sub <- A_zi_lognorm #On cr?? une matrice suppl?mentaire pour ne pas supprimer la matrice activit? d'origine
A_zi_sub[,1] <- as.character(A_zi_sub[,1]) #On passe la premi?re colonne de facteur ? character pour que les fonctions marchent
for(i in 2:dim(A_zi_sub)[2]){ #Remplacement des NA ?ventuels par des z?ros
  if (dim(A_zi_sub[which(is.na(A_zi_sub[,i])),])[1]>0){
    A_zi_sub[which(is.na(A_zi_sub[,i])),i] <- 0
  }
}
if(config$simul$lrepr_activite==0){
  if(config$factor$X6 == 1) Id_zadj <- ID_around_zbis(grid_test, A_zi_sub) #Application ? la matrice activit?
} else {
  if(config$factor$X6 == 1) Id_zadj <- sauv_activite$Id_zadj
}

## 7?me FACTEUR: choix entre score m?dian et principe de pr?caution: pas besoin de fonctions
# Un simple ?chantillonage al?atoire d'un des deux suffixes suffira amplement

## Fonction qui permettra d'afficher le temps de calcul une fois les simulations r?alis?es
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

# Cr?ation d'une fonctin qui permet d'exporter une table dans la base de donn?es en lui rajoutant un champ g?om et un certain nombre de m?ta-donn?es
# in.con: connexion ? la base de donn?es
# in.scheme: sch?ma dans lequel on souhaite exporter la table
# in.grid: la grille qui contient l'identifiant, le champ geom et les m?tadonn?es associ?es ? chaque maille
# table contenant les donn?es ? exporter. Elle doit contenir une olonne "mesh" avec les m?mes identifiants que dans la grille
# Nom que l'on souhaite donner ? la table qui sera cr??e dans la base de donn?es
export_table <- function(in.con, in.scheme, in.grid, in.table, in.nametab){
  print(paste("Export de la table-----", in.nametab, "-------dans le sch?ma------",in.scheme, sep=""))
  index_export <- in.grid
  name_id <- colnames(head(index_export))[i_idz]
  New_Attribut <- merge(index_export@data,in.table,by.x=name_id,by.y="mesh",all.x=T,sort=FALSE)
  index_export@data <- New_Attribut 
  dbWriteSpatial(in.con, index_export, schemaname=in.scheme, tablename=in.nametab, replace=T)
  request <- paste("select UpdateGeometrySRID('",in.scheme,"', '",in.nametab,"', 'geom', 4326)", sep="")
  dbExecute(in.con, request)
}

## Fonction pour calculer dans un tableau contenant plusieurs simulations les mailles ayant le risque le plus ?lev? et le moins ?lev?
# Tableu d'entr?e: une premi?re colonne "mesh" avec les id, puis une colonne par simulation. 
# La lsite de sortie contient deux tableaux avec la m?me structure que le tableau d'entr?e: 
# Dans le premier, tableau, pour chaque simulation, les mailles ayant un 1 font partie des 10% (ou autre seuil selon ce qui a ?t? param?tr?) ayant le REFC le plus ?lev?, inversement pour -1
# Idem pour le deuxi?me tableau sauf qu'il s'agit des 25% les plus/moins impact?s (ou autre seuil)
simul_calc <- function(simul_tab, val){
  n <- dim(simul_tab)[2]
  nval <- length(val)
  out <- list()
  for(i in 1:nval){
    out[[i]] <- simul_tab
    for(j in 2:n){
      out[[i]][,j] <- 0
      out[[i]][which(simul_tab[,j]>=as.numeric(quantile(simul_tab[,j], probs = seq(0, 1, by= (val[i]/100))))[100/val[i]]),j] <- 1
      out[[i]][which(simul_tab[,j]<=as.numeric(quantile(simul_tab[,j], probs = seq(0, 1, by= (val[i]/100))))[2]),j] <- -1
    }
  }
  return(out)
}

##Fonction permettant d'obtenir un certain nombre de statistiques sur les simulations
# Tableu d'entr?e: une premi?re colonne "mesh" avec les id, puis une colonne par simulation.
# Tableau de sortie: premi?re colonne mesh avec les id, une colonne avec la moyenne des simulations, une avec l'?cart type et une avec le coefficient de variation
stat_tab <- function(tab){
  out <- tab[,1]
  nco <- dim(tab)[2]
  nli <- dim(tab)[1]
  moy <- c() #Vecteur qui contiendra la moyenne
  std <- c() #Vecteur qui contiendra l'?cart type
  cv <- c() #Vecteur qui contiendra le coefficient de variation
  for(i in 1:nli){ #Boucle sur toutes les mailles
    moy <- c(moy, mean(as.numeric(tab[i, 2:nco]))) #Calcul de la moyenne
    std <- c(std, sd(as.numeric(tab[i, 2:nco]))) #Calcul de l'?cart type
    if(moy[i]>0) { #Pour le cv, on distingue le cas ou la moyenne est nulle: division par 0 impossible, on attribue la valeur de 0 au cv
      cv <- c(cv, sd(as.numeric(tab[i, 2:nco]))/mean(as.numeric(tab[i, 2:nco]))) #sinon, division de l'ecart type par la moyenne
    } else {
      cv <- c(cv, 0)
    }
  }
  out <- cbind.data.frame(out, moy, std, cv)
  colnames(out) <- c("mesh","REFC_moy", "sd_REFC", "cv_REFC")
  return(out)
}

if(config$simul$lsauv_activite==1){
  filesauv_activite <- paste(sauvrepdir,"/activite_",chainchar,"_",datesauv,".Rdata",sep="")
  sauv_activite <- list()
  sauv_activite$A_zi <- A_zi
  sauv_activite$vec <- vec_A_zi_names
  sauv_activite$vec_zid <- vec_zid
  sauv_activite$Id_zadj <- Id_zadj
  save(sauv_activite, file = filesauv_activite)
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








print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 3 - REALISATION DES SIMULATIONS                           ")
print("# ------------------------------------------------------------------------------------------")


#Connexion avec la base de donn?e si l'option export a ?t? choisie
if(config$simul$export_simul|config$simul$export_result){
  #Connexion avec la base de donn?es
  if(config$simul$db_choice=="local"){
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
}

Simul <- function(n){
  out <- vec_zid
  for (k in 1:n){
    
    print("----------------------------------------------------------------------------------------")
    print(paste("DEBUT DE LA SIMULATION N?", k, sep=""))
    print("----------------------------------------------------------------------------------------")
    
    ## 1?re SOUS-ETAPE: Fixation d'un certain nombre de facteurs qui seront les m?mes pour chaque maille
    
    ##Score de sensibilit?: m?dian ou pr?caution: 
    if(config$factor$X7 == 1) suf_sensib <- sample(c("_med","_pre"), 1)
    if(config$factor$X7 == 2) suf_sensib <- "_pre"
    if(config$factor$X7 == 3) suf_sensib <- "_med"
    
    ##Cr?ation d'une matrice activit? pression (MatAP_sub_i) modifi?e selon l'indice de confiance 
    ##Cr?ation d'une matrice (Mat_rel_AP) qui indiquera quelle option de relation activit? pression (optimiste, pessimiste, lin?aire ou logistique)
    ##Cr?ation d'une matrice (Mat_f_dist) qui indique pour chaque couple activit? pression si une fonction de distance s'applique sur 0, 1, 2 ou 3 mailles
    MatAP_sub_i <- MatAP_sub
    Mat_rel_AP <- MatAP_sub
    if(config$factor$X6 == 1) Mat_f_dist <- MatAP_sub
    for(i in 1:dim(MatAP_sub_i)[1]){
      for(j in 1:dim(MatAP_sub_i)[2]){
        if(config$factor$X4 == 1) MatAP_sub_i[i,j] <- f_AP(MatAP_sub[i,j], MatAP_IC_sub[i,j])
        if(config$factor$X2 == 1) Mat_rel_AP[i,j] <- sample(c(1,2,3,4), 1)
        if(config$factor$X2 == 2) Mat_rel_AP[i,j] <- 1
        if(config$factor$X2 == 3) Mat_rel_AP[i,j] <- 2
        if(config$factor$X2 == 4) Mat_rel_AP[i,j] <- 3
        if(config$factor$X2 == 5) Mat_rel_AP[i,j] <- 4
        if(config$factor$X6 == 1) Mat_f_dist[i,j] <- sample(c(1,2,3,4), 1)
      }
    }
    
    ##Modification de l'intensit? des activit?s en prenant en compte l'indice de qualit? des donn?es
    A_zi_sub_i <- A_zi_sub[,1]
    if(config$factor$X5 == 1){
      print("Modification de l'intensit? des activit?s selon l'indice de qualit? des donn?es")
      for(j in 2:dim(A_zi_sub)[2]){ #Boucle sur les activit?s
        A_zi_sub_i <- cbind.data.frame(A_zi_sub_i, apply(as.matrix(c(1:nz)), 1, function(x) f_sensib(A_zi_sub[x,j], A_zi_iq[x,j], 1)))
      }
      colnames(A_zi_sub_i) <- colnames(A_zi_sub)
    } else {A_zi_sub_i <- A_zi_sub}
    
    ## Appliation de la fonction de distance sur les nouvelles donn?es activit?s
    if(config$factor$X6 == 1){
      print("APPLICATION DE LA FONCTION DE DISTANCE")
      Azi_dist_i <- Distance(A_zi_sub_i, Id_zadj, 3)
    }
    
    ##Cumul des pressions: additif, antagoniste ou synergique
    if(config$factor$X1 == 1) cumul <- sample(c("additif","antagoniste","synergique"), 1)
    if(config$factor$X1 == 2) cumul <- "additif"
    if(config$factor$X1 == 3) cumul <- "antagoniste"
    if(config$factor$X1 == 4) cumul <- "synergique"
    
    ## 2?me SOUS-ETAPE: Calcul de la sensibilit?
    print("-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  ")
    print("CALCUL DE LA SENSIBILITE")
    # stockage des r?sultats de sensibilit? cumul?e
    Mat_Sensib_Pj <- lHabBenth_z$idmesh_z
    
    #  - Boucle sur les pressions : calcul du score de sensibilit? cumul?e
    for(j in 1:nj){
      if(P_evalDEM[j]==1){  #reduction du nombre de pressions pour le d?monstrateur
        print(paste("Calcul de la sensibilit? ? la pression ",P_code[j]), sep="")
        i_idPcode <- c()
        i_idIC <- c()
        # code de la pression
        P_code_HabBenth <- paste(tolower(P_code[j]),suf_sensib,sep="")
        # identification de la colonne correspondante ? cette pression
        i_idPcode   <- which(colnames(HabBenth_z@data)==P_code_HabBenth)
        if(config$factor$X3 == 1){
          # code de l'indice de confiance
          P_code_IC <- paste(tolower(P_code[j]),"_icas",sep="")
          # identification de la colonne correspondante ? cet indice de confiance
          i_idIC   <- which(colnames(HabBenth_z@data)==P_code_IC)
        }
        if(length(i_idPcode)==0){
          print(paste("#  STOP : le code pression ",P_code_HabBenth," n a pas d equivalent dans la table habbenth_sensib : ",sep=""))
          stop(paste(lHabBenth_z$P_codes_HabBenth," / ",sep=""))
        }
        
        #cr?ation des vecteurs qui contiennent les r?sultats
        # - score de sensibilit? cumul?
        varName <- paste("SensibC_",P_code[j],sep="")
        res   <- array(0.,dim=c(nz,1),dimnames=list(paste("z",1:nz,sep=""),varName))
        
        for(ipoly in 1:nb_poly){
          if(config$factor$X3 == 1) res[speed_sensib[ipoly,1]] <- res[speed_sensib[ipoly,1]] + speed_sensib[ipoly,2] * f_sensib(HabBenth_z@data@.Data[[i_idPcode]][[ipoly]], HabBenth_z@data@.Data[[i_idIC]][[ipoly]], 5)
          if(config$factor$X3 == 0) res[speed_sensib[ipoly,1]] <- res[speed_sensib[ipoly,1]] + speed_sensib[ipoly,2] * f_sensib2(HabBenth_z@data@.Data[[i_idPcode]][[ipoly]])
        }
        Mat_Sensib_Pj <- cbind.data.frame(Mat_Sensib_Pj,res)
        colnames(Mat_Sensib_Pj)[dim(Mat_Sensib_Pj)[2]] <- varName
      }
    }
    
    ## Correction de la matrice sensibilit?: si la maille n'est pas dans la table gr_eco_hab, on lui attribue une valeur al?atoire entre 0 et 3
    id_nohab <- which(!Mat_Sensib_Pj[,1] %in% HabBenth_z@data@.Data[[i_idzhab]])
    for (i in 2:dim(Mat_Sensib_Pj)[2]){
      Mat_Sensib_Pj[id_nohab,i] <- runif(length(id_nohab), 0, 3)
    }
    
    ##Normalisation entre 0 et 1 des scores de sensibilit?: 
    Mat_Sensib_Pj[2:dim(Mat_Sensib_Pj)[2]] <- Mat_Sensib_Pj[2:dim(Mat_Sensib_Pj)[2]]/3
    
    
    ## 3?me SOUS-ETAPE: Cartographie des pressions
    print("-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  ")
    print("CARTOGRAPHIE DES PRESSIONS")
    Mat_Pj_z <- lHabBenth_z$idmesh_z
    for(j in 1:dim(MatAP_sub)[2]){
      print(paste("Cartographie de la pression ",colnames(MatAP_sub)[j], " - ", vec_P_name_eval[j], sep=""))
      P <- colnames(MatAP_sub)[j]
      if(P %in% config$P_code) id_config <- which(config$P_code == P)
      if(P %in% config$P_code_ag) id_config <- which(config$P_code_ag == P)[1]
      Mat_PjAi_z    <- lHabBenth_z$idmesh_z
      if(config$P_modcalc[id_config]==1){
        compteur_AP <- 0
        for(i in 1:dim(MatAP_sub)[1]){
          PjAi_z <- array(0.,dim=c(nz,1))
          if(MatAP_sub_i[i,j]==1){
            compteur_AP <- compteur_AP + 1
            if(Mat_rel_AP[i,j]==1){
              if(config$factor$X6 == 1) PjAi_z <- Azi_dist_i[[Mat_f_dist[i,j]]][,i+1]
              if(config$factor$X6 == 0) PjAi_z <- A_zi_sub_i[,i+1]
            }
            if(Mat_rel_AP[i,j]==2){
              if(config$factor$X6 == 1) PjAi_z <- optimiste(Azi_dist_i[[Mat_f_dist[i,j]]][,i+1])
              if(config$factor$X6 == 0) PjAi_z <- optimiste(A_zi_sub_i[,i+1])
            }
            if(Mat_rel_AP[i,j]==3){
              if(config$factor$X6 == 1) PjAi_z <- pessimiste(Azi_dist_i[[Mat_f_dist[i,j]]][,i+1])
              if(config$factor$X6 == 0) PjAi_z <- pessimiste(A_zi_sub_i[,i+1])
            }
            if(Mat_rel_AP[i,j]==4){
              if(config$factor$X6 == 1) PjAi_z <- logistique(Azi_dist_i[[Mat_f_dist[i,j]]][,i+1])
              if(config$factor$X6 == 0) PjAi_z <- logistique(A_zi_sub_i[,i+1])
            }
          }
          Mat_PjAi_z <- cbind.data.frame(Mat_PjAi_z, PjAi_z)
        }
        if(compteur_AP==0){
          Mat_Pj_z <- cbind.data.frame(Mat_Pj_z,array(0.,dim=nz))
          Mat_Pj_z[,j+1]<-as.numeric(Mat_Pj_z[,j+1])
        } else {Mat_Pj_z <- cbind.data.frame(Mat_Pj_z,      
                                             apply(as.matrix(c(1:nz)),1,function(x) sum(Mat_PjAi_z[x,2:dim(Mat_PjAi_z)[2]],na.rm=TRUE)))
        }
      } else { #Cas des pressions directes : pas de lien avec la matrice AP, import direct depuis la BD
        print(paste("#  >> pression : ",P,"    -------------------------",sep=""))
        Mat_Pj_z <- cbind.data.frame(Mat_Pj_z, Pj_direct_z[,which(colnames(Pj_direct_z)==P)])
      }
    }
    colnames(Mat_Pj_z) <- c("mesh", colnames(MatAP_sub))
    
    ## Normalisation des pressions
    Mat_Pj_lognorm_z <- as.matrix(vec_zid) ; colnames(Mat_Pj_lognorm_z) <- c("mesh")
    for(j in 2:dim(Mat_Pj_z)[2]){
      Mat_Pj_lognorm_z <- cbind.data.frame(Mat_Pj_lognorm_z,f_lognorm(Mat_Pj_z[,j]))
      colnames(Mat_Pj_lognorm_z)[j] <- paste(vec_P_code_eval[j-1],"_lognorm",sep="")
    }
    
    ## 4?me SOUS-ETAPE: Calcul du risque d'effet
    print("-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  ")
    print("CALCUL DU RISQUE D'EFFET")
    Mat_REF_Pj_z <- vec_zid
    for(j in 2:dim(Mat_Pj_lognorm_z)[2]){  
      Mat_REF_Pj_z  <- cbind.data.frame(Mat_REF_Pj_z ,Mat_Pj_lognorm_z[,j]*Mat_Sensib_Pj[,j]) 
      colnames(Mat_REF_Pj_z)[j] <- paste("REF_",colnames(Mat_Pj_z)[j],sep="")
    }
    colnames(Mat_REF_Pj_z)[1] <- "mesh"
    
    refc_sum <- rowSums(Mat_REF_Pj_z[,2:dim(Mat_Pj_lognorm_z)[2]])
    if(cumul == "additif"){
      REFC <- cbind.data.frame(vec_zid, refc_sum)
    } else {
      sub <- Mat_REF_Pj_z[which(refc_sum > 0),]
      REFC <- cbind.data.frame(vec_zid, array(0., dim=nz))
      if(cumul == "antagoniste") REFC[which(refc_sum > 0),2] <- unlist(apply(as.matrix(c(1:dim(sub)[1])), 1, function(x) antagoniste(sub[x, which(sub[x,] > 0)])))
      if(cumul == "synergique") REFC[which(refc_sum > 0),2] <- unlist(apply(as.matrix(c(1:dim(sub)[1])), 1, function(x) synergique(sub[x, which(sub[x,] > 0)])))
    }
    REFC[,2] <- as.numeric(REFC[,2])
    REFC[,2] <- REFC[,2]/max(REFC[,2]) #On ram?ne le r?sultat entre 0 et 1
    out <- cbind.data.frame(out, REFC[,2]) #On stocke le risque d'effet calcul? dans la matrice de sortie et on passe ? la simulation suivante
    colnames(out)[k+1] <- paste("simul", k, sep="")
    colnames(out)[1] <- "mesh"
    # Export des simulations
    if(config$simul$export_simul==1) {
      name_simul <- paste("simul", n, tolower(chainchar), sep="_")
      export_table(con, config$simul$export_scheme, grid_z, out, name_simul)}
    print(paste("Fin de la simulation n?",k, sep=" "))
  }
  return(out)
}

# R?alisation des simulations
nsim <- config$simul$nbsimul # Nombre de simulations ? r?aliser
ptm1 <- proc.time() #Initialisation pour le temps de calcul
Test <- Simul(nsim)
printime(as.numeric((proc.time()-ptm1)[3])) #Affichage du temps de calcul une fois les simulations termin?es


# Calcul et export des statistiques sur les r?sultats
if(config$simul$export_result==1) { #Seulement si on a param?tr? l'option
  name_result <- paste("stat_simul", nsim, tolower(chainchar), sep="_") #Nom de la table qui sera export?e
  # Application de la fonction simul_calc pour identifier les cellules les plus ou les moins impact?es dans chaque simulation
  Stat <- simul_calc(Test, thres_stat)
  # Tableau allant contenir le r?sultat final
  simul_stat <- cbind.data.frame(vec_zid, matrix(data=0, nrow=nz, ncol=(2*length(thres_stat))))
  colnames(simul_stat)[1] <- c("mesh") 
  for(i in 1:length(thres_stat)){
    colnames(simul_stat)[2*i] <- paste("top", thres_stat[i], sep="") # Nom des colonnes
    colnames(simul_stat)[(2*i)+1] <- paste("bottom", thres_stat[i], sep="") #Nom des colonnes
  }
  for(i in 1:nz){ # Boucle sur toutes les mailles
    for(j in 1:length(thres_stat)){
      simul_stat[i,2*j] <- (sum(Stat[[j]][i,]==1)/nsim)*100 #On ram?ne en pourcentage le nombre de simulations ou la maille est dans les plus impact?es sur le nombre total de simulations
      simul_stat[i,(2*j)+1] <- (sum(Stat[[j]][i,]==-1)/nsim)*100 #Idem pour les moins impact?es
    }
  }
  
  # Calcul de la moyenne, de l'?cart type et du coefficient de variation
  statest <- stat_tab(Test)
  simul_stat <- cbind.data.frame(simul_stat, statest[,2:4]) #int?gration avec les autres statistiques de la table
  #export de la table dans la base de donn?es
  export_table(con, config$simul$export_scheme, grid_z, simul_stat, name_result)
}
