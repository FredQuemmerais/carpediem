# ANALYSE DU RISQUE D'EFFETS CUMULES DES ACTIVITES ANTHROPIQUES SUR LES HABITATS BENTHIQUES
# ETAPE 2 : REALISATION DE GRAPHIQUE, SYNTHESE STATISTIQUE PAR ZONE d'INTERET

# Projet        : code développé dans le cadre du projet CARPEDIEM (2016-2018)
# Date          : 2022/10/06
# Version code  : 1.0
# Version R     : R-3.5.1
# Auteurs       : Julien barrere
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
#       a renseigner aux lignes 64 et 66.
#     - Lecture et import des tables de donnees sources contenues dans une base de donnees PostgreSQL (locale et/ou distante).
#     - Export des resultats dans la base de donnees, dans de nouvelles tables, dans un schema a specifier.

# -----------------------------------------------------------------------------
# PRODUCTION DE GRAPHIQUES SPECIFIQUES DE CERTAINES ZONES
#              ETAPE 1 - LECTURE DU FICHIER DE PARAMETRAGE ET IMPORT DES DONNEES 
#              ETAPE 2 - PRODUCTION D'UNE MATRICE ACTIVITE PRESSION REDUITE AUX ACTIVITES ET PRESSIONS PRISES EN COMPTE DANS L'EVALUATION
#              ETAPE 3 - CREATION DES GRAPHES
# -----------------------------------------------------------------------------

rm(list=ls())


print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 1 - LECTURE DU FICHIER DE PARAMETRAGE ET IMPORT DES DONNEES                         ")
print("# ------------------------------------------------------------------------------------------")


# PARAMETRES DE CONFIGURATION POUVANT ETRE MODIFIES PAR TOUT UTILISATEUR ------
# reprises et sauvegardes
# parametrage
fileparam         <- "fichier_parametrage_refc_2022_source_fonctionnel.xlsx"    # fichier de parametrage Excel
# utilisateur
userdir           <- "D:/02_SIG/00_CODE_CARPEDIEM_BENTHOS_OPERATIONNEL_2022/"
# ------------------------------------------------------------------------------


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
library(RPostgreSQL)  # pour lire DB et transferer data.frames vers DB PostgreSQL
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


#-- pas encore reinstalle sous R.3.4.1
library(grDevices)


print("# ")
source('f_import.R')                # code source pour importer les parametres / tables de DB
source('f_lec_Postgres_DB.R')       # code source de fonctions de lecture de BD Postgre  
source('./Rpostgis/Rpostgis.R')     # code source Nicolas Lambert - modifie par AVB
source('f_map_plot.R')              # code source fonctions de plot des cartes
source('f_spatial_data_analysis.R') # Fonctions ouverture couches SIG, ...

print("# LECTURE DES PARAMETRES DU FICHIER EXCEL ET ECRITURE DANS LA LISTE config        ")
file_path <- paste(paramdir,"/",fileparam,sep="")         # path et nom du fichier Excel qui contient les parametres
config    <- f_importConfig(file_path)                    # lecture du fichier Excel de parametrage



# Unites et conversions 
# - surfaces exprimees en km2
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


# Date 
datesauv <- paste(substr(date(),21,24),substr(date(),5,7),substr(date(),9,10),sep="") #suffixe du fichier de sauvegarde



# - Enregistrement des numeros de colonne du feuillet tables_graph
i_Table_code <- which(colnames(config$tabgraph$Data)=="Table_code")
i_Var_code <- which(colnames(config$tabgraph$Data)=="Var_code")
i_DB_choice <- which(colnames(config$tabgraph$Data)=="DB_choice")
i_DB_scheme <- which(colnames(config$tabgraph$Data)=="DB_scheme")
i_DB_table <- which(colnames(config$tabgraph$Data)=="DB_table")
i_Data_name <- which(colnames(config$tabgraph$Data)=="Data_name")
i_Field <- which(colnames(config$tabgraph$Data)=="Field")
i_Codes <- which(colnames(config$tabgraph$Data) %in% paste("Code", c(1:10), sep=""))

# Connexion avec la base de donnees locale
con_local <- dbConnect(drv=config$DBConnectlocal$DBConnect_drv,
                 host=config$DBConnectlocal$DBConnect_host,
                 user=config$DBConnectlocal$DBConnect_user,
                 password=config$DBConnectlocal$DBConnect_password,
                 dbname=config$DBConnectlocal$DBConnect_dbname,
                 port=config$DBConnectlocal$DBConnect_port)

# Connexion avec la base de donnees distante
con_dist <- dbConnect(drv=config$DBConnectdist1$DBConnect_drv,
                 host=config$DBConnectdist1$DBConnect_host,
                 user=config$DBConnectdist1$DBConnect_user,
                 password=config$DBConnectdist1$DBConnect_password,
                 dbname=config$DBConnectdist1$DBConnect_dbname,
                 port=config$DBConnectdist1$DBConnect_port)

# Boucle d'import des tables specifiees dans le feuillet table_graph
codes <- unique(config$tabgraph$Data[,i_Table_code]) # Extraction des codes associes aux tables a importer
for(i in 1:length(codes)){ # Boucle sur toutes les tables
  i_code <- which(config$tabgraph$Data[,i_Table_code] == codes[i])[1] # Numero de ligne de la table
  tab_code <- codes[i] # code de la table
  print(paste("Import de la table", tab_code, sep = " "))
  if(config$tabgraph$Data[i_code, i_DB_choice] == "local") con <- con_local # Connexion avec la BD locale ou distante selon la colonne DB_choice
  if(config$tabgraph$Data[i_code, i_DB_choice] == "dist1") con <- con_dist
  # Import de la table
  tab <- dbReadSpatial(con, schemaname=config$tabgraph$Data[i_code, i_DB_scheme], tablename=config$tabgraph$Data[i_code, i_DB_table], geomcol="geom")
  eval(parse(text=paste(tab_code, " <- tab",sep="")))
}

# Nom de la colonne identifiants
IDmesh <- as.character(config$tabgraph$Data[which(config$tabgraph$Data[,i_Var_code]=="idmesh"), i_Field])

# - Creation d'une fonction qui peut extraire des tables activites ou pression uniquement les colonnes d'interet
# Trois variable d'entree: la table (avec champ geom), le nom de la colonne contenant l'identifiant des mailles et un sufixe commun aux colonnes a extraire (ex: "a_" pour les activites)
extr_tab <- function(tab, suf){
  n <- nchar(suf) #Nombre de caracteres du suffixe
  i_ID <- which(colnames(tab@data)==IDmesh) #Numero de colonne de l'identifiant des mailles
  out <- tab@data[,i_ID] #On initialise la table de sortie avec les identifiants de maille
  for(i in 1:dim(tab@data)[2]){ #Boucle sur toutes les colonnes de la table d'entree
    suf_tab <- substr(colnames(tab@data)[i], 1, n) #On extrait le debut du nom de la colonne (meme nb de caracteres que le suffixe)
    if(suf_tab==suf){ # Si cela correspond au suffixe
      out <- cbind.data.frame(out, tab@data[,i]) # On integre la colonne dans le tableau de sortie
      colnames(out)[dim(out)[2]] <- colnames(tab@data)[i] # On conserve le nom de colonne
    } 
  }
  colnames(out)[1] <- "mesh"
  return(out)
}

# - Extraction des colonnes d'interets pour la table activite
# Suffixe pour la table activite
suf_act <- as.character(config$tabgraph$Data[which(config$tabgraph$Data[,i_Var_code]=="activite"), i_Field])
# Application de la fonction extr_tab
A_zi <- extr_tab(activite, suf_act)

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
nz <- dim(A_zi)[1]
# Stockage des activites normalisees dans une matrice
A_zi_lognorm <- as.matrix(A_zi[,1])
for(i in 2:dim(A_zi)[2]){
  A_zi_lognorm <- cbind.data.frame(A_zi_lognorm,f_lognorm(A_zi[,i]))
  colnames(A_zi_lognorm)[i] <- paste(colnames(A_zi)[i], "_lognorm", sep="")
}
colnames(A_zi_lognorm)[1] <- "mesh"

# - Extraction des colonnes d'interets pour la table pression
# Suffixe pour la table pression
suf_pre <- as.character(config$tabgraph$Data[which(config$tabgraph$Data[,i_Var_code]=="pression"), i_Field])
# Application de la fonction extr_tab
Mat_Pj_z <- extr_tab(pression, suf_pre)

# - Extraction des colonnes d'interets pour la table effet
# Suffixe pour la table pression
suf_eff <- as.character(config$tabgraph$Data[which(config$tabgraph$Data[,i_Var_code]=="effet"), i_Field])
# Application de la fonction extr_tab
Mat_REF_PjE1_z <- extr_tab(effet, suf_eff)



# - Extraction des colonnes d'interets pour la table zones
# Numeros de ligne dans le feuillet table_graph des champs de la table zones a etudier
i_zones <- as.numeric(which(config$tabgraph$Data[,i_Table_code] == "zones"))
# Nombre de zones a etudie
n_zones <- length(i_zones)
# Vecteur contenant les champs de la table zones a etudier
vec_zones <- config$tabgraph$Data[i_zones, i_Field]
# Extraction des colonnes d'interets
sub_zones <- zones@data[,which(colnames(zones@data) %in% c(IDmesh, vec_zones))]
# Classement des lignes dans le meme ordre que les autres tables
ord <- cbind.data.frame(A_zi[,1], as.matrix(c(1:dim(A_zi)[1])))
colnames(ord) <- c("mesh", "order")
sub_zones <- merge(ord, sub_zones, by.x = "mesh", by.y = IDmesh, all.x = TRUE)
sub_zones <- sub_zones[order(sub_zones[,which(colnames(sub_zones)=="order")], decreasing = F),]
sub_zones <- sub_zones[,which(colnames(sub_zones) %in% c("mesh", vec_zones))]

# Extraction des colonnes d'interets de la table index
data_index <- index@data[,which(colnames(index@data) %in% c(IDmesh, "ima_option1", "ima_option2", "ipc_option1", "ipc_option2", "refc_e1"))]
data_index <- cbind.data.frame(data_index, confiance@data[,which(colnames(confiance@data) == "ic")])
colnames(data_index)[dim(data_index)[2]] <- "ic"
sub_data_index <- data_index[which(!is.na(data_index[,2])),] #table contenant uniquement les mailles pour les quelles le refc a ete calcule
# creation d'un autre tableau ou les variables discretes sont des facteurs
data_index_factor <- data_index 
data_index_factor[,3] <- as.factor(data_index_factor[,3])
data_index_factor[,5] <- as.factor(data_index_factor[,5])
sub_data_index_factor <- data_index_factor[which(!is.na(data_index_factor[,2])),]


# - Extraction des donnees habitats
if("habitats" %in% config$tabgraph$Data[,i_Table_code]){
  habitats_z <- habitats@data
  icol_hab <- which(colnames(habitats_z) == as.character(config$tabgraph$Data[which(config$tabgraph$Data[,i_Var_code]=="habitats"), i_Field]))
  icol_surf <- which(colnames(habitats_z) == as.character(config$tabgraph$Data[which(config$tabgraph$Data[,i_Var_code]=="surface"), i_Field]))
  icol_id <- which(colnames(habitats_z) == IDmesh)
  habitats_z <- as.data.frame(habitats_z[, c(icol_id, icol_hab, icol_surf)])
  colnames(habitats_z) <- c("mesh", "code", "surface")
  
  
  sub_habitats_z <- habitats_z[which(habitats_z[,1] %in% as.character(A_zi[,1])),]
  # Classement des lignes dans le meme ordre que les autres tables
  ord <- cbind.data.frame(A_zi[,1], as.matrix(c(1:dim(A_zi)[1])))
  colnames(ord) <- c("mesh", "position")
  sub_habitats_z <- merge(sub_habitats_z, ord, by.x = "mesh", by.y = "mesh", all.x = TRUE)
  
  
  
  Mat_hab_z <- as.matrix(A_zi[,1])
  list_hab <- config$tabgraph$Data[which(config$tabgraph$Data[,i_Var_code]=="habitats"), i_Codes] 
  list_hab <- as.character(list_hab[which(!is.na(list_hab))])
  
  
  for (i in 1:length(list_hab)){
    Mat_hab_z <- cbind.data.frame(Mat_hab_z, array(0, dim = nz))
    Mat_hab_z[,i+1] <- as.numeric(Mat_hab_z[,i+1])
    colnames(Mat_hab_z)[i+1] <- list_hab[i]
    Mat_hab_z[sub_habitats_z[which(sub_habitats_z[,2] == list_hab[i]), 4],i+1] <- sub_habitats_z[which(sub_habitats_z[,2] == list_hab[i]), 3]
  }
  
  colnames(Mat_hab_z)[1] <- "mesh"
  Mat_hab_z[,1] <- as.character(Mat_hab_z[,1])
}

print("# --------------------------------------------------------------------------------------------------------------------------")
print("# ETAPE 2 - PRODUCTION D'UNE MATRICE ACTIVITE PRESSION REDUITE AUX ACTIVITES ET PRESSIONS PRISES EN COMPTE DANS L'EVALUATION")
print("# --------------------------------------------------------------------------------------------------------------------------")

ni <- config$ni

# correspondance activites / donnees descriptives des activites / tables chargees

# - champs de la table contenue dans le feuillet 'Data'
j_Var_code    <- which(colnames(config$DBData$Data)=="Var_code")
j_Data_code    <- which(colnames(config$DBData$Data)=="Data_code")

nbcol <- length(grep("Code",colnames(config$DBData$Data)))  # nombre de colonnes de codes a parcourir

# creation du vecteur qui accueille le Numero de la ligne du tableau 'Data' qui decrit l'intensite de l'activite
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
        config$A_ilindata[i] <- vec_A_ilindata[which(config$DBData$Data[vec_A_ilindata,j_Var_code]=="intensity")]
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


##Boucle des pressions reduite pour obtenir uniquement le nom des pressions que l'on va integrer dans l'analyse

# reduction du nombre de pressions si agregation de plusieurs pressions
# - import de toutes les pressions et caracteristiques (presentes dans mat A/P)
nj <- config$nj
P_code <- config$P_code
P_name <- config$P_name
P_modcalc <- config$P_modcalc
P_evalDEM <- config$P_evalDEM

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
vec_controle_P <- array(0,dim=(nj))
vec_P_name_eval <- c()
vec_P_code_eval <- c()
for(j in 1:nj){
  id_Pcode_corres <- which(P_code==config$P_code[j])
  if(length(id_Pcode_corres)==0){   #pression agregee
    id_Pcode_corres <- which(P_code==config$P_code_ag[j])[1]
  }
  if(config$P_evalDEM[j]==1){  #reduction du nombre de pressions pour le demonstrateur
    
    if(!is.na(config$P_code_ag[j])){    # pression agregee
      # verification que la pression agregee n a pas deja ete calculee
      if(max(vec_controle_P[which(config$P_code_ag==config$P_code_ag[j])])==1){vec_controle_P[j] <- 1} ###Necessite de mettre vec_controle_P[j] <- 1 apres avoir realise le calcul sur la premiere pression agregee, sinon boucle inutile
      list_iP_ag <- config$P_code[which(config$P_code_ag==config$P_code_ag[j])]
      #title_nameP <- config$P_code_ag[j]
    }else{                               # pression non agregee
      list_iP_ag  <- config$P_code[j]
      #title_nameP <- config$P_code[j]      
    }
    if(vec_controle_P[j]==0){           # evitons de refaire le calcul pour pressions agregees deja calculees
      vec_P_name_eval <- c(vec_P_name_eval,P_name[id_Pcode_corres])
      vec_P_code_eval <- c(vec_P_code_eval,P_code[id_Pcode_corres])  
    } #boucle vec_controle_P
  } 
  if(length(list_iP_ag)>1) {vec_controle_P[j]<-1}
}
## - IMPORT DE LA MATRICE ACTIVITE PRESSION - ##
ideb_A <- 3
ifin_A <- length(config$Mat_AP[,1])
ideb_P <- 5  
ifin_P <- length(config$Mat_AP[1,])
vec_lines <- which(config$Mat_AP[,1]=="Final")
# - liste des activites dans la matrice
list_Acode_MatAP <- config$Mat_AP[vec_lines,2]
list_Aname_MatAP<-character(length=length(list_Acode_MatAP))
# - liste des pressions dans la matrice
list_Pcode_MatAP <- config$Mat_AP[1,ideb_P:ifin_P] 
list_Pname_MatAP <- config$Mat_AP[2,ideb_P:ifin_P] 
# Rassemblement du nom de l'activte et de la phase lorsqu'une phase est precisee.
for (i in 1:length(list_Aname_MatAP)) {
  if (is.na(config$Mat_AP[vec_lines[i],4])){
    list_Aname_MatAP[i]=config$Mat_AP[vec_lines[i],3]
  } else {
    list_Aname_MatAP[i]=paste(config$Mat_AP[vec_lines[i],3],config$Mat_AP[vec_lines[i],4],sep="_")
  }
}
vec_A_zi_names <- c()
for(i in 1:ni_ag){
  ilin <- config$A_ilindata[which(config$Aiag==Codes_i_ag[i])[1]]
  vec_A_zi_names <- c(vec_A_zi_names,config$DBData$Data[ilin,j_Data_code])
}

# -- creation de la matrice utilisee pour l'evaluation et indices des dimensions
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
      MatAP[i,j] <- NA # On met des NA au lieu de ND (que R ne sait pas interpreter)
    }
  }
}
MatAP <- as.data.frame(MatAP)
for(i in 1:dim(MatAP)[2]){
  MatAP[,i] <- as.numeric(as.character(MatAP[,i]))
}
# Double boucle permettant de reduire les matrices creees ci-dessus aux pressions et activites utilisees dans l'analyse
for(i in 2:dim(A_zi)[2]){
  iA_code <- toupper(colnames(A_zi)[i]) #extraction du code de l'activite
  list_iA_ag <- config$A_code[which(config$Aiag==iA_code)]  #
  n_act <- length(list_iA_ag)
  sub <- MatAP[which(rownames(MatAP) %in% list_iA_ag),]
  sub_final <- as.data.frame(matrix(data=0, nrow=1, ncol=length(vec_P_code_eval), dimnames = list(iA_code, vec_P_code_eval)))
  for(j in 1:length(vec_P_code_eval)){
    iP_code <- vec_P_code_eval[j]
    if(iP_code %in% config$P_code) {
      list_iP_ag <- P_code[which(P_code==iP_code)]
    } else {
      list_iP_ag <- config$P_code[which(config$P_code_ag==iP_code)]
    }
    if(length(which(is.na(unlist(sub[,which(colnames(sub) %in% list_iP_ag)]))))==0){
      sub_final[which(colnames(sub_final)==iP_code)] <- max(unlist(sub[,which(colnames(sub) %in% list_iP_ag)]), na.rm = T)
    } 
  }
  if(i==2){
    MatAP_sub <- sub_final
  } else {
    MatAP_sub <- rbind.data.frame(MatAP_sub, sub_final)
  }
}


print("# ------------------------------------------------------------------------------------------")
print("# ETAPE 3 - CREATION DES GRAPHES                                                            ")
print("# ------------------------------------------------------------------------------------------")

##                               ------                                    ##
## --- 1ere etape: Fonctions necessaires a la production des graphes   --- ##

## Dossier qui contiendra les graphiques
dirname <- paste("graphs", chainchar, datesauv, sep="_")
path_newdir <- paste(outdir,"graphs", dirname, sep = "/")
dir.create(path_newdir)

#ecriture d'une fonction permettant a partir d'un nombre x de categorie (nb_class) et d'un vecteur (vec) de diviser les valeurs du vacteur en x categorie (intervalles egaux) 
#La fonction renvoie un tableau contenant pour chaque intervalle le nombre total et le pourcentage d'elements du vecteur se trouvant dans cette categorie
prep_graph <- function (vec,nbclass) {
  out <- as.data.frame(matrix(data=0.,nrow=nbclass,ncol=2)) #on initialise le tableau de sortie
  inter <- diff(range(vec))/nbclass # Longueur de chaque intervalle
  for(i in 1:nbclass) { # Boucle sur toutes les classes
    inf_i <- min(vec)+(i-1)*inter # Borne inferieur de la classe i
    sup_i <- inf_i+inter # Borne superieur
    row.names(out)[i]=paste(as.character(round(inf_i,3)), as.character(round(sup_i,3)), sep=" - ") # Nom du rang = intervalle 
    for(j in 1:length(vec)){ # Boucle sur tous les elements du vecteur
      if (vec[j]>=inf_i && vec[j]<sup_i){ # Si l'element j est dans l'intervalle j
        out[i,1] <- out[i,1]+1 # On rajoute une unite dans la colonne "compte" pour cette classe
      }
    }
    out[i,2]=round((out[i,1]/length(vec)*100),3) # Conversion du compte en pourcentage
  }
  colnames(out) <- c("Compte","Pourcentage")
  Cat<-as.factor(row.names(out))
  out <- cbind(Cat,out) # Integration des intervalles dans le tableau de sortie
  row.names(out)<-c(1:nbclass)
  return(out)
}

# Fonction produisant un graphique de repartition des valeurs (variables discretes)
# Arguments d'entree: tableau(in.tab), numero de colonne(in.col), titre de l'axe des ordonnees(in.table)
f_hist_dis <- function(in.tab, in.col, in.title){
  out <- ggplot(in.tab, aes(x=in.tab[,in.col])) 
  out <- out +  geom_bar(aes(y = (..count..)/sum(..count..)*100), fill="#959ce5")
  out <- out + xlab(in.title) + ylab("Pourcentage (%)") 
  out <- out + theme_bw()+theme(axis.title = element_text(size=12), axis.text = element_text(size=12))
  out <- out + scale_x_continuous(breaks=c(0:max(in.tab[,in.col])))
  return(out)
}

# Fonction produisant un graphique de repartition des valeurs (variables continues)
# Arguments d'entree: tableau(in.tab), numero de colonne(in.col), nombre de categories(ncat), titre de l'axe des ordonnees(in.table)
f_hist_cont <- function(in.tab, in.col, ncat, in.title){
  plot <- prep_graph(in.tab[,in.col],ncat)
  out <- ggplot(data=plot, aes(x=Cat, y=Pourcentage))+geom_bar(stat="identity", width=0.6, fill="#959ce5")
  out <- out + theme_bw()+theme(axis.title = element_text(size=12), axis.text = element_text(size=12))
  out <- out + xlab(in.title) + ylab("Pourcentage (%)")
  return(out)
}

# Alternative a prep_graph pour comparer les histogrammes de zones specifique a l'ensemble de la grille
prep_graph_bis <- function (vec,nbclass) {
  out <- as.data.frame(matrix(data=0.,nrow=nbclass,ncol=2)) #on initialise le tableau de sortie
  inter <- 1/nbclass # Longueur de chaque intervalle
  for(i in 1:nbclass) { # Boucle sur toutes les classes
    inf_i <- (i-1)*inter # Borne inferieur de la classe i
    sup_i <- inf_i+inter # Borne superieur
    row.names(out)[i]=paste(as.character(round(inf_i,3)), as.character(round(sup_i,3)), sep=" - ") # Nom du rang = intervalle 
    for(j in 1:length(vec)){ # Boucle sur tous les elements du vecteur
      if (vec[j]>=inf_i && vec[j]<sup_i){ # Si l'element j est dans l'intervalle j
        out[i,1] <- out[i,1]+1 # On rajoute une unite dans la colonne "compte" pour cette classe
      }
    }
    out[i,2]=round((out[i,1]/length(vec)*100),3) # Conversion du compte en pourcentage
  }
  colnames(out) <- c("Compte","Pourcentage")
  Cat<-as.factor(row.names(out))
  out <- cbind(Cat,out) # Integration des intervalles dans le tableau de sortie
  row.names(out)<-c(1:nbclass)
  return(out)
}

# Alternative a f_hist_cont pour comparer les histogrammes de zones specifiques a l'ensemble de la grille
# histogrammes classes par classe de valeur IMA2, IPC2, REFC
f_hist_cont_bis <- function(in.tab, in.col, in.TAB, in.COL, ncat, in.title){
  plot <- prep_graph_bis(in.tab[,in.col],ncat)
  plot <- cbind.data.frame(plot, as.character(matrix(data = " zone etudiee ", ncol = 1, nrow = 5)))
  colnames(plot)[4] <- "Zone"
  PLOT <- prep_graph_bis(in.TAB[,in.COL],ncat)
  PLOT <- cbind.data.frame(PLOT, as.character(matrix(data = chainchar, ncol = 1, nrow = 5)))
  colnames(PLOT)[4] <- "Zone"
  plot_final <- rbind.data.frame(plot, PLOT)
  out <- ggplot(data=plot_final, aes(x=Cat, y=Pourcentage, fill = Zone))+geom_bar(stat="identity", width=0.7, position=position_dodge())
  out <- out + theme_bw()+theme(legend.position="bottom", legend.title = element_blank(), axis.title = element_text(size=12), axis.text = element_text(size=12))+ scale_fill_manual(values=c('#D62976','#feda75'))
  out <- out + xlab(in.title) + ylab("Pourcentage (%)") + ylim(0,100)
  return(out)
}

# Alternative a f_hist_dis pour comparer les histogrammes de zones specifiques a l'ensemble de la grille
# Histogrammes classes par categories IMA1 et IPC1 (1, 2,3...) zone totale en jaune, zone etudiee en bleu ok
f_hist_dis_bis <- function(in.tab, in.col, in.TAB, in.title){
  nmax <- as.numeric(max(in.TAB[,in.col], na.rm = T))
  out.tab <- as.data.frame(matrix(data=0.,nrow= 2*nmax+1, ncol = 3)) #on initialise le tableau de sortie
  out.tab[,3] <- as.character(out.tab[,3])
  colnames(out.tab) <- c("valeur", "Pourcentage", "Zone")
  for(i in 0:nmax){
    out.tab[(2*i+1):(2*i+2), 1] <- as.character(i)
    out.tab[(2*i+1), 2] <- (length(which(in.tab[,in.col]==i))/(dim(in.tab)[1]))*100 
    out.tab[(2*i+2), 2] <- (length(which(in.TAB[,in.col]==i))/(dim(in.TAB)[1]))*100 
    out.tab[(2*i+1), 3] <- " zone etudiee " #avant note zone reduite
    out.tab[(2*i+2), 3] <- chainchar
  }
  out <- ggplot(data=out.tab, aes(x=valeur, y=Pourcentage, fill = Zone))+geom_bar(stat="identity", width=0.7, position=position_dodge())
  out <- out + theme_bw()+theme(legend.position="bottom", legend.title = element_blank(), axis.title = element_text(size=12), axis.text = element_text(size=12))+ scale_fill_manual(values=c('#D62976','#feda75'))
  out <- out + xlab(in.title) + ylab("Pourcentage (%)") + scale_x_discrete(limits=as.character(c(0:nmax)))
  return(out)
}

# Fonction d'export des graphes
# Arguments d'entree: le graphique (in.graph), le dossier de sauvegarde (in.dir), le nom qu'on donne au graphe (in.namefile)
export_graph <- function(in.graph, in.dir, in.namefile){
  if (!file.exists(paste(in.dir, in.namefile, sep="/"))){
    ggsave(in.namefile, plot = in.graph, device = "jpeg" , path = in.dir, width=22.5, height=17, units=c("cm"))
    print(paste("Export du fichier-----",in.namefile))
  } else {
    print(paste("le fichier-----",in.namefile,"--------existe deja"))
  }
}

# Fonction produisant un boxplot d'une variable continue (y) en fonction d'une variable discrete (x)
# Arguments d'entree: tableau(in.tab), numero de colonne x (in.col.x), numero de colonne y (in.col.y), titre de l'axe des x (in.title.x), titre de l'axe des y (in.title.y)
f_boxplot <- function(in.tab, in.col.x, in.col.y, in.title.x, in.title.y){
  out <- ggplot(in.tab, aes(x=in.tab[,in.col.x], y=in.tab[,in.col.y])) + geom_boxplot(color="#2f367f", fill="#dbdef6")
  out <- out + xlab(in.title.x) + ylab(in.title.y)
  out <- out + theme_bw() + scale_fill_brewer(palette="Blues")
  out <- out + theme(axis.title = element_text(size=12), axis.text = element_text(size=12), legend.position='none')
  return(out)
}

# - Codage d'une fonction qui a partir des numeros de lignes qui nous interesse renvoie le graphe bilan des activites sur ces lignes uniquement
# Argument d'entree: les numeros de lignes des mailles sur lesquelles on souhaite travailler
Stat_Ai_plot <- function(lines){
  Stat <- as.data.frame(matrix(data = NA, nrow <- 3*(dim(A_zi)[2]-1), ncol <- 3)) # Matrice qui contiendra les statistiques permettant de faire les graphes
  Stat[,3] <- as.numeric(Stat[,3]) # Conversion en numerique de la colonne qui contiendra les pourcentages
  colnames(Stat) <- c("Activite","Statistique","Pourcentage")
  for (i in 1:(dim(A_zi)[2]-1)){ # Boucle sur les activites
    j <- 3*i - 2 # Compteur permettant de remplir 3 lignes du tableau par activite
    Stat[j,1] <- vec_A_zi_names[i] # Nom de l'activite
    Stat[j+1,1] <- vec_A_zi_names[i]
    Stat[j+2,1] <- vec_A_zi_names[i]
    Stat[j,2] <- " presence sur la zone " 
    Stat[j+1,2] <- " mailles impactees "
    Stat[j+2,2] <- " contribution au refc total "
    Stat[j,3] <- (length(which(A_zi[lines,i+1]>0))/length(lines))*100 #ratio du nombre de maille ou l'activite est positive (donc presente) sur le nombre de mailles dans la zone etudiee  
    Stat[j+1,3] <- (length(which(Contrib_Ai[lines,i+1]>0))/length(lines))*100 #ratio du nombre de maille ou la contribution de l'activite est positive (impact de l'activite) sur le nombre de mailles dans la zone d'etude. 
    Stat[j+2,3] <- (sum(Contrib_Ai[lines,i+1])/sum(Contrib_Ai[lines,2:dim(Contrib_Ai)[2]]))*100 #Somme des contributions dues a cette activite divisee par la somme de tous les risques d'effets.
  }
  #Creation du graphique
  out <- ggplot(Stat, aes(x=Activite, y=Pourcentage, fill=Statistique)) + geom_bar(stat="identity", position=position_dodge(), width = 0.7) + coord_flip()
  out <- out + scale_x_discrete(limits = Stat[which(Stat[,2]==" contribution au refc total ")[order(Stat[which(Stat[,2]==" contribution au refc total "),3])], 1])
  out <- out + scale_fill_manual(values=c("#4C6B4F","#56C46A","#A3E589"),  guide = guide_legend(reverse = TRUE))
  out <- out + theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(size=12), axis.title = element_text(size = 12), axis.text = element_text(size=12))
  return(out)
}

# - Codage d'une fonction qui a partir des numeros de lignes qui nous interesse renvoie le graphe bilan des pressions sur ces lignes uniquement
# Argument d'entree: les numeros de lignes des mailles sur lesquelles on souhaite travailler
Stat_Pj_plot <- function(lines){
  Stat <- as.data.frame(matrix(data = NA, nrow <- 3*(dim(Mat_Pj_z)[2]-1), ncol <- 3)) # Matrice qui contiendra les statistiques permettant de faire les graphes
  Stat[,3] <- as.numeric(Stat[,3])  # Conversion en numerique de la colonne qui contiendra les pourcentages
  colnames(Stat) <- c("Pression","Statistique","Pourcentage")
  for (i in 1:(dim(Mat_Pj_z)[2]-1)){ #Boucle sur les pressions
    name_Pj <- tolower(paste("REF_E1_",colnames(Mat_Pj_z)[i+1],"_lognorm",sep="")) #Colonne de la matrice Mat_REF_PjE1_z associee a la pression d'interet
    icol_REF <- which(colnames(Mat_REF_PjE1_z)==name_Pj)
    j <- 3*i - 2  # Compteur permettant de remplir 3 lignes du tableau par pression
    Stat[j,1] <- P_name[which(P_code==colnames(Mat_Pj_z)[i+1])] # Nom de la pression
    Stat[j+1,1] <- P_name[which(P_code==colnames(Mat_Pj_z)[i+1])]
    Stat[j+2,1] <- P_name[which(P_code==colnames(Mat_Pj_z)[i+1])]
    Stat[j,2] <- " presence sur la zone "
    Stat[j+1,2] <- " mailles impactees "
    Stat[j+2,2] <- " contribution au refc total "
    Stat[j,3] <- (length(which(Mat_Pj_z[lines,i+1]>0))/length(lines))*100 #ratio nb de mailles ou la pression est presente/ nb de mailles sur la zone d'etude
    Stat[j+1,3] <- (length(which(Mat_REF_PjE1_z[lines,icol_REF]>0))/length(lines))*100 #ratio nb de mailles ou REFC-Pj >0 (et donc ou la pression a un impact) / nb de mailles sur la zone d'etude
    Stat[j+2,3] <- (sum(Mat_REF_PjE1_z[lines,icol_REF], na.rm=T)/sum(Mat_REF_PjE1_z[lines,2:dim(Mat_REF_PjE1_z)[2]], na.rm=T))*100 #Somme des REFC_Pj sur somme des REFC
  }
  # Realisation du graphe
  out <- ggplot(Stat, aes(x=Pression, y=Pourcentage, fill=Statistique)) + geom_bar(stat="identity", position=position_dodge(), width = 0.7) + coord_flip()
  out <- out + scale_x_discrete(limits = Stat[which(Stat[,2]==" contribution au refc total ")[order(Stat[which(Stat[,2]==" contribution au refc total "),3])], 1])
  out <- out + scale_fill_manual(values=c("#842658","#DD308D","#E894C1"),  guide = guide_legend(reverse = TRUE))
  out <- out + theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(size=12), axis.title = element_text(size = 12), axis.text = element_text(size=12))
  return(out)
}

# Alternative pour n'afficher que le pourcentage de mailles dans lesquelles chaque activite est presente
# Cette fonction est utilisee lorsque toutes les mailles qui nous interessent ont ete exclues par le seuil
Stat_Ai_plot_bis <- function(lines){
  Stat <- as.data.frame(matrix(data = NA, nrow <- (dim(A_zi)[2]-1), ncol <- 2))
  Stat[,2] <- as.numeric(Stat[,2])
  colnames(Stat) <- c("Activite","Pourcentage")
  for (i in 1:(dim(A_zi)[2]-1)){
    Stat[i,1] <- vec_A_zi_names[i]
    Stat[i,2] <- (length(which(A_zi[lines,i+1]>0))/length(lines))*100 #ratio du nombre de maille ou l'activite est positive (donc presente) sur le nombre de mailles dans la zone etudiee  
  }
  #Creation du graphique
  out <- ggplot(Stat, aes(x=Activite, y=Pourcentage)) + geom_bar(stat="identity", color="black", fill="#6699FF",position=position_dodge(), width = 0.7) + coord_flip()
  out <- out + theme(axis.title = element_text(size = 12), axis.text = element_text(size=12)) + ylab("Presence (% de mailles)")
  return(out)
}

# Alternative pour n'afficher que le pourcentage de mailles dans lesquelles chaque pression est presente
# Cette fonction est utilisee lorsque toutes les mailles qui nous interessent ont ete exclues par le seuil
Stat_Pj_plot_bis <- function(lines){
  Stat <- as.data.frame(matrix(data = NA, nrow <- (dim(Mat_Pj_z)[2]-1), ncol <- 2)) # Matrice qui contiendra les statistiques permettant de faire les graphes
  Stat[,2] <- as.numeric(Stat[,2]) # Conversion au format numerique de la colonne qui contiendra les pourcentages
  colnames(Stat) <- c("Pression","Pourcentage")
  for (i in 1:(dim(Mat_Pj_z)[2]-1)){ #Boucle sur les pressions
    name_Pj <- tolower(paste("REF_E1_",colnames(Mat_Pj_z)[i+1],"_lognorm",sep="")) #Colonne de la matrice Mat_REF_PjE1_z associee a la pression d'interet
    icol_REF <- which(colnames(Mat_REF_PjE1_z)==name_Pj)
    Stat[i,1] <- P_name[which(P_code==colnames(Mat_Pj_z)[i+1])] # Nom de la pression
    Stat[i,2] <- (length(which(Mat_Pj_z[lines,i+1]>0))/length(lines))*100 #ratio nb de mailles ou la pression est presente/ nb de mailles sur la zone d'etude
  }
  # Realisation du graphe
  out <- ggplot(Stat, aes(x=Pression, y=Pourcentage)) + geom_bar(stat="identity", color="black", fill = "#33CC00", position=position_dodge(), width = 0.7) + coord_flip()
  out <- out + theme(axis.title = element_text(size = 12), axis.text = element_text(size=12)) + ylab("Presence (% de mailles)")
  return(out)
}

# Fonction pour produire un graphe de comparaison des differents indices entre les zones etudiees HISTOGRAMME FICHIER : INDEX_COMPARE
# cat = Vecteur contenant les categories a comparer
# numero de colonne de la table sub_zones contenant les categories
f_compare <- function(cat, k){
  categ_stat <- as.data.frame(matrix(data = NA, nrow <- 5*length(cat), ncol <- 4)) # Matrice qui contiendra les statistiques permettant de faire les graphes
  colnames(categ_stat) <- c("categorie","index", "moyenne", "sd")
  indexes <- c("IMA1","IMA2","IPC1","IPC2","REFC") # Vecteur qui permettra de remplir la colonne "index" de la table categ_stat
  for(i in 1:2) categ_stat[,i] <- as.character(categ_stat[,i]) # Conversion des 2 premieres colonnes au format caractere
  for(i in 3:4) categ_stat[,i] <- as.numeric(categ_stat[,i]) # Conversion des 2 dernieres colonnes au format numerique
  for(j in 1:length(cat)){ # Boucle sur l'ensemble des categories a comparer
    cat_i <- cat[j] # On extrait le nom de la categorie j
    i <- 5*j - 4 # compteur permettant de remplir 5 lignes par categorie (5 lignes car 5 indices)
    if(cat[1] == "IN") lines_i <- list_lines[[j]] # cas particulier des categories binaires (presence / absence) -> cf. 3eme sous-partie
    else lines_i <- which(sub_zones[,k]==cat_i) # Sinon identification des numeros de lignes des mailles de cette categorie
    for(h in 1:5) { # Remplissage des 2 premieres colonnes 
      categ_stat[(i+h-1), 1] <- cat_i # 1ere colonne = nom de la categorie
      categ_stat[(i+h-1), 2] <- indexes[h] # 2eme colonne = indice (ima1/2, ipc1/2, refc)
    }
    # Remplissage des deux dernieres colonnes
    categ_stat[i, 3] <- mean(data_index[lines_i, 3], na.rm = T) #ima1 moyenne
    categ_stat[i, 4] <- sd(data_index[lines_i, 3], na.rm = T) #ima1 sd
    categ_stat[i+1, 3] <- mean(data_index[lines_i, 4], na.rm = T) #ima2 moyenne
    categ_stat[i+1, 4] <- sd(data_index[lines_i, 4], na.rm = T) # ima2 sd
    categ_stat[i+2, 3] <- mean(data_index[lines_i, 5], na.rm = T) #ipc1 moyenne
    categ_stat[i+2, 4] <- sd(data_index[lines_i, 5], na.rm = T) # ipc1 sd
    categ_stat[i+3, 3] <- mean(data_index[lines_i, 6], na.rm = T) #ipc2 moyenne
    categ_stat[i+3, 4] <- sd(data_index[lines_i, 6], na.rm = T) # ipc2 sd
    categ_stat[i+4, 3] <- mean(data_index[lines_i, 2], na.rm = T) #refc moyenne
    categ_stat[i+4, 4] <- sd(data_index[lines_i, 2], na.rm = T) # refc sd
  }
  # Realisation du graphe
  out <- ggplot(data=categ_stat, aes(x=categorie, y=moyenne, fill=index))+geom_bar(stat="identity", width=0.5)+ facet_grid(index~., scale="free")
  out <- out + theme(legend.position = 'none', strip.text = element_text(size=12, face = "bold"), axis.title = element_blank(), axis.text.x = element_text(size=12, angle=70, hjust=1), axis.text.y = element_text(size=12))
  out <- out + scale_fill_manual(values=c("#feda75","#fa7e1e","#d62976","#962fbf","#4F5BD5"))
  out <- out + geom_errorbar(aes(ymin=moyenne-sd, ymax=moyenne+sd), width=.2, position=position_dodge(0.9))
  return(out) 
}

# Autre fonction permettant de realiser un graphe du REFC vs IPC2 categorise NUAGE DE POINTS REFC VS IPC2 NOMME REFC_IPC2
# Arguments d'entree: le tableau data_index (tab) et la colonne contenant les categories (col)
f_compare2 <- function(tab, col, cat){
  tab_plot <- cbind.data.frame(tab, col) # rassemblement du tableau de donnees et de la colonne categories dans un meme tableau
  tab_plot <- tab_plot[which(col %in% cat),] # retrait des lignes pour lesquelles la categorie n'est pas specifiee
  tab_plot <- tab_plot[which(!is.na(tab_plot[,2])),] # retrait des lignes ou le refc n'a pas ete calcule a cause du seuil
  # Realisation du graphe
  out_plot <- ggplot(tab_plot, aes(x=tab_plot[,6], y=tab_plot[,2], colour = tab_plot[,dim(tab_plot)[2]]))+geom_point()+ scale_colour_viridis_d(begin=0.1, end= 1, direction=1, option="C")
  out_plot <- out_plot + xlab("IPC2") + ylab("REFC")
  out_plot <- out_plot + theme_gray() 
  out_plot <- out_plot + theme(axis.title = element_text(size=12), axis.text = element_text(size=12), legend.text = element_text(size=12), legend.title = element_blank(), legend.position = "bottom")
  return(out_plot)
}

# - Fonction permettant de produire un graphique sur les habitats precises dans le fichier de parametrage
plot_hab <- function(lines){
  out.data_index <- data_index[lines,]
  out.Mat_hab_z <- Mat_hab_z[lines,]
  out.na <- which(!is.na(out.data_index[,2]))
  out.Stat_hab <- as.data.frame(matrix(data = NA, nrow <- 3*(dim(out.Mat_hab_z)[2]-1), ncol <- 3))
  out.Stat_hab[,3] <- as.numeric(out.Stat_hab[,3])
  colnames(out.Stat_hab) <- c("Habitat","Statistique","Pourcentage")
  for (i in 1:(dim(out.Mat_hab_z)[2]-1)){
    j <- 3*i - 2
    out.Stat_hab[j,1] <- colnames(out.Mat_hab_z)[i+1]
    out.Stat_hab[j+1,1] <- colnames(out.Mat_hab_z)[i+1]
    out.Stat_hab[j+2,1] <- colnames(out.Mat_hab_z)[i+1]
    out.Stat_hab[j,2] <- "Presence (% de mailles)"
    out.Stat_hab[j+1,2] <- "Moyenne de la Surface"
    out.Stat_hab[j+2,2] <- "Risque d'effet moyen"
    out.Stat_hab[j,3] <- (length(which(out.Mat_hab_z[out.na,i+1] > 0))/length(out.na))*100 
    out.Stat_hab[j+1,3] <- mean(out.Mat_hab_z[which(!is.na(out.data_index[,2]) & out.Mat_hab_z[,i+1]>0), i+1]) 
    out.Stat_hab[j+2,3] <- (sum(out.Mat_hab_z[out.na, i+1]*out.data_index[out.na, 2]) / sum(out.Mat_hab_z[out.na, i+1]))*100 
  }
  
  
  out.hab_plot <- ggplot(out.Stat_hab, aes(x=Habitat, y=Pourcentage, fill=Statistique)) + geom_bar(stat="identity", color="black", position=position_dodge(), width = 0.7) + coord_flip()
  out.hab_plot <- out.hab_plot + scale_x_discrete(limits = out.Stat_hab[which(out.Stat_hab[,2]=="Risque d'effet moyen")[order(out.Stat_hab[which(out.Stat_hab[,2]=="Risque d'effet moyen"),3])], 1])
  out.hab_plot <- out.hab_plot + scale_fill_manual(values=c("#FF99FF", "#CC33CC", "#FF0033"),  guide = guide_legend(reverse = TRUE))
  out.hab_plot <- out.hab_plot + theme(legend.position = c(0.8, 0.1), legend.title = element_blank(), legend.text = element_text(size=15), axis.title = element_text(size = 15), axis.text = element_text(size=12), legend.background = element_rect(size=0.5, linetype="solid",colour ="black"))
  return(out.hab_plot)
}

# Fonction permettant de realiser tous les graphes caracteristiques d'une zone (histogrammes, boxplots, bilan activites / pressions etc.)
# Arguments d'entree: dataframe contenant les indices (tab), lignees de ce dataframe contenant les mailles de la zone d'interet (lines), dossier de sauvegarde du dossier (dir)
graph_maker <- function(tab, lines, dir){
  dir.create(dir) # Creation du dossier de sortie si il n'existe pas
  sub_tab <- tab[lines,] # Dataframe reduit aux lignes d'interets
  sub_lines <- lines[which(!is.na(sub_tab[,2]))] # numeros de ligne des mailles de la zone d'interet non exclues par le seuil 
  # Creation d'un dataframe identique a tab sauf que ima1 et ipc1 sont des facteurs --> permet de faire des boxplots
  tab_factor <- tab 
  tab_factor[,3] <- as.factor(tab_factor[,3])
  tab_factor[,5] <- as.factor(tab_factor[,5])
  sub_tab_factor <- tab_factor[lines,]
  print(paste(length(sub_lines), "/", length(lines), "mailles avec REFC non nul", sep=" ")) # Affichage de la proportion de mailles non exclues par le seuil dans cette dans la categorie
  out.title <- paste("graphique pour", length(sub_lines), "/", length(lines), "mailles avec REFC non nul", sep=" ") # Titre de graphique incluant cette proportion
  # - Histogrammes de repartion des donnees
  # IMA1
  plot1 <- f_hist_dis(sub_tab, 3, "IMA1")
  export_graph(plot1, dir, "Histogramme_IMA1.jpeg")
  # IMA2
  if(max(sub_tab[,4]) > 0){
    plot2 <- f_hist_cont(sub_tab, 4, 5, "IMA2")
    export_graph(plot2, dir, "Histogramme_IMA2.jpeg")
  } else {print("Tous les IMA2 sont nuls -> pas d'export du graphe Histogramme_IMA2.jpeg")}  
  # IPC1
  plot3 <- f_hist_dis(sub_tab, 5, "IPC1")
  export_graph(plot3, dir, "Histogramme_IPC1.jpeg")
  # IPC2
  if(max(sub_tab[,6]) > 0){
    plot4 <- f_hist_cont(sub_tab, 6, 5, "IPC2")
    export_graph(plot4, dir, "Histogramme_IPC2.jpeg")
  } else {print("Tous les IPC2 sont nuls -> pas d'export du graphe Histogramme_IPC2.jpeg")}  
  
  
  
  
  # - Boxplots des relations entre indices
  # IMA1 vs IMA2
  plot6 <- f_boxplot(sub_tab_factor, 3, 4, "IMA1 : nombre d'activites", "IMA2 : somme de l'intensite des activites")
  export_graph(plot6, dir, "Boxplot_IMA1_IMA2.jpeg")
  # IPC1 vs IPC2
  plot7 <- f_boxplot(sub_tab_factor, 5, 6, "IPC1 : nombre de pressions", "IPC2 : somme de l'intensite des pressions")
  export_graph(plot7, dir, "Boxplot_IPC1_IPC2.jpeg")
  
  if(length(sub_lines) > 5){ # Dans le cas ou la categorie comprend des mailles non exclues par le seuil, on produit aussi les graphiques dependant du REFC sur ces mailles
    # histogramme REfC
    if(max(tab[sub_lines, 2]) > 0){
      plot5 <- f_hist_cont(tab[sub_lines,], 2, 5, "REFC") + ggtitle(out.title) # Ajout d'un titre avec le nombre de mailles non exclues
      export_graph(plot5, dir, "Histogramme_REFC.jpeg")
    } else {print("Tous les REFC sont nuls -> pas d'export du graphe Histogramme_REFC.jpeg")}  
    # IMA1 vs REFC
    plot8 <- f_boxplot(tab_factor[sub_lines,], 3, 2, "IMA1 : nombre d'activites", "REFC") + ggtitle(out.title)
    export_graph(plot8, dir, "Boxplot_IMA1_REFC.jpeg")
    # IMA1 vs IC
    plot8bis <- f_boxplot(tab_factor[sub_lines,], 3, 7, "IMA1 : nombre d'activites", "IC") + ggtitle(out.title)
    export_graph(plot8bis, dir, "Boxplot_IMA1_IC.jpeg")
    # IPC1 vs REFC
    plot9 <- f_boxplot(tab_factor[sub_lines,], 5, 2, "IPC1 : nombre de pressions", "REFC") + ggtitle(out.title)
    export_graph(plot9, dir, "Boxplot_IPC1_REFC.jpeg")
    # IPC1 vs IC
    plot9 <- f_boxplot(tab_factor[sub_lines,], 5, 7, "IPC1 : nombre de pressions", "IC") + ggtitle(out.title)
    export_graph(plot9, dir, "Boxplot_IPC1_IC.jpeg")
    
    # - Nuage de point REFC en fonction de l'IPC2 
    plot10 <- ggplot(tab[sub_lines,], aes(x=tab[sub_lines,6], y=tab[sub_lines,2]))+geom_point()
    plot10 <- plot10 + xlab("IPC2") + ylab("REFC")
    plot10 <- plot10 + theme_bw() + ggtitle(out.title)
    plot10 <- plot10 + theme(axis.title = element_text(size=12), axis.text = element_text(size=12))
    export_graph(plot10, dir, "REFC_IPC2.jpeg")
    
    # - Nuage de point REFC en fonction de l'IC 
    plot10bis <- ggplot(tab[sub_lines,], aes(x=tab[sub_lines,7], y=tab[sub_lines,2]))+geom_point()
    plot10bis <- plot10bis + xlab("IC") + ylab("REFC")
    plot10bis <- plot10bis + theme_bw() + ggtitle(out.title)
    plot10bis <- plot10bis + theme(axis.title = element_text(size=12), axis.text = element_text(size=12), legend.position='none')
    export_graph(plot10bis, dir, "REFC_IC.jpeg")
    
    # - Graphiques bilan
    # Bilan activites
    plot11 <- Stat_Ai_plot(sub_lines)
    export_graph(plot11, dir, "Bilan_activites.jpeg")
    write.csv(as.data.frame(plot11$data), paste(dir, "Bilan_activites.csv", sep = "/"))
    # Bilan pressions
    plot12 <- Stat_Pj_plot(sub_lines)
    export_graph(plot12, dir, "Bilan_pressions.jpeg")
    write.csv(as.data.frame(plot12$data), paste(dir, "Bilan_pressions.csv", sep = "/"))
    
    # - Graphiques habitats
    if("habitats" %in% config$tabgraph$Data[,i_Table_code]){
      plot13 <- plot_hab(lines) + ggtitle(out.title)
      export_graph(plot13, dir, "Graphe_habitats.jpeg")
    }
  }else{ # Si toutes les mailles sont exclues par le seuil, on affiche seulement les differentes activites et pressions
    # - Graphiques bilan
    # Bilan activites
    plot11 <- Stat_Ai_plot_bis(lines)
    export_graph(plot11, dir, "Bilan_activites.jpeg")
    write.csv(as.data.frame(plot11$data), paste(dir, "Bilan_activites.csv", sep = "/"))
    # Bilan pressions
    plot12 <- Stat_Pj_plot_bis(lines)
    export_graph(plot12, dir, "Bilan_pressions.jpeg")
    write.csv(as.data.frame(plot12$data), paste(dir, "Bilan_pressions.csv", sep = "/"))
  }
}

# Alternative a graph maker pour prendre en compte des zones specifiques qui seront comparees a l'ensemble de la grille pour avoir un referentiel
graph_maker_bis <- function(tab, lines, dir){
  dir.create(dir) # Creation du dossier de sortie si il n'existe pas
  sub_tab <- tab[lines,] # Dataframe reduit aux lignes d'interets
  sub_lines <- lines[which(!is.na(sub_tab[,2]))] # numeros de ligne des mailles de la zone d'interet non exclues par le seuil 
  # Creation d'un dataframe identique a tab sauf que ima1 et ipc1 sont des facteurs --> permet de faire des boxplots
  tab_factor <- tab 
  tab_factor[,3] <- as.factor(tab_factor[,3])
  tab_factor[,5] <- as.factor(tab_factor[,5])
  sub_tab_factor <- tab_factor[lines,]
  print(paste(length(sub_lines), "/", length(lines), "mailles avec REFC non nul", sep=" ")) # Affichage de la proportion de mailles non exclues par le seuil dans cette dans la categorie
  out.title <- paste("graphique pour", length(sub_lines), "/", length(lines), "mailles avec REFC non nul", sep=" ") # Titre de graphique incluant cette proportion
  # - Histogrammes de repartion des donnees
  # IMA1
  plot1 <- f_hist_dis_bis(sub_tab, 3, tab, "IMA1")
  export_graph(plot1, dir, "Histogramme_IMA1.jpeg")
  # IMA2
  if(max(sub_tab[,4]) > 0){
    plot2 <- f_hist_cont_bis(sub_tab, 4, tab, 4, 5, "IMA2")
    export_graph(plot2, dir, "Histogramme_IMA2.jpeg")
  } else {print("Tous les IMA2 sont nuls -> pas d'export du graphe Histogramme_IMA2.jpeg")}  
  # IPC1
  plot3 <- f_hist_dis_bis(sub_tab, 5, tab, "IPC1")
  export_graph(plot3, dir, "Histogramme_IPC1.jpeg")
  # IPC2
  if(max(sub_tab[,6]) > 0){
    plot4 <- f_hist_cont_bis(sub_tab, 6, tab, 6, 5, "IPC2")
    export_graph(plot4, dir, "Histogramme_IPC2.jpeg")
  } else {print("Tous les IPC2 sont nuls -> pas d'export du graphe Histogramme_IPC2.jpeg")}  
  
  
  
  
  # - Boxplots des relations entre indices
  # IMA1 vs IMA2
  plot6 <- f_boxplot(sub_tab_factor, 3, 4, "IMA1 : nombre d'activites", "IMA2 : somme de l'intensite des activites")
  export_graph(plot6, dir, "Boxplot_IMA1_IMA2.jpeg")
  # IPC1 vs IPC2
  plot7 <- f_boxplot(sub_tab_factor, 5, 6, "IPC1 : nombre de pressions", "IPC2 : somme de l'intensite des pressions")
  export_graph(plot7, dir, "Boxplot_IPC1_IPC2.jpeg")
  
  if(length(sub_lines) > 5){ # Dans le cas ou la categorie comprend des mailles non exclues par le seuil, on produit aussi les graphiques dependant du REFC sur ces mailles
    # histogramme REfC
    if(max(tab[sub_lines, 2]) > 0){
      plot5 <- f_hist_cont_bis(tab[sub_lines,], 2, tab[which(!is.na(tab[,2])),], 2, 5, "REFC") + ggtitle(out.title) # Ajout d'un titre avec le nombre de mailles non exclues
      export_graph(plot5, dir, "Histogramme_REFC.jpeg")
    } else {print("Tous les REFC sont nuls -> pas d'export du graphe Histogramme_REFC.jpeg")}  
    # IMA1 vs REFC
    plot8 <- f_boxplot(tab_factor[sub_lines,], 3, 2, "IMA1 : nombre d'activites", "REFC") + ggtitle(out.title)
    export_graph(plot8, dir, "Boxplot_IMA1_REFC.jpeg")
    # IMA1 vs IC
    plot8bis <- f_boxplot(tab_factor[sub_lines,], 3, 7, "IMA1 : nombre d'activites", "IC") + ggtitle(out.title)
    export_graph(plot8bis, dir, "Boxplot_IMA1_IC.jpeg")
    # IPC1 vs REFC
    plot9 <- f_boxplot(tab_factor[sub_lines,], 5, 2, "IPC1 : nombre de pressions", "REFC") + ggtitle(out.title)
    export_graph(plot9, dir, "Boxplot_IPC1_REFC.jpeg")
    # IPC1 vs IC
    plot9 <- f_boxplot(tab_factor[sub_lines,], 5, 7, "IPC1 : nombre de pressions", "IC") + ggtitle(out.title)
    export_graph(plot9, dir, "Boxplot_IPC1_IC.jpeg")
    
    # - Nuage de point REFC en fonction de l'IPC2 
    plot10 <- ggplot(tab[sub_lines,], aes(x=tab[sub_lines,6], y=tab[sub_lines,2]))+geom_point()
    plot10 <- plot10 + xlab("IPC2") + ylab("REFC")
    plot10 <- plot10 + theme_bw() + ggtitle(out.title)
    plot10 <- plot10 + theme(axis.title = element_text(size=12), axis.text = element_text(size=12), legend.position='none')
    export_graph(plot10, dir, "REFC_IPC2.jpeg")
    
    # - Nuage de point REFC en fonction de l'IC 
    plot10bis <- ggplot(tab[sub_lines,], aes(x=tab[sub_lines,7], y=tab[sub_lines,2]))+geom_point()
    plot10bis <- plot10bis + xlab("IC") + ylab("REFC")
    plot10bis <- plot10bis + theme_bw() + ggtitle(out.title)
    plot10bis <- plot10bis + theme(axis.title = element_text(size=12), axis.text = element_text(size=12), legend.position='none')
    export_graph(plot10bis, dir, "REFC_IC.jpeg")
    
    # - Graphiques bilan
    # Bilan activites
    plot11 <- Stat_Ai_plot(sub_lines)
    export_graph(plot11, dir, "Bilan_activites.jpeg")
    write.csv(as.data.frame(plot11$data), paste(dir, "Bilan_activites.csv", sep = "/"))
    # Bilan pressions
    plot12 <- Stat_Pj_plot(sub_lines)
    export_graph(plot12, dir, "Bilan_pressions.jpeg")
    write.csv(as.data.frame(plot12$data), paste(dir, "Bilan_pressions.csv", sep = "/"))
    
    
    # - Graphiques habitats
    if("habitats" %in% config$tabgraph$Data[,i_Table_code]){
      plot13 <- plot_hab(lines) + ggtitle(out.title)
      export_graph(plot13, dir, "Graphe_habitats.jpeg")
    }
  }else{ # Si toutes les mailles sont exclues par le seuil, on affiche seulement les differentes activites et pressions
    # - Graphiques bilan
    # Bilan activites
    plot11 <- Stat_Ai_plot_bis(lines)
    export_graph(plot11, dir, "Bilan_activites.jpeg")
    write.csv(as.data.frame(plot11$data), paste(dir, "Bilan_activites.csv", sep = "/"))
    # Bilan pressions
    plot12 <- Stat_Pj_plot_bis(lines)
    export_graph(plot12, dir, "Bilan_pressions.jpeg")
    write.csv(as.data.frame(plot12$data), paste(dir, "Bilan_pressions.csv", sep = "/"))
  }
}

##                      ------                         ##
## --- 2eme sous-etape : preparation des donnees   --- ##

# CONSTRUCTION D'UNE MATRICE INDIQUANT LA CONTRIBUTION DE CHAQUE ACTIVITE AU RISQUE D'EFFET TOTAL
colnames(Mat_Pj_z)[2:dim(Mat_Pj_z)[2]] <- colnames(MatAP_sub)
Contrib_Ai <- cbind.data.frame(as.matrix(A_zi[,1]), matrix(data=0, nrow=dim(A_zi)[1], ncol=dim(A_zi)[2]-1))
colnames(Contrib_Ai) <- c("mesh", toupper(colnames(A_zi)[2:dim(A_zi)[2]]))
Contrib_Ai[,1] <- as.character(Contrib_Ai[,1]) #Colonne contenant l'identifiant des mailles
for(i in 2:dim(Contrib_Ai)[2]){ #Boucle sur les activites
  Contrib_Ai[,i] <- as.numeric(as.character(Contrib_Ai[,i]))
  name_Ai <- colnames(Contrib_Ai[i])
  for(j in 2:dim(Mat_Pj_z)[2]){ #Boucle sur les pressions
    name_Pj <- colnames(Mat_Pj_z)[j]
    if(name_Pj %in% config$P_code) {id_calc <- which(config$P_code==name_Pj)}
    if(name_Pj %in% config$P_code_ag) {id_calc <- which(config$P_code_ag==name_Pj)[1]}
    if(config$P_modcalc[id_calc]==1){ 
      if (MatAP_sub[which(rownames(MatAP_sub)==name_Ai), which(colnames(MatAP_sub)==name_Pj)]==1){ #Si aucune valeur positive dans la maille, a priori pas de lien activite pression et donc contribution au risque d'effet nulle
        active <- as.numeric(which(Mat_Pj_z[,j]>0))
        PjAi <- array(0,dim=c(nz,1))
        PjAi[active] <- A_zi_lognorm[active,i]/Mat_Pj_z[active,j] #Le ratio Ai/Pj indique le pourcentage du REFC_Pj attribuable a Ai
        PjAi <- PjAi*as.numeric(as.character(Mat_REF_PjE1_z[,j])) #On multiplie par REFC_Pj pour avoir la contribution reelle
        PjAi[which(is.na(PjAi))] <- 0
        Contrib_Ai[,i] <- Contrib_Ai[,i]+PjAi #On somme ces contributions a chaque REFCPj pour avoir la part du REFC_total attribuable a Ai
      }
    }
  }
}




##                       ------                       ##
## --- 3eme sous-etape : production des graphes   --- ##

## - Graphique a l'echelle de toutes les mailles
graph_maker(data_index, c(1:dim(data_index)[1]), paste(path_newdir, "1_analyse_globale", sep = "/"))


## - Graphique a l'echelle de zones specifiques
# Boucle sur les types de zones
for(i in 1:n_zones){
  zone_i <- vec_zones[i] # champs associe a la zone i (ex: amp_pnm)
  Code_i <- config$tabgraph$Data[i_zones[i], i_Var_code] # Code de la zone i (ex: pnm)
  Name_i <- config$tabgraph$Data[i_zones[i], i_Data_name] # Nom de la zone i (ex: Parcs Naturels Marins)
  print("-------------------------")
  print(paste("CREATION DE GRAPHIQUES PAR ZONES - ", Name_i, sep = ""))
  print("-------------------------")
  newdir <- paste(path_newdir, Code_i, sep="/") # nouveau dossier dans lequel sera sauvegarde les graphes de cette zone
  field_i <- sub_zones[,which(colnames(sub_zones)==config$tabgraph$Data[i_zones[i], i_Field])] #numero de colonne du tableau sub_zones associe a la zone i
  dir.create(newdir) # Creation du nouveau dossier
  # Cas de figure ou on divise l'ensemble des mailles en deux categories (selon presence d'une valeur dans le champ d'interet)
  if(config$tabgraph$Data[i_zones[i], i_Codes[1]] == "BIN"){ 
    print("---- categories prises en compte - option binaire")
    list_lines <- list() # Stockage des numeros de lignes des deux categories dans une liste a deux elements
    list_lines[[1]] <- as.numeric(which(!is.na(field_i)))
    list_lines[[2]] <- as.numeric(which(is.na(field_i)))
    categories <- c("IN", "OUT")
    field_i[list_lines[[1]]] <- "IN"
    field_i[list_lines[[2]]] <- "OUT"
    for(t in 1:2) { # Boucle sur les deux categories pour la creation des graphes
      print(paste("-- Production de graphes pour --> ", Name_i, " = ", categories[t], sep=""))
      graph_maker_bis(data_index, list_lines[[t]], paste(newdir, categories[t], sep="/"))
    }
  }
  else {
    # Cas de figure ou l'on s'interesse a toutes les categories 
    if(config$tabgraph$Data[i_zones[i], i_Codes[1]] == "ALL"){
      print("---- Toutes les categories prises en compte")
      categories <- unique(field_i[which(!is.na(field_i))])} # Extraction de toutes les categories associees a la zone
    # Cas de figure ou l'on s'interesse a quelques categories en particulier
    else {categories <- config$tabgraph$Data[i_zones[i], i_Codes] # Extraction de categories specifiees a partir du feuillet tables_graph
          categories <- categories[which(!is.na(categories))]
          print(paste("---- ", length(categories), " categories prises en compte", sep=""))
    }
    for(j in 1:length(categories)){ # Boucle sur les categories prises en compte
      cat_j <- categories[j]
      print(paste("-- Production de graphes pour --> ", Name_i, " = ", cat_j, sep=""))
      lines_j <- as.numeric(which(field_i == cat_j)) # extraction des numeros de ligne associes a cette categorie
      if(length(lines_j) > 10) graph_maker_bis(data_index, lines_j, paste(newdir, cat_j, sep="/")) # Application du graph_maker sur ces numeros de ligne.
      else print("Nombre de mailles concernees < 10 --> insuffisant pour la production de graphiques")
    }
    
  }
  # Production et export dans le meme dossier du graphe comparatif entre les differentes categories de la zone i
  compare_plot <- f_compare(categories, which(colnames(sub_zones)==config$tabgraph$Data[i_zones[i], i_Field])) + ggtitle(Name_i)
  export_graph(compare_plot, newdir, "index_compare.jpeg")
  refc_ipc2_cat <- f_compare2(data_index, field_i, categories)
  export_graph(refc_ipc2_cat, newdir, "REFC_IPC2.jpeg")
}



