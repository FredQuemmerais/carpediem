# import_param.R                                                      
# IMPORT DES FICHIERS DE PARAMETRES EXCEL
# contient les fonctions :                                         
#  - f_importConfig
#--------------------------------------------------------------------

f_importConfig <- function(path) {
  
  #testFonction : path <- file_path
  
  require(readODS)
  #lecture du fichier Excel
  
  out <- list()
  
  #feuillet Utilisateur ---------------------------------------------
  print("import_param.R - lecture feuillet : configuration")
  
  res <- as.matrix(read_ods(path, sheet="configuration",col_names=FALSE))
  #on retire des champs ? lire les espaces et les quotes (ATTENTION : aucun espace dans aucun des champs)
  res[,1:2] <- apply(res[,1:2],2,function(x) gsub(" ","",x))
  res[,1:2] <- apply(res[,1:2],2,function(x) gsub("\"","",x))
  
  indexBEG <- match(c("*****zone_de_travail",
                      "*****export_sig_resultats","*****gestion_sauvegarde_rdata_export_bd_sig","*****gestion_donnees_habitats"),res[,1])
  
  indexEND <- which(res[,1]%in%"*****")
  
  #Elements 'TimeSpace'
  #tstep
  index <- match("config_SRM",res[,1])
  out$TimeSpace$config_SRM <- as(as.vector(res[index,2]),res[index,3])
  
  
  #Elements 'Outputs'
  index <- match("lplot_sensibC_P",res[,1])
  out$Outputs$lplot_sensibC_P <- f_Int_to_Log(as(as.vector(res[index,2]),res[index,3]))
  index <- match("lplot_Ai",res[,1])
  out$Outputs$lplot_Ai <- f_Int_to_Log(as(as.vector(res[index,2]),res[index,3]))
  index <- match("lplot_PjAi",res[,1])
  out$Outputs$lplot_PjAi <- f_Int_to_Log(as(as.vector(res[index,2]),res[index,3]))
  index <- match("lplot_Pj",res[,1])
  out$Outputs$lplot_Pj <- f_Int_to_Log(as(as.vector(res[index,2]),res[index,3]))
  index <- match("lplot_REX_Pj",res[,1])
  out$Outputs$lplot_REX_Pj <- f_Int_to_Log(as(as.vector(res[index,2]),res[index,3]))
  index <- match("lplot_REF_Pj",res[,1])
  out$Outputs$lplot_REF_Pj <- f_Int_to_Log(as(as.vector(res[index,2]),res[index,3]))  
  
  
  #Elements 'SauvRep'
  #lrepr_DBtables
  index <- match("lrepr_DBtables",res[,1])
  out$SauvRep$lrepr_DBtables <- as(as.vector(res[index,2]),res[index,3])
  #lsauv_DBtables
  index <- match("lsauv_DBtables",res[,1])
  out$SauvRep$lsauv_DBtables <- as(as.vector(res[index,2]),res[index,3])
  #daterepr
  index <- match("daterepr",res[,1])
  out$SauvRep$daterepr <- as(as.vector(res[index,2]),res[index,3])
  #lrepr_grid_z
  index <- match("lrepr_grid_z",res[,1])
  out$SauvRep$lrepr_grid_z <- as(as.vector(res[index,2]),res[index,3])
  #lsauv_grid_z
  index <- match("lsauv_grid_z",res[,1])
  out$SauvRep$lsauv_grid_z <- as(as.vector(res[index,2]),res[index,3])
  #daterepr_grid_z
  index <- match("daterepr_grid_z",res[,1])
  out$SauvRep$daterepr_grid_z <- as(as.vector(res[index,2]),res[index,3])
  #lrepr_hab_sensib
  index <- match("lrepr_hab_sensib",res[,1])
  out$SauvRep$lrepr_hab_sensib <- as(as.vector(res[index,2]),res[index,3])
  #lsauv_hab_sensib
  index <- match("lsauv_hab_sensib",res[,1])
  out$SauvRep$lsauv_hab_sensib <- as(as.vector(res[index,2]),res[index,3])
  #daterepr_hab_sensib
  index <- match("daterepr_hab_sensib",res[,1])
  out$SauvRep$daterepr_hab_sensib <- as(as.vector(res[index,2]),res[index,3])
  #lrepr_iq
  index <- match("lrepr_iq",res[,1])
  out$SauvRep$lrepr_iq <- as(as.vector(res[index,2]),res[index,3])
  #lsauv_iq
  index <- match("lsauv_iq",res[,1])
  out$SauvRep$lsauv_iq <- as(as.vector(res[index,2]),res[index,3])
  #daterepr_iq
  index <- match("daterepr_iq",res[,1])
  out$SauvRep$daterepr_iq <- as(as.vector(res[index,2]),res[index,3])
  #export_index_sql
  index <- match("export_index_sql",res[,1])
  out$SauvRep$export_index_sql <- as(as.vector(res[index,2]),res[index,3])
  #export_db_tables
  index <- match("export_db_tables",res[,1])
  out$SauvRep$export_db_tables <- as(as.vector(res[index,2]),res[index,3])
  #shape_ic
  index <- match("shape_ic",res[,1])
  out$SauvRep$shape_ic <- as(as.vector(res[index,2]),res[index,3])
  #export_ic
  index <- match("export_ic",res[,1])
  out$SauvRep$export_ic <- as(as.vector(res[index,2]),res[index,3])
  #db_choice
  index <- match("db_choice",res[,1])
  out$SauvRep$db_choice <- as(as.vector(res[index,2]),res[index,3])
  #export_scheme
  index <- match("export_scheme",res[,1])
  out$SauvRep$export_scheme <- as(as.vector(res[index,2]),res[index,3])
  
  
  
  #Elements 'Calc'
  #l_corr_surfhab
  index <- match("l_corr_surfhab",res[,1])
  out$Calc$l_corr_surfhab <- f_Int_to_Log(as(as.vector(res[index,2]),res[index,3]))
  #l_th_sensib
  index <- match("l_th_sensib",res[,1])
  out$Calc$l_th_sensib <- f_Int_to_Log(as(as.vector(res[index,2]),res[index,3]))
  #th_sensib
  index <- match("th_sensib",res[,1])
  out$Calc$th_sensib <- as(as.vector(res[index,2]),res[index,3])
  #agreg_sensib
  index <- match("agreg_sensib",res[,1])
  out$Calc$agreg_sensib <- as(as.vector(res[index,2]),res[index,3])
  
  
  #feuillet connexion_bd ---------------------------------------------
  print("import_param.R - lecture feuillet : connexion_bd")
  
  res <- as.matrix(read_ods(path, sheet="connexion_bd",col_names=FALSE))
  #on retire des champs ? lire les espaces et les quotes (ATTENTION : aucun espace dans aucun des champs)
  res[,1:2] <- apply(res[,1:2],2,function(x) gsub(" ","",x))
  res[,1:2] <- apply(res[,1:2],2,function(x) gsub("\"","",x))
  
  indexBEG <- match(c("*****connexion_bd_locale",
                      "*****connexion_bd_distante"),res[,1])
  
  ##########################################################,"*****D?limitations","*****Grilles","*****gestion_donnees_habitats"
  
  indexEND <- which(res[,1]%in%"*****")
  
  #Elements 'DBConnectlocal'
  index <- match("DBConnect_drv",res[,1])
  out$DBConnectlocal$DBConnect_drv <- as(as.vector(res[index,2]),res[index,3])
  index <- match("DBConnect_dbname",res[,1])
  out$DBConnectlocal$DBConnect_dbname <- as(as.vector(res[index,2]),res[index,3])
  index <- match("DBConnect_host",res[,1])
  out$DBConnectlocal$DBConnect_host <- as(as.vector(res[index,2]),res[index,3])
  index <- match("DBConnect_port",res[,1])
  out$DBConnectlocal$DBConnect_port <- as(as.vector(res[index,2]),res[index,3])
  index <- match("DBConnect_user",res[,1])
  out$DBConnectlocal$DBConnect_user <- as(as.vector(res[index,2]),res[index,3])
  index <- match("DBConnect_password",res[,1])
  out$DBConnectlocal$DBConnect_password <- as(as.vector(res[index,2]),res[index,3])
  
  #Elements 'DBConnectdist1'
  index <- match("DBConnect_drv_dist",res[,1])
  out$DBConnectdist1$DBConnect_drv <- as(as.vector(res[index,2]),res[index,3])
  index <- match("DBConnect_dbname_dist",res[,1])
  out$DBConnectdist1$DBConnect_dbname <- as(as.vector(res[index,2]),res[index,3])
  index <- match("DBConnect_host_dist",res[,1])
  out$DBConnectdist1$DBConnect_host <- as(as.vector(res[index,2]),res[index,3])
  index <- match("DBConnect_port_dist",res[,1])
  out$DBConnectdist1$DBConnect_port <- as(as.vector(res[index,2]),res[index,3])
  index <- match("DBConnect_user_dist",res[,1])
  out$DBConnectdist1$DBConnect_user <- as(as.vector(res[index,2]),res[index,3])
  index <- match("DBConnect_password_dist",res[,1])
  out$DBConnectdist1$DBConnect_password <- as(as.vector(res[index,2]),res[index,3])
  
  #####################################################
  #Elements 'Calc'
  #l_corr_surfhab
  #184index <- match("l_corr_surfhab",res[,1])
  #185out$Calc$l_corr_surfhab <- f_Int_to_Log(as(as.vector(res[index,2]),res[index,3]))
  #l_th_sensib
  #187index <- match("l_th_sensib",res[,1])
  #188out$Calc$l_th_sensib <- f_Int_to_Log(as(as.vector(res[index,2]),res[index,3]))
  #th_sensib
  #190index <- match("th_sensib",res[,1])
  #191out$Calc$th_sensib <- as(as.vector(res[index,2]),res[index,3])
  #agreg_sensib
  #193index <- match("agreg_sensib",res[,1])
  #194out$Calc$agreg_sensib <- as(as.vector(res[index,2]),res[index,3])
  ##############################################################
  
  #feuillet typologie_pressions ---------------------------------------------
  print("import_param.R - lecture feuillet : typologie_pressions")
  
  res  <- as.matrix(read_ods(path, sheet="typologie_pressions",col_names=FALSE))
  #dimension de la matrice lue
  ncol <- dim(res)[2] ; nlin <- dim(res)[1]
  # nombre de pressions
  out$nj <-  nlin-1  
  #codes et nom des pressions
  out$P_code   <- res[2:nlin,which(res[1,]=="code_pression")]  
  out$P_name   <- res[2:nlin,which(res[1,]=="nom_pression")]
  #perimetre de l'evaluation (evaluation dans la matrice A/P et utilisation dans le demonstrateur)
  out$P_evalDEM<- res[2:nlin,which(res[1,]=="demonstrateur")]
  out$P_modcalc<- res[2:nlin,which(res[1,]=="mode_calcul")]
  out$P_code_ag   <- res[2:nlin,which(res[1,]=="code_pression_agg_sensib")] ; 
  out$P_name_ag   <- res[2:nlin,which(res[1,]=="nom_pression_agg_sensib")]
  
  
  #feuillet typologie_activites ---------------------------------------------
  print("import_param.R - lecture feuillet : typologie_activites")
  res <- as.matrix(read_ods(path, sheet="typologie_activites",col_names=FALSE))
  # suppression des lignes et colonnes qui contiennent des NA
  res <- res[1:length(na.omit(res[,1])),1:length(na.omit(res[1,]))]
  # dimension de la matrice lue
  ncol <- dim(res)[2] ; nlin <- dim(res)[1]
  out$ni <-  nlin-1
  
  #codes et nom des activites
  out$A_code <- res[2:nlin,which(res[1,]=="code_activite_phase")] 
  out$A_name <- res[2:nlin,which(res[1,]=="nom_activite")]
  
  
  #feuillet data ---------------------------------------------
  print("import_param.R - lecture feuillet : data")
  
  res <- as.matrix(read_ods(path, sheet="data",col_names=FALSE))
  #reperage du debut des donnees
  indexBEG <- which(res[,1]%in%"*****")[1]; 
  indexEND <- which(res[,1]%in%"*****")[2]; 
  titles <- as.vector(res[(indexBEG+1),]);
  res <- res[(indexBEG+2):(indexEND-1),] ; colnames(res) <- c(titles);
  
  #on retire de la liste des Data_code les espaces et les quotes (ATTENTION : aucun espace dans aucun des champs)
  res[,1:ncol(res)] <- apply(res[,1:ncol(res)],2,function(x) gsub("\"","",x))
  res[,1:ncol(res)] <- apply(res[,1:ncol(res)],2,function(x) gsub("'","",x))
  out$DBData$Data <- res
  
  #feuillet matrice_activites_pressions ---------------------------------------------
  print("import_param.R - lecture feuillet : matrice_activites_pressions")
  
  res <- as.matrix(read_ods(path, sheet="matrice_activites_pressions",col_names=FALSE))
  out$Mat_AP <- res
  
  
  
  
  #feuillet simulation_monte_carlo ---------------------------------------------
  print("import_param.R - lecture feuillet : simulation_monte_carlo")
  
  res <- as.matrix(read_ods(path, sheet="simulation_monte_carlo",col_names=FALSE))
  #on retire des champs ? lire les espaces et les quotes (ATTENTION : aucun espace dans aucun des champs)
  res[,1:2] <- apply(res[,1:2],2,function(x) gsub(" ","",x))
  res[,1:2] <- apply(res[,1:2],2,function(x) gsub("\"","",x))
  
  
  
  
  #Elements 'facteurs'
  #X1
  index <- match("X1",res[,1])
  out$factor$X1 <- as(as.vector(res[index,2]),res[index,3])
  #X2
  index <- match("X2",res[,1])
  out$factor$X2 <- as(as.vector(res[index,2]),res[index,3])
  #X3
  index <- match("X3",res[,1])
  out$factor$X3 <- as(as.vector(res[index,2]),res[index,3])
  #X4
  index <- match("X4",res[,1])
  out$factor$X4 <- as(as.vector(res[index,2]),res[index,3])
  #X5
  index <- match("X5",res[,1])
  out$factor$X5 <- as(as.vector(res[index,2]),res[index,3])
  #X6
  index <- match("X6",res[,1])
  out$factor$X6 <- as(as.vector(res[index,2]),res[index,3])
  #X7
  index <- match("X7",res[,1])
  out$factor$X7 <- as(as.vector(res[index,2]),res[index,3])
  
  #Elements 'Simulations'
  #Nombre de simulations
  index <- match("Simul",res[,1])
  out$simul$nbsimul <- as(as.vector(res[index,2]),res[index,3])
  #Reprise des donnees activites
  index <- match("lrepr_activite",res[,1])
  out$simul$lrepr_activite <- as(as.vector(res[index,2]),res[index,3])
  #Sauvegarde des donnees activites
  index <- match("lsauv_activite",res[,1])
  out$simul$lsauv_activite <- as(as.vector(res[index,2]),res[index,3])
  #Date de reprise des donnees activites
  index <- match("daterepr_activite",res[,1])
  out$simul$daterepr_activite <- as(as.vector(res[index,2]),res[index,3])
  #Export des simulations
  index <- match("export_simul",res[,1])
  out$simul$export_simul <- as(as.vector(res[index,2]),res[index,3])
  #Export des resultats
  index <- match("export_result",res[,1])
  out$simul$export_result <- as(as.vector(res[index,2]),res[index,3])
  #choix de la base de donnees
  index <- match("db_choice",res[,1])
  out$simul$db_choice <- as(as.vector(res[index,2]),res[index,3])
  #Sch?ma d'export des simulations
  index <- match("export_scheme",res[,1])
  out$simul$export_scheme <- as(as.vector(res[index,2]),res[index,3])
  #Seuil pour les statistiques des simulations
  index <- match("thres1",res[,1])
  out$simul$thres1 <- as(as.vector(res[index,2]),res[index,3])
  #Seuil pour les statistiques des simulations
  index <- match("thres2",res[,1])
  out$simul$thres2 <- as(as.vector(res[index,2]),res[index,3])
  #Seuil pour les statistiques des simulations
  index <- match("thres3",res[,1])
  out$simul$thres3 <- as(as.vector(res[index,2]),res[index,3])
  
  
  #feuillet statistiques_par_zones ---------------------------------------------
  print("import_param.R - lecture feuillet : statistiques_par_zones")
  
  res <- as.matrix(read_ods(path, sheet="statistiques_par_zones",col_names=FALSE))
  #reperage du debut des donnees
  indexBEG <- which(res[,1]%in%"*****")[1]; 
  indexEND <- which(res[,1]%in%"*****")[2]; 
  titles <- as.vector(res[(indexBEG+1),]);
  res <- res[(indexBEG+2):(indexEND-1),] ; colnames(res) <- c(titles);
  
  #on retire de la liste des Data_code les espaces et les quotes (ATTENTION : aucun espace dans aucun des champs)
  res[,1:ncol(res)] <- apply(res[,1:ncol(res)],2,function(x) gsub("\"","",x))
  res[,1:ncol(res)] <- apply(res[,1:ncol(res)],2,function(x) gsub("'","",x))
  out$tabgraph$Data <- res
  
  
  
  return(out)
  
}

