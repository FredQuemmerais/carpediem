# -------------------------------------------------------------------
# f_lec_Postgres_DB.R                                                      
# FONCTIONS FACILITANT OUVERTURE ET LECTURE DES TABLES DECRITES DANS FICHIER DE PARAMETRAGE
# contient les fonctions :                                         
#  - f_argDB
#  - f_idFieldDBTable_from_nameField    
#--------------------------------------------------------------------

# Renvoie les arguments necessaires a la fonction d'import des tables dans BD
f_argDB <- function(in.Data_code,in.Data_List,in.config) {
  # TestCode : in.Data_code <-iTable_code  ;  in.config <- config ; in.Data_List <- config$DBData$Data
  #
  # Identifier le type de BD qui contient la table : DB_choice dans DBData
    #  - liste des Data_code de la table DBData contenues dans la colonne 'Data_code'
  f.list  <- in.Data_List[,which(colnames(in.Data_List)=="Table_code")]
  #  - identification du jeu de donnees dans la liste de codes de donnees 
  f.lin  <- which(f.list==in.Data_code)[1] #utile de lire premiere valeur si plusieurs lignes 
  #  - identification de la base de donnees qui contient la table
  f.col <- which(colnames(in.Data_List)=="DB_choice")
  #  - lecture DB_choice de la table
  f.DB_choice <- in.Data_List[f.lin,f.col]
  
  if(f.DB_choice=="local"){
    out.DBConnect_drv      <- in.config$DBConnectlocal$DBConnect_drv;
    out.DBConnect_dbname   <- in.config$DBConnectlocal$DBConnect_dbname;
    out.DBConnect_host     <- in.config$DBConnectlocal$DBConnect_host;
    out.DBConnect_port     <- in.config$DBConnectlocal$DBConnect_port;
    out.DBConnect_user     <- in.config$DBConnectlocal$DBConnect_user;
    out.DBConnect_password <- in.config$DBConnectlocal$DBConnect_password;
  }
  
  if(f.DB_choice=="dist1"){
    out.DBConnect_drv      <- in.config$DBConnectdist1$DBConnect_drv;
    out.DBConnect_dbname   <- in.config$DBConnectdist1$DBConnect_dbname;
    out.DBConnect_host     <- in.config$DBConnectdist1$DBConnect_host;
    out.DBConnect_port     <- in.config$DBConnectdist1$DBConnect_port;
    out.DBConnect_user     <- in.config$DBConnectdist1$DBConnect_user;
    out.DBConnect_password <- in.config$DBConnectdist1$DBConnect_password;
  }
  
  # ajouter des cas de figures si autres DB
  
  f.col <- which(colnames(in.Data_List)=="DB_scheme")
  out.DB_scheme <- in.Data_List[f.lin,f.col]
  
  f.col <- which(colnames(in.Data_List)=="DB_table")
  out.DB_table <- in.Data_List[f.lin,f.col]
  
  out <- c(out.DBConnect_drv,out.DBConnect_dbname,
           out.DBConnect_host,out.DBConnect_port,
           out.DBConnect_user,out.DBConnect_password,
           out.DB_scheme,out.DB_table)
  
  rm(f.list,f.lin,f.col,f.DB_choice)
  
  return(out)
  
}

# Renvoie le numero de colonne de la DB/Table correspondant au champ Var_code indique dans le feuillet de parametrage/'Data'
f_idFieldDBTable_from_nameField <- function(in.Var_code,in.SheetData,in.DBTable,in.TableName){
  #TestFonction : in.Var_code <- "srm" ; in.SheetData <- config$DBData$Data ; in.DBTable <- G_grid@data ; in.TableName <-"grid"

  
  #  - recherche du numero de la colonne correspondant a "Var_code"
  icol <- which(colnames(in.SheetData)=="Var_code")
  #    - lignes qui contiennent Var_code = in.Var_code
  ilin <- which(in.SheetData[,icol]==in.Var_code)
  #    - en cas de plusieurs lignes, ligne qui correspond a la table
  ilin <- ilin[which(in.SheetData[ilin,which(colnames(in.SheetData)=="Table_code")]==in.TableName)]
  #  - recherche du numero de la colonne correspondant a "Field"
  icol <- which(colnames(in.SheetData)=="Field")
  # - recherche du nom du champ de la DB/table correspondant a la srm
  nameField <- in.SheetData[ilin,icol]
  # recherche du numero de colonne de la table correspondant a ce champ 
  # - verification que ce champ existe bien
  if(!nameField%in%colnames(in.DBTable)){print(paste("STOP lec_Postgres_DB.R : le champ ",nameField,"n existe pas colonne Field / ligne: ",ilin,
                                                    " - Renseigner dans Data/Field un nom parmi : "));
                                         stop(print(colnames(in.DBTable)))}
  out  <- which(colnames(in.DBTable)==nameField)
  return(out)
}                                                      
