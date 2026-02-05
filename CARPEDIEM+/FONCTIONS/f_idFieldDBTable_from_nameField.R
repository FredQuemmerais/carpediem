# -------------------------------------------------------------------
# f_idFieldDBTable_from_nameField
# FONCTION D'OUVERTURE ET LECTURE DES TABLES DECRITES DANS FICHIER DE PARAMETRAGE
# Sert à récupérer les numéros des colonnes correspondant à certaines informations pour constituer une commande en SQL
# 
# A combiner avec :    f_argDB
#--------------------------------------------------------------------

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
