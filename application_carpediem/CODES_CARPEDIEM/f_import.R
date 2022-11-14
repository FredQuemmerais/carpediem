# -------------------------------------------------------------------"
# IMPORT DES FICHIERS DE PARAMETRES EXCEL                         "
# -------------------------------------------------------------------"

source('f_import_param.R')


# -------------------------------------------------------------------"
# IMPORT D'UNE TABLE CONTENUE DANS UN SCHEMA D'UNE BD PostgreSQL    "
# -------------------------------------------------------------------"

f_importTableFromDBPostgresSQL <- function(in.drv,in.dbname,in.host,in.port,in.user,in.password,in.scheme,in.table){
  in.options <- paste("-c search_path=",in.scheme,sep="")
  
  #- test de la fonction, ? copier manuellement dans la console
  #in.drv <- config$DBConnect$DBConnect_drv ; in.dbname <- config$DBConnect$DBConnect_dbname ; in.host <- config$DBConnect$DBConnect_host ;
  #in.port <- config$DBConnect$DBConnect_port ; in.user <- config$DBConnect$DBConnect_user ; in.password <- config$DBConnect$DBConnect_password ;
  #in.scheme <-  config$Grid$DB_grid_scheme ; in.table <- config$Grid$DB_grid_table

  require(DBI)

  out.table <- list()
  
  drv <- dbDriver(in.drv)
  on.exit(dbUnloadDriver(drv), add = TRUE)
  
  con <- dbConnect(drv, dbname = in.dbname,
                               host = in.host, port = in.port,
                               user = in.user, password = in.password)
                               #,options=in.options)        # selectionner le bon schema
  on.exit(dbDisconnect(con))
  
  #test Nico.Lambert http://neocarto.hypotheses.org/1186
  spdf <- dbReadSpatial(con, schemaname=in.scheme, tablename=in.table, geomcol="geom")
  proj4string(spdf)
  #head(spdf@data)
  #plot(spdf)
  
  
  # liste des tables du schema kit_simcelt
  #DB_kit_simcelt_Tables <- dbListTables(con)
  
  # verification de l'existence de la table                      
  #check <- dbExistsTable(con,in.table)       #option lecture de sch?ma ???             
  #if(!check){stop(paste("STOP : La table ",in.table," n est pas dans le sch?ma ",in.scheme,sep=""))}

  # import de table dans R - librairie PostGIStools
  #out.table <- get_postgis_query(con,paste("SELECT * FROM ",in.table,sep=""))
  
  #paste("CONNECTIONS drv : ",dbListConnections(drv))
  #dbUnloadDriver(drv)

  out.table <- spdf
  return(out.table)
}
