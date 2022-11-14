# -------------------------------------------------------------------
# Rpostgis.R
# IMPORT AND EXPORT GEOM BETWEEN R AND POSTGIS
# Script based on https://philipphunziker.wordpress.com/2014/07/20/transferring-vector-data-between-postgis-and-r/
# And updated by Nicolas LAMBERT, 2015
# contient les fonctions : 
# - dbWriteSpatial
# - dbReadSpatial
# -------------------------------------------------------------------
dbWriteSpatial <- function(con, spatial.df, schemaname="public", tablename, replace=FALSE,srid=NULL) {
  
  # Create WKT and add to spatial dataframe
  if(nrow(spatial.df)==1){byid<-F}
  if(nrow(spatial.df)!=1){byid<-T}
  spatialwkt <- writeWKT(spatial.df, byid=byid)
  spatial.df$wkt <- spatialwkt
  
  # Add temporary unique ID and Set column names to lower case
  spatial.df$spatial_id <- 1:nrow(spatial.df)
  names(spatial.df) <- tolower(names(spatial.df))
  
  # Upload the dataframe to postgis
  data.df <- spatial.df@data
  rv <- dbWriteTable(con, c(schemaname, tablename), data.df, overwrite=replace, row.names=FALSE)
  
  # Create geometry column and clean up table
  schema.table <- paste(schemaname, ".", tablename, sep="")
  query1 <- paste("ALTER TABLE ", schema.table, " ADD COLUMN geom GEOMETRY;", sep="")
  er <- dbSendQuery(con, statement=query1)
  dbClearResult(er)
  query2 <- paste("UPDATE ", schema.table, " SET geom = ST_GEOMETRYFROMTEXT(t.wkt) FROM ", schema.table, " t  WHERE t.spatial_id = ", schema.table, ".spatial_id;", sep="")
  er <- dbSendQuery(con, statement=query2)
  dbClearResult(er)
  query3 <- paste("ALTER TABLE ", schema.table, " DROP COLUMN spatial_id;")
  er <- dbSendQuery(con, statement=query3)
  dbClearResult(er)
  query4 <- paste("ALTER TABLE ", schema.table, " DROP COLUMN wkt;")
  er <- dbSendQuery(con, statement=query4)
  dbClearResult(er)
  if (!is.null(srid)) {
  query5 <- paste("SELECT UpdateGeometrySRID('",tablename,"','geom',",srid,")",sep="")
  er <- dbSendQuery(con, statement=query5)
  dbClearResult(er)
  } else{srid <- "undefined"}

  # Display Output message
  output<-paste("The layer",tablename,"was added to POSTGIS with SRID =",srid)
  return(output)
  }

dbReadSpatial <- function(con, schemaname, tablename,geomcol) {  #ALICE remplacement schemaname par schemaname=in.schema
    # ALICEold: dbReadSpatial <- function(con, schemaname="public", tablename, geomcol="geom", idcol=NULL) {  #ALICE remplacement schemaname par schemaname=in.schema
    #ALICE      schemaname <- in.schema ; tablename <- in.table ; geomcol <- "geom"; idcol <- NULL
  idcol<-NULL
  ## Build query and fetch the target table
  # Get column names
  q.res <- dbSendQuery(con, statement=paste("SELECT column_name FROM information_schema.columns WHERE table_name ='", tablename, "' AND table_schema ='", schemaname, "';", sep=""))
  schema.table = paste(schemaname, ".", tablename, sep="")
  q.df <- fetch(q.res, -1)
  # Some safe programming
  if (!(geomcol %in% q.df[,1])) {stop(paste("No", geomcol, "column in specified table."))}
  if (!is.null(idcol)) {
    if (!(idcol %in% q.df[,1])) {stop(paste("Specified idname '", idcol, "' not found.", sep=""))}
  }
  # Get table
  query <- paste("SELECT", paste(q.df[,1][q.df[,1] != geomcol], collapse=", "), paste(", ST_ASTEXT(", geomcol, ") AS geom FROM", sep=""), schema.table, ";")
  t.res <- dbSendQuery(con, statement=query)
  t.df <- fetch(t.res, -1)
  
  ## Get geometry ID column number
  if (!is.null(idcol)) {
    idcolnum <- which(names(t.df) == idcol)
  } else {
    t.df$id.new <- 1:nrow(t.df)
    idcolnum <- which(names(t.df) == "id.new")
  }
  
  ## Get geometry column number
  geomcolnum <- which(names(t.df) == geomcol)
  
  ## Build spatial data frame using OGR
  write.df <- t.df[,geomcolnum,drop=FALSE]
  names(write.df) <- "WKT"
  filename <- paste("vector_", as.character(format(Sys.time(), "%H_%M_%S")), sep="")
  filename.csv <- paste(filename, ".csv", sep="")
  write.csv(write.df, paste(gsub("[\\]", "/", tempdir()), "/", filename.csv, sep=""), row.names=TRUE)
  down.spdf <- readOGR(dsn=paste(gsub("[\\]", "/", tempdir()), "/", filename.csv, sep=""), layer=filename, verbose=FALSE)
  rv <- file.remove(paste(gsub("[\\]", "/", tempdir()), "/", filename.csv, sep=""))
  data.df <- data.frame(t.df[,-geomcolnum])
  names(data.df) <- names(t.df)[-geomcolnum]  
  
  # For Spatial Points Data Frame  
  if (grepl("POINT", t.df[1,geomcolnum])) {
    spatial.df <-  SpatialPointsDataFrame(down.spdf@coords, data.df, match.ID=FALSE)
  }
  # For Spatial Polygons/Lines Data Frame    
  if (grepl("POLYGON", t.df[1,geomcolnum]) | grepl("LINE", t.df[1,geomcolnum])) {
    spatial.df <- down.spdf
    spatial.df@data <- data.df
    spatial.df <- spChFIDs(spatial.df, paste(t.df[,idcolnum]))
  }
  
  # Add proj4 information
  query1 <- paste("SELECT ST_SRID(geom) FROM",schema.table,"LIMIT 1")  #ALICE : schema.table  #original : tablename
  srid <- as.numeric(dbGetQuery(con, statement=query1))  
  query2=paste("SELECT proj4text from spatial_ref_sys where auth_SRID =",srid);
  proj <-as.character(dbGetQuery(con, statement=query2))
  proj4string(spatial.df) <- proj
  
  # Delete id.new column
  spatial.df@data$id.new <- NULL
  return(spatial.df)
}