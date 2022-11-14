# -------------------------------------------------------------------"
#        FUNCTIONS FOR MANAGE GIS DATA                               "
#                                                                    "
#                                                                    "
# -------------------------------------------------------------------"

# -------------------------------------------------------------------"
# load_SIG_file  : OPEN GIS LAYERS                                   "
#                                                                    "
# creation   : Alice Vanhoutte-Brunier                               "
# contributors :                                                     "
#                                                                    "
# -------------------------------------------------------------------"
# Functions for opening GIS layers
# ---------------------------------
# Load shapefile 
# sigdir<-dir;fname<-file
load_SIG_file <- function(sigdir,fname,type,proj,band) {
  if(type == 1){
    SIGlayer <- readOGR(sigdir, fname,
                        verbose = TRUE,
                        p4s=NULL,
                        stringsAsFactors=default.stringsAsFactors(),
                        drop_unsupported_fields=FALSE,
                        #input_field_name_encoding=NULL,
                        pointDropZ=FALSE,
                        dropNULLGeometries=TRUE,
                        useC=TRUE,
                        disambiguateFIDs=FALSE,
                        addCommentsToPolygons=TRUE,
                        encoding=NULL,
                        use_iconv=FALSE)
  }
  if(type == 2){
    name <- paste(sigdir,"/",fname,".tif",sep="")
    SIGlayer1 <- raster(name,band=band)
#    SIGlayer1 <- readGDAL (name,p4s=NULL, silent = FALSE)
    SIGlayer  <- SIGlayer1
    if(proj == 2) {SIGlayer <- spTransform(SIGlayer1, CRS ("+init=epsg:4326"))}
    
  }
  
  return(SIGlayer)
}

grid_cell_coords <- function(gridlayer, cellnum=1) {
  gridlayer@polygons[[cellnum]]@Polygons[[1]]@coords
}

grid_cell_center <- function(gridlayer, cellnum=1) {
  gridlayer@polygons[[cellnum]]@labpt
}



# -------------------------------------------------------------------"
# RasterToMat  : TRANSFORM RASTER LAYER INTO MATRICES                "
#                                                                    "
# created  by  : 20/07/2014 par Alice Vanhoutte-Brunier              "
# versioning	 : Mercurial / SourceTree, Bitbucket project MODKELP   "
# contributors :                                                     "
#                                                                    "
# -------------------------------------------------------------------"

RasterToMat <- function(name,type){
  MAT <- list()
  MAT$type <- type
  MAT$str  <- class(name)
  
  if(type == 1){                      # read of a shape file
    
    
  }
  if(type == 2){                      # read of a raster file
#OLD    
    MAT$val <- as.matrix(name)
    ext  <- extent(name)
    res <- res(name)    #resolution 
    ni  <- ncol(name) ; nj <- nrow(name); ncell <- ncell(name);
    dx  <- res[1]; dy <- res[2]
  
    # remplissage matrice lat : convention = N -> S 
    vec <- rep(0,nj)    
    mat <- matrix(0,nrow=nj, ncol=ni)
    for (j in 1:nj){vec[j] <- ext@ymax - ((j-0.5)*dy) ;}
    for (i in 1:ni){for (j in 1:nj){mat[j,i] <- vec[j];}}
    MAT$lat <- mat
    MAT$latmin <- MAT$lat - (0.5)*dy
    MAT$latmax <- MAT$lat + (0.5)*dy
    
    # remplissage matrice long : convention = O -> E 
    vec <- rep(0,ni)    
    mat <- matrix(0,nrow=nj, ncol=ni)
    for (i in 1:ni){vec[i] <- ext@xmin + ((i-0.5)*dx);}
    for (j in 1:nj){for (i in 1:ni){mat[j,i] <- vec[i];}}
    MAT$long <- mat
#NEW
#    MAT$val <- as.matrix(name)
#    ext  <- extent(name)
#    res <- name@grid@cellsize    #resolution 
#    ni  <- name@grid@cells.dim[2]; nj <- name@grid@cells.dim[1]; ncell <- ni*nj;
#    dx  <- res[1]; dy <- res[2]
#    
    # remplissage matrice lat : point de reference = SO, convention S -> N
#    vec <- rep(0,ni)    
#    mat <- matrix(0,nrow=nj, ncol=ni)
#    for (j in 1:nj){vec[j] <- name@bbox[2,1] + ((j-1)*dy) ;}
#    for (i in 1:ni){for (j in 1:nj){mat[j,i] <- vec[j];}}
#    MAT$lat <- mat
    
    # remplissage matrice long : convention = O -> E 
#    vec <- rep(0,ni)    
#    mat <- matrix(0,nrow=nj, ncol=ni)
#    for (i in 1:ni){vec[i] <- name@bbox[1,1] + ((i-1)*dx) ;}
#    for (j in 1:nj){for (i in 1:ni){mat[j,i] <- vec[i];}}
#    MAT$long <- mat
    
#    print(paste("coin SO : ", MAT$lat[1,1], MAT$long[1,1]))
#    print(paste("coin NO : ", MAT$lat[nj,1], MAT$long[nj,1]))
#    print(paste("coin SE : ", MAT$lat[1,ni], MAT$long[1,ni]))
#    print(paste("coin NE : ", MAT$lat[nj,ni], MAT$long[nj,ni]))
    
    MAT$ncell <- ncell
    MAT$ni    <- ni
    MAT$nj    <- nj
    MAT$dx    <- dx
    MAT$dy    <- dy
    MAT$proj  <- crs(name)
    
  }
    
  return(MAT)
  
}

# Functions for opening NetCDF files
# ---------------------------------
# Load shapefile 
print_vars_netCDF <- function(dirfile,namefile,lprint,printfile) {
  #namefile <- paste(config$res_PREVIMER$file_prefix_previmer_2D,"_20140101T0000Z_MeteoMF.nc",sep="")
  #dirfile <- paste(indir,"/DATA/",config$res_PREVIMER$file_dir_previmer_2D,sep="")
  #lprint <- FALSE ; printfile <- paste(outdir,"/",sep="")
  printfile <- paste(printfile,"VARIABLES_fichier_",namefile,".txt",sep="")
  pathfile  <- paste(dirfile,namefile,sep="")
  #if(lprint) sink(fileprint)
  if(!file.exists(pathfile)) {
    if(lprint)print(paste("fichier PREVIMER ",namefile," n existe pas"))
    status <- 0
  } else {
    status <- 1
    in.nc <- nc_open(pathfile, write=FALSE, readunlim=TRUE,verbose=FALSE)
    if(lprint)print(paste("Fichier",in.nc$pathfile,"contient",in.nc$nvars,"variables : "))
    for(i in 1:in.nc$nvars) {
      v <- in.nc$var[[i]]
      if(lprint)print(paste("Variable n ",i,"  / Nom: ",v$name,"  / Nom long: ",v$longname," Unit: ",v$units," ValManq: ",v$missval," Dims: ",v$ndims," VarSize:",v$varsize,sep=""))
    }
    nc_close(in.nc)
  }
  return(status)
}

load_var_netCDF_grid <- function(dirfile,namefile,lprint,printfile,varname,lreg,ijorder,gridlat,gridlong,in.reduc) {
# LOAD VARIABLE FROM A NETCDF FILE t= 1 AT THE MODEL CATCHMENT
  #ijorder <- "ij"; ijorder <- "ji";
  #varname <- uname;gridlat <- grid_z$lat;gridlong <- grid_z$long
  #varname <- uname
  #in.reduc <- in_previmer$reduc
  #lreg <- lreggrid
  #dirfile,namefile,lprint,printfile,lreggrid,ijorder,grid_z$lat,grid_z$long,"lat"
  out <- list()
  
  pathfile  <- paste(dirfile,namefile,sep="")
  printfile <- paste(printfile,"LEC_VARIABLE_",varname,".txt",sep="")
  if(!file.exists(pathfile)) stop("erreur lecture fichier PREVIMER, il n existe pas")
  #if(lprint) sink(fileprint)
  in.nc <- nc_open(pathfile, write=FALSE, readunlim=TRUE,verbose=FALSE)
  
  if(lreg){ # grille reguliere
  if(!in.reduc$lreduc){  # la reduction du domaine n a pas encore ete calculee
    # lecture dimensions spatiales
    in.lat <- ncvar_get(in.nc,varid="latitude") ; in.lon <- ncvar_get(in.nc,varid="longitude")
    if(ijorder=="ij") {in.lat <- t(in.lat);  in.lon <- t(in.lon)} 
    nj  <- dim(in.lat)[1] ; ni <- dim(in.lat)[2]
    
    # si longitude et latitude sont des vecteurs, les transformer en matrice de dimensions identiques ? la grille
    if(is.na(ni) | is.na(nj)){
      ni <- dim(in.lon)[1]
      nj <- dim(in.lat)[1]
      
      temp <- in.lon
      #in.lon <- as.matrix(rep(in.lon,nj,nrow=nj))
      for (j in 1:(nj-1)){temp <- rbind(in.lon,temp)}
      in.lon <- temp
      #
      temp <- in.lat
      for (i in 1:(ni-1)){temp <- cbind(in.lat,temp)}
      in.lat <- temp
    }
    # r?duction du domaine lu ? l'emprise du domaine modelis?
    imin    <- which(round(in.lon[1,],1)==round(gridlong[1],1))[1]-1
    vecimax <- which(round(in.lon[1,],1)==round(gridlong[dim(gridlong)],1))
    imax    <- vecimax[dim(as.matrix(vecimax))[1]]+1
    icount  <- imax-imin+1
    
    jmin    <- which(round(in.lat[,1],1)==round(gridlat[1],1))[1]-1
    vecjmax <- which(round(in.lat[,1],1)==round(gridlat[dim(gridlat)],1))
    jmax    <- vecjmax[dim(as.matrix(vecjmax))[1]]+1
    jcount  <- jmax-jmin+1
    print(paste("s?lection du domaine modelis? j/i:  ",jcount,"/",nj," - ", icount,"/",ni, sep=""))
    out$reduc$imin <- imin
    out$reduc$jmin <- jmin
    out$reduc$icount <- icount
    out$reduc$jcount <- jcount
    out$reduc$lreduc <- T
    
  } else {
    imin <- in.reduc$imin
    jmin <- in.reduc$jmin
    icount <- in.reduc$icount
    jcount <- in.reduc$jcount
    out$reduc <- in.reduc
  }
  
  # lecture des r?sultats previmer
  if(ijorder=="ij") {
    in.vals <- ncvar_get(in.nc,varid=varname,start=c(imin,jmin,1),count=c(icount,jcount,1))
    in.vals <- t(in.vals)
  } else {
    in.vals  <- ncvar_get(in.nc,varid=varname,start=c(jmin,imin,1),count=c(jcount,icount,1))
  }
  
  } else { # grille irreguliere  ==============================================================
    in.vals  <- ncvar_get(in.nc,varid=varname)
    if(varname == "uubr" | varname == "vubr") {  #validit? uubr et vubr -+ 180 m/s
      in.vals[in.vals< -180.0] <- NA
      in.vals[in.vals>  180.0] <- NA
    }
  
#     if(!in.reduc$lreduc){  # la reduction du domaine n a pas encore ete calculee
#       # lecture dimensions spatiales
#       in.lat <- ncvar_get(in.nc,varid="latitude") ; in.lon <- ncvar_get(in.nc,varid="longitude")
#       nj  <- dim(in.lat)[1] ; ni <- dim(in.lon)[1]
#       
#       # s?lection des mailles dans le domaine modelis? 
#       vecind  <- which(in.lon>gridlong[1] & in.lon<gridlong[dim(gridlong)] & in.lat>gridlat[1] & in.lat<gridlat[dim(gridlat)])
#       out$reduc$vecind <- vecind
#       out$reduc$lreduc <- T
#       
#     } else {
#       vecind <- in.reduc$vecind
#       out$reduc <- in.reduc
#     }
#     
#     # lecture des r?sultats previmer
#     in.vals <- in.vals[vecind]
   }
#   
  out$vals <- in.vals
  nc_close(in.nc)
  return(out)
}

load_latlon_netCDF_grid <- function(dirfile,namefile,lprint,printfile,lreg,ijorder,gridlat,gridlong,axe) {
  # LOAD VARIABLE FROM A NETCDF FILE t= 1 AT THE MODEL CATCHMENT
  pathfile  <- paste(dirfile,namefile,sep="")
  printfile <- paste(printfile,"LEC_",axe,".txt",sep="")
  if(!file.exists(pathfile)) stop("erreur lecture fichier PREVIMER, il n existe pas")
  in.nc <- nc_open(pathfile, write=FALSE, readunlim=TRUE,verbose=FALSE)
  
  if(lreg){
  #if(lprint) sink(fileprint)

  if(axe=="lat") {
    in.lat  <- ncvar_get(in.nc,varid="latitude")
    if(ijorder=="ij") {in.lat <- t(in.lat)} 
    
    if(is.na(dim(in.lat)[2])){  # vecteur
      jmin    <- which(round(in.lat,1)==round(gridlat[1],1))[1]-1
      vecjmax <- which(round(in.lat,1)==round(gridlat[dim(gridlat)],1))
      jmax    <- vecjmax[dim(as.matrix(vecjmax))[1]]+1
      print(paste(" latitude du domaine modelis? :  ",signif(in.lat[jmin],3)," - ",signif(in.lat[jmax],3),sep=""))
      in.axe <- in.lat[jmin:jmax]
    } else {                    #   matrice
      jmin    <- which(round(in.lat[,1],1)==round(gridlat[1],1))[1]-1
      vecjmax <- which(round(in.lat[,1],1)==round(gridlat[dim(gridlat)],1))
      jmax    <- vecjmax[dim(as.matrix(vecjmax))[1]]+1
      print(paste(" latitude du domaine modelis? :  ",signif(in.lat[jmin,1],3)," - ",signif(in.lat[jmax,1],3),sep=""))
      in.axe <- in.lat[jmin:jmax,1]
    }
  }
  if(axe=="lon") {
    in.lon  <- ncvar_get(in.nc,varid="longitude")
    if(ijorder=="ij") {in.lon <- t(in.lon)} 
    
    if(is.na(dim(in.lon)[2])){  # vecteur
      imin    <- which(round(in.lon,1)==round(gridlong[1],1))[1]-1
      vecimax <- which(round(in.lon,1)==round(gridlong[dim(gridlong)],1))
      imax    <- vecimax[dim(as.matrix(vecimax))[1]]+1
      print(paste(" longitude du domaine modelis?:  ",signif(in.lon[imin],3)," - ",signif(in.lon[imax],3),sep=""))
      in.axe <- in.lon[imin:imax]
    } else {                    #   matrice
      imin    <- which(round(in.lon[1,],1)==round(gridlong[1],1))[1]-1
      vecimax <- which(round(in.lon[1,],1)==round(gridlong[dim(gridlong)],1))
      imax    <- vecimax[dim(as.matrix(vecimax))[1]]+1
      print(paste(" longitude du domaine modelis?:  ",signif(in.lon[1,imin],3)," - ",signif(in.lon[1,imax],3),sep=""))
      in.axe <- in.lon[1,imin:imax]
    }
  }
  } else { #grille irreguliere, pas de reduction du domaine modelise
    if(axe=="lat") {
      in.axe   <- ncvar_get(in.nc,varid="latitude")
    }
    if(axe=="lon") {
      in.axe  <- ncvar_get(in.nc,varid="longitude")
    }
  }
  nc_close(in.nc)
  return(in.axe)
}


