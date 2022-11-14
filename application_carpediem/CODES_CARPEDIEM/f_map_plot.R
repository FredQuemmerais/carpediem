# -------------------------------------------------------------------"
# Plot_VARz_grid2D  : PLOT 2D OF THE Z VALUES OF THE GRID             "
#                                                                    "
# created  by  : 23/08/2014 par Alice Vanhoutte-Brunier              "
# versioning   : Mercurial / SourceTree, Bitbucket project MODKELP   "
# contributors : A. Marzin                                                   "
#                                                                    "
# IN           : VARval    : matrice valeurs a tracer [1:nz,1] ou  [1:nz,1:2] si colonne 1 = "mesh"                                          "
#              : grid_plot : grille de gestion fine
#              : VARname   : nom de la variable ? tracer dans la matrice VARval
#              : VARattr   : l?gende de la figure
#              : outformat : shp ou pdf ou png ou pdfpng
#              : choicepal : palette de couleurs
#              : nbniv     : "" ou vecteur, si vecteur, doit correspondre au nombre de niveau definis intrinsequement
#              : ltdc      : logique egal a TRUE si ajout du trait de cote
#              : lcont     : logique egal a TRUE si ajout du contour du site d'etude Molene
#              : lsta      : logique egal a TRUE si ajout des points des donnees in situ campagne estimation biomasse
#              : lrest     : logique egal a TRUE si ajout des zones fermees a l'exploitation LH
#              : lgrid     : logique egal a TRUE si ajout des numeros de maille
#              : lcity     : logique egal a TRUE si ajout des noms de villes cotieres
# -------------------------------------------------------------------"


Plot_VARz_grid2D <- function(VARval,grid_plot,config,VARname,VARattr){
  #TestFonction VARval <- VARz; grid_plot <- grid_z; config <- config; VARname <- nameVARz
  #             VARattr <- "surface de mer par maille"; choicepal <- "pal_CPUE"; nbniv <- 10
  #             outformat <- "shp"; ltdc<-TRUE; lcont<-FALSE; lcity<-TRUE; lzoom<-config$Outputs$lzoom
  
  print(paste("# f_map_plot.R: Plot_VARz_grid2D : ",VARname,sep=""))
  # add the column "mesh to the IN matrix VARval -> VARplot
  # label de la ligne = numero de maille
  i_idz    <- f_idFieldDBTable_from_nameField("idmesh",config$DBData$Data,G_grid@data,"grid")
  name_id  <- colnames(head(grid_plot))[i_idz]
  lplot <- grid_plot
  VARplot <- VARval
  
  # ajout d'un nouvel attribut dans la grille correspondant a la variable
  if(VARname %in% colnames(as.matrix(grid_plot@data))){
    print(paste("# f_map_plot.R:    -- the variable ",VARname," is already in the grid ",sep=""))
  }else{
    print(paste("# f_map_plot.R:    -- the variable ",VARname," is not in the grid ",sep=""))
    New_Attribut <- merge(grid_plot@data,VARplot,by.x=name_id,by.y="mesh",all.x=T,sort=FALSE)
    lplot@data <- New_Attribut 
  }  
  
  # trace des plot ou creation de .shp
  lplot@data[,dim(lplot@data)[2]] <- as.numeric(lplot@data[,dim(lplot@data)[2]])
  datelayer <- paste(substr(date(),21,24),substr(date(),5,7),substr(date(),9,10),substr(date(),12,13),substr(date(),15,16),sep="")
  layername <- paste(VARname,datelayer,sep="_")
  writeOGR(lplot, dsn = outdir, layer = layername, driver = 'ESRI Shapefile')
}