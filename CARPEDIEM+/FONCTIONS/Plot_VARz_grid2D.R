# -------------------------------------------------------------------"
# Plot_VARz_grid2D  : PLOT 2D OF THE Z VALUES OF THE GRID             "
#                                                                    "
# created  by  : 23/08/2014 par Alice Vanhoutte-Brunier              "
# versioning   : Mercurial / SourceTree, Bitbucket project MODKELP   "
# contributors : A. Marzin                                                   "
#                                                                    "
# IN           : VARval    : matrice valeurs ? tracer [1:nz,1] ou  [1:nz,1:2] si colonne 1 = "mesh"                                          "
#              : grid_plot : grille de gestion fine
#              : VARname   : nom de la variable ? tracer dans la matrice VARval
#              : VARattr   : l?gende de la figure
#              : outformat : shp ou pdf ou png ou pdfpng
#              : choicepal : palette de couleurs
#              : nbniv     : "" ou vecteur, si vecteur, doit correspondre au nombre de niveau d?finis intrins?quement
#              : ltdc      : logique ?gal ? TRUE si ajout du trait de c?te
#              : lcont     : logique ?gal ? TRUE si ajout du contour du site d'?tude Mol?ne
#              : lsta      : logique ?gal ? TRUE si ajout des points des donn?es in situ campagne estimation biomasse
#              : lrest     : logique ?gal ? TRUE si ajout des zones ferm?es ? l'exploitation LH
#              : lgrid     : logique ?gal ? TRUE si ajout des num?ros de maille
#              : lcity     : logique ?gal ? TRUE si ajout des noms de villes c?ti?res
# -------------------------------------------------------------------"


Plot_VARz_grid2D <- function(VARval,grid_plot,config,VARname,VARattr){
  #TestFonction VARval <- VARz; grid_plot <- grid_z; config <- config; VARname <- nameVARz
  #             VARattr <- "surface de mer par maille"; choicepal <- "pal_CPUE"; nbniv <- 10
  #             outformat <- "shp"; ltdc<-TRUE; lcont<-FALSE; lcity<-TRUE; lzoom<-config$Outputs$lzoom
  
  print(paste("# f_map_plot.R: Plot_VARz_grid2D : ",VARname,sep=""))
  # add the column "mesh to the IN matrix VARval -> VARplot
  # label de la ligne = numero de maille
  i_idz    <- f_idFieldDBTable_from_nameField("idmesh",config$DBData$Data,st_drop_geometry(G_grid),"grid")
  name_id  <- colnames(head(grid_plot))[i_idz]
  lplot <- grid_plot
  VARplot <- VARval
  #VARplot <- VARplot %>% mutate(!!VARname := ifelse(is.na(!!sym(VARname)), 0, !!sym(VARname))) # test lodier 010324 remplacement NA
  
  # ajout d'un nouvel attribut dans la grille correspondant ? la variable
  if(VARname %in% colnames(as.matrix(st_drop_geometry(grid_plot)))){
    print(paste("# f_map_plot.R:    -- the variable ",VARname," is already in the grid ",sep=""))
  }else{
    print(paste("# f_map_plot.R:    -- the variable ",VARname," is not in the grid ",sep=""))
    New_Attribut <- merge(grid_plot,VARplot,by.x=name_id,by.y="mesh",all.x=T,sort=FALSE)
    lplot <- New_Attribut
  }  
  
  # trace des plot ou creation de .shp
  #print(lplot[,dim(lplot)[2]])
  #lplot[,dim(lplot)[2]] <- as.numeric(lplot)[,dim(lplot)[2]]
  #st_geometry(lplot)
  #datelayer <- paste(substr(date(),21,24),substr(date(),5,7),substr(date(),9,10),substr(date(),12,13),substr(date(),15,16),sep="")
  datelayer <- gsub(" ", "", paste(substr(date(),21,24),substr(date(),5,7),substr(date(),9,10),substr(date(),12,13),substr(date(),15,16),sep="")) # test lodier 010324 si espace pose problème lors de l'écriture
  print(datelayer) # debug lodier 010324
  layername <- paste(VARname,datelayer,sep="_")
  #layername <- paste0(VARname,"_", datelayer,".gpkg") # lodier 290224 gpkg # lodie 010324 json
  
  print(lplot)
  #writeOGR(lplot, dsn = outdir, layer = layername, driver = 'ESRI Shapefile')
  #st_write(lplot, dsn = outdir, layer = layername, driver = 'ESRI Shapefile')
  st_write(lplot, dsn = paste0(graphdir, "/", layername, ".gpkg"), layer = layername, driver = 'GPKG',delete_layer = TRUE, layer_options = c("OVERWRITE=YES", "GEOMETRY_NAME=geometry", "FID=fid2")) # lodier 290224 GPKG # lodier 070324 FID #AEsq.2025 graphdir
  #write_sf(lplot, dsn = paste0(outdir, "/", layername), delete_layer = TRUE, append = FALSE) # lodier 010324
}