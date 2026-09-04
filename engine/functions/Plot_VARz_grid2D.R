# -------------------------------------------------------------------
# Plot_VARz_grid2D : export (optionnel) d'une variable sur une grille sf
#   VARval    : data.frame avec colonnes "idmesh" + VARname (ou "value" renommée en VARname)
#   grid_plot : sf avec colonne idmesh + géométrie
#   config    : attend config$Outputs$outputsdir et config$Outputs$write_maps
#   VARname   : nom de la variable à joindre/écrire
#   VARattr   : (non utilisé ici, conservé pour compat legacy)
# -------------------------------------------------------------------
Plot_VARz_grid2D <- function(VARval, grid_plot, config, VARname, VARattr, outputsdir = NULL) {
  
  verbose <- isTRUE(config$Outputs$verbose_maps)
  
  if (verbose) message("# f_map_plot.R: Plot_VARz_grid2D : ", VARname)
  
  # --- Répertoire de sortie (fallback tempdir) ---
  if (is.null(outputsdir) || !nzchar(outputsdir)) {
    outputsdir <- tempdir()
  }
  # --- Guards minimaux ---
  if (!inherits(grid_plot, "sf")) stop("Plot_VARz_grid2D: grid_plot doit être un objet sf.")
  if (!("idmesh" %in% names(grid_plot))) stop("Plot_VARz_grid2D: grid_plot doit contenir la colonne 'idmesh'.")
  if (!is.data.frame(VARval)) stop("Plot_VARz_grid2D: VARval doit être un data.frame.")
  if (!("idmesh" %in% names(VARval))) stop("Plot_VARz_grid2D: VARval doit contenir une colonne 'idmesh'.")
  if (!(VARname %in% names(VARval))) {
    if ("value" %in% names(VARval)) {
      names(VARval)[names(VARval) == "value"] <- VARname
    } else {
      stop("Plot_VARz_grid2D: VARval doit contenir la colonne '", VARname, "' (ou 'value').")
    }
  }
  
  # --- Jointure de la variable à la grille ---
  if (VARname %in% names(sf::st_drop_geometry(grid_plot))) {
    if (verbose) message("# f_map_plot.R:    -- the variable ", VARname, " is already in the grid ")
    lplot <- grid_plot
  } else {
    if (verbose) message("# f_map_plot.R:    -- the variable ", VARname, " is not in the grid ")
    lplot <- merge(grid_plot, VARval, by.x = "idmesh", by.y = "idmesh", all.x = TRUE, sort = FALSE)
  }
  
  # --- Nom unique (évite collisions) ---
  datelayer <- format(Sys.time(), "%Y%m%d_%H%M%S")
  layername <- paste(VARname, datelayer, sep = "_")
  
  # --- Ecriture optionnelle ---
  if (isTRUE(config$Outputs$write_maps)) {
    sf::st_write(
      lplot,
      dsn = file.path(outputsdir, paste0(layername, ".gpkg")),
      layer = layername,
      driver = "GPKG",
      delete_layer = TRUE,
      layer_options = c("OVERWRITE=YES", "GEOMETRY_NAME=geometry", "FID=fid2")
    )
  }
  
  invisible(lplot)
}
