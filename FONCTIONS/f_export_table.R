# Miniscript f_export_table

f_export_table <- function(in.con, in.scheme, in.grid, in.table, in.nametab){                                 # MAJ lodier 05 12 23
  print(paste0("Export de la table-----", in.nametab, "-------dans le schema------",in.scheme))
  index_export <- in.grid
  name_id <- "idmesh"
  New_Attribut <- merge(index_export,in.table,by.x="idmesh",by.y="idmesh",all.x=T,sort=FALSE)
  index_export <- New_Attribut 
  dbWriteSpatial(in.con, index_export, schemaname=in.scheme, tablename=in.nametab, replace=T)
}