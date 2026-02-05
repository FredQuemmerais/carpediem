# Ici nous essayons de faire en sorte de creer une carte d'inadequation des activites

inadeq_act_z <- grid_z["idmesh"] %>% st_drop_geometry() %>% filter(idmesh %in% HabBenth_z$E_idmesh)

press_ag_corresp <- data.frame(
  Code = config$P_code, 
  Name = config$P_name, 
  CodeAgrege = config$P_code_ag,
  NomAgrege = config$P_name_ag
) %>% 
  mutate(CodeAgrege = if_else(is.na(CodeAgrege), Code, CodeAgrege),
         NomAgrege = if_else(is.na(NomAgrege), Name, NomAgrege)) %>% 
  group_by(CodeAgrege) %>%
  mutate(CodeAll = list(Code)) %>% 
  slice_head(n = 1) %>%
  ungroup() %>% 
  dplyr::select(CodeAgrege, CodeAll) %>% 
  column_to_rownames("CodeAgrege")

# Partie 1 - Inadequation theorique des activites ------------------------------
for (activite_i in config$A_code[config$A_code %in% rownames(MatAP)]) {
  if(config$Calc$agreg_sensib==1){
    press_ag_corresp[activite_i] <- sapply(1:nrow(press_ag_corresp), function(i) {
      max(as.numeric( MatAP[activite_i, press_ag_corresp$CodeAll[[i]]]) )
    }  )
  } else if(config$Calc$agreg_sensib==2){
    press_ag_corresp[activite_i] <- sapply(1:nrow(press_ag_corresp), function(i) {
      mean(as.numeric( MatAP[activite_i, press_ag_corresp$CodeAll[[i]]]) )
    }  )
  } 
  
  inadeq_Ai <- Mat_Sensib_Pj %>%
    st_drop_geometry() %>%
    dplyr::select(-E_surfmer) %>% 
    mutate(across(
      .cols = any_of(row.names(press_ag_corresp) ), 
      .fns = ~ .x * press_ag_corresp[cur_column(), activite_i]
    )) %>%
    rowwise() %>%
    mutate(sum = sum(c_across(-E_idmesh), na.rm = TRUE)) %>%
    ungroup() %>% 
    dplyr::select(E_idmesh, sum)
  
  inadeq_act_z[activite_i] <- inadeq_Ai$sum
}

# VARz <- inadeq_act_z
# colnames(VARz)[1] <- "mesh"
# nameVARz <- "Inadequation_activites"
# title <- ("Indice d'inadequation des activites")
# checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)

inadeq_act_z <- inadeq_act_z %>% left_join(grid_z["idmesh"], by = "idmesh")
st_write(inadeq_act_z, paste0(graphdir,"/Inadequation_activites.gpkg"), layer = "inadeq_activ", append = F)



# Partie 2 - Discordance plan existant vs. inadequations -----------------------

inadeq_plan <- grid_z["idmesh"] %>% st_drop_geometry() %>% filter(idmesh %in% inadeq_act_z$idmesh)
for (activite_i in unique(act_data_corresp$Code)) {
  inadeq_plan[[activite_i]] <- rowSums(
    A_zi_lognorm[A_zi_lognorm$mesh %in% inadeq_act_z$idmesh, paste0(act_data_corresp$data_col[act_data_corresp$Code == activite_i], "_norm"), drop = FALSE]
    , na.rm = TRUE)
  
  inadeq_plan[[activite_i]] <- inadeq_plan[[activite_i]] * inadeq_act_z[[activite_i]]
}

# VARz <- inadeq_plan
# colnames(VARz)[1] <- "mesh"
# nameVARz <- "Inadequation_existant"
# title <- ("Indice d'inadequation des activites existantes")
# checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)
inadeq_plan <- inadeq_plan %>% left_join(grid_z["idmesh"], by = "idmesh")
st_write(inadeq_plan, paste0(graphdir,"/Inadequation_activites.gpkg"), layer = "inadeq_plan_existant", append = F)
