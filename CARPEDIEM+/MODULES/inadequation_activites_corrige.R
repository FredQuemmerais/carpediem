# Tests. Ne pas manger.
activite_i <- "A_2_P_1"

# Cette part est en fait la noucle "for activite_i in liste d'activites
pruss_ompli <- press_ag_corresp[activite_i]
pruss_ompli <- pruss_ompli %>% filter(A_2_P_1 >0)
pruss_cude <- rownames(pruss_ompli)

inadeq_Ai <- Mat_Sensib_Pj %>%
  st_drop_geometry() %>%
  dplyr::select(-E_surfmer) %>% 
  dplyr::select(E_idmesh,any_of(pruss_cude))
inaduq_Ai <- inadeq_Ai

culunes <- colnames(inadeq_Ai)
culunes <- culunes[culunes != "E_idmesh"]

for(culone in culunes){
  inadeq_Ai[[culone]]<-inadeq_Ai[[culone]] * pruss_ompli[[culone, activite_i]]
}

inadeq_Ai <- inadeq_Ai %>%
  rowwise() %>%
  mutate(sum = sum(c_across(-E_idmesh), na.rm = TRUE)) %>%
  ungroup() %>% 
  dplyr::select(E_idmesh, sum)

inaduq_Ai <- inadeq_Ai %>% 
  rename(idmesh = E_idmesh) %>% 
  left_join(grid_z["idmesh"], by = "idmesh")

st_write(inaduq_Ai, 
         "C:/Users/alexis.esquerre/Desktop/srv60 (mais local)/Norsaic/Norsaic_IC_2caseStudies/00_Carpediem+_Norsaic/OUTPUTS/EUGNS_test/Inadequation_activites_EUGNSgood.gpkg",
          layer = "inadeq_activ_ChaPel", append = F)






# Partie 2 - Discordance plan existant vs. inadequations -----------------------

# inadoq_plan <- inadeq_plan
# 
# inadeq_plan <- grid_z["idmesh"] %>% st_drop_geometry() %>% filter(idmesh %in% inadeq_act_z$idmesh)
# for (activite_i in unique(act_data_corresp$Code)) {
#   inadeq_plan[[activite_i]] <- rowSums(
#     A_zi_lognorm[A_zi_lognorm$mesh %in% inadeq_act_z$idmesh, paste0(act_data_corresp$data_col[act_data_corresp$Code == activite_i], "_norm"), drop = FALSE]
#     , na.rm = TRUE)
# 
#   inadeq_plan[[activite_i]] <- inadeq_plan[[activite_i]] * inadeq_act_z[[activite_i]]
# }


Azilog_bso <- A_zi_lognorm[A_zi_lognorm$mesh %in% inaduq_Ai$idmesh,] %>% 
  select(idmesh = mesh, A_2_P_1...A_19_P_1_norm) %>% #A ce point on a restreint Azilog à la colonne du trafic maritime et aux lignes de la zone d'étude
  # Comme on a un jeu de donnees pour le trafic mar specifiquement (looking at you, "Pelagic trawls and seines"), on ne garde que la colonne d'activite
  # theorique A_68_P_1
  # Si on a un jeu de donnees qui regroupait A2P1 et A19P1, on doit garder la colonne A2P1_A19P1 et supposer qu'il y a 99.9% de A2P1 dedans
  # et donc multiplier cette colonne par l'inadéquation de A2P1
  left_join(inaduq_Ai, by="idmesh")


Azilog_bso[["inadeq"]] <- Azilog_bso[["A_2_P_1...A_19_P_1_norm"]] * Azilog_bso[["sum"]]




# VARz <- inadeq_plan
# colnames(VARz)[1] <- "mesh"
# nameVARz <- "Inadequation_existant"
# title <- ("Indice d'inadequation des activites existantes")
# checkplot <- Plot_VARz_grid2D(VARz,grid_z,config, nameVARz,title)
### inadeq_plan <- inadeq_plan %>% left_join(grid_z["idmesh"], by = "idmesh")

st_write(Azilog_bso,
         "C:/Users/alexis.esquerre/Desktop/srv60 (mais local)/Norsaic/Norsaic_IC_2caseStudies/00_Carpediem+_Norsaic/OUTPUTS/EUGNS_test/Inadequation_activites_EUGNSgood.gpkg",
         layer = "inadeq_plan_existant_ChaPel", append = F)



