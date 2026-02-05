## Code pour passer la matrice d'amplitudes en categories a une version individuelle 

# on a une matrice avec des categories de pressions vs des categories d'amplitude
# on a aussi une matrice AP d'origine ou certaines activites d'une categorie a forte
# amplitude n'exercent pas la pression etudiee

# on a donc besoin d'une transformation qui "deplie" les categories ==> table d'amplitudes
# que l'on multiplie ensuite avec notre matrice AP normale : les 1 vont prendre la valeur d'amplitude
# et les 0, vont rester 0. Et les NA ? Bah resteront NA.

fich_amplitudes <- "ampli_par_cat.ods"
ou_mettre_resultat <- "Matrice_multipliee.ods"
source("././00_chemins_acces.R")

library(readODS)
library(tidyverse)

# On lit les tableaux. On extrait les correspondances categories-individuelles
categorisees <- list()
categorisees$Amplitudes <- read_ods(paste0(paramdir, "/",fich_amplitudes), sheet = 2) %>% 
  rename(Activite_cat = 1)

categorisees$Pressions <- read_ods(paste0(paramdir, "/",fich_amplitudes), sheet = 3) %>% 
  pivot_longer(everything(),
               names_to = "categorie",
               values_to = "valeurs",
               values_drop_na = TRUE) 
# %>% 
#   group_by(categorie) %>% 
#   mutate(pressions = list(valeurs)) %>%
#   slice_head(n = 1) %>%
#   ungroup() %>% 
#   select(-valeurs)

categorisees$Activites <- read_ods(paste0(paramdir, "/",fich_amplitudes), sheet = 4) %>% 
  rename(Activite_cat = 1, 
         Activs_indiv = 2)

categorisees$MatAP_depart <- read_ods(paste(paramdir, fileparam, sep="/"), sheet = 6)

MatAP_bnr <- categorisees$MatAP_depart %>% 
  rename(Type = 1,
         Code_A = 2,
         Activite_nom = 3,
         Phase = 4) %>% 
  dplyr::filter(Type == "Final") %>% 
  column_to_rownames(var = "Code_A") %>% 
  dplyr::select(-c("Type","Activite_nom","Phase")) 

MatAmpli <- categorisees$Activites %>% 
  left_join(categorisees$Amplitudes, by = "Activite_cat") %>% 
  dplyr::select(-Activite_cat) %>% 
  pivot_longer(cols = -Activs_indiv, names_to = "categorie", values_to = "Valeur") %>% 
  pivot_wider(names_from = Activs_indiv, values_from = Valeur) %>% 
  right_join(categorisees$Pressions, by = "categorie") %>% 
  dplyr::select(-categorie) %>% 
  column_to_rownames("valeurs") %>% 
  t() %>% 
  as.data.frame()

# MatAmpli <- MatAmpli[rownames(MatAP_bnr), colnames(MatAP_bnr)] # Cette methode ne fonctionne pas si on retire certaines pressions/activites de MatAmpli

# Methode alternative : reduire MatAmpli aux colonnes et pressions dans MatAP_bnr
print(c(paste("Activites non etudiees :", length(setdiff(rownames(MatAP_bnr), rownames(MatAmpli))) ),
        setdiff(rownames(MatAP_bnr), rownames(MatAmpli))))

print(c(paste("Pressions non etudiees :", length(setdiff(colnames(MatAP_bnr), colnames(MatAmpli))) ),
        setdiff(colnames(MatAP_bnr), colnames(MatAmpli))))

MatAmpli <- MatAmpli %>%
  rownames_to_column("rowname") %>%
  dplyr::filter(rowname %in% rownames(MatAP_bnr)) %>%
  column_to_rownames("rowname") %>%
  dplyr::select(any_of(colnames(MatAP_bnr)))

# Pour eviter tout problème on reduit aussi MatAP_bnr
MatAP_bnr <- MatAP_bnr %>% 
  rownames_to_column("rowname") %>%
  dplyr::filter(rowname %in% rownames(MatAmpli)) %>%
  column_to_rownames("rowname") %>%
  dplyr::select(any_of(colnames(MatAmpli)))

# On prepare les matrices et on realigne MatAmpli sur MatAP_bnr
MatAmpli[] <- lapply(MatAmpli, as.numeric)
MatAmpli <- as.matrix(MatAmpli)
MatAmpli <- MatAmpli[rownames(MatAP_bnr), colnames(MatAP_bnr), drop=FALSE]
MatAmpli <- 10^(MatAmpli)

MatAP_bnr[] <- lapply(MatAP_bnr, as.numeric)
MatAP_bnr <- as.matrix(MatAP_bnr)

## Multiplication AP * amplitudes ---------------------------------------------

MatAP_finale <- MatAP_bnr * MatAmpli %>% 
  as.data.frame()

## Integrer dans la structure d'une Matrice AP normale ------------------------
En_tete <- categorisees$MatAP_depart[1,] %>% 
  rename(
    Type = `0...1`,
    Code_A = `0...2`,
    Activite = `0...3`,
    Phase = `0...4`
  ) %>%
  dplyr::select(Type, Code_A, Activite, Phase, colnames(MatAP_finale))

MatAP_IC <- categorisees$MatAP_depart %>%
  dplyr::filter(`0...1` == "IC") %>% 
  dplyr::filter(`0...2` %in% rownames(MatAP_finale)) %>%
  rename(
    Type = `0...1`,
    Code_A = `0...2`,
    Activite = `0...3`,
    Phase = `0...4`
  ) %>%
  dplyr::select(Type, Code_A, Activite, Phase, colnames(MatAP_finale))

MatAP_IC[, 5:ncol(MatAP_IC)] <- lapply(MatAP_IC[, 5:ncol(MatAP_IC)], as.numeric) 

MatAP_finale <- MatAP_finale %>% 
  rownames_to_column("Code_A") %>% 
  left_join(dplyr::select(MatAP_IC, Type, Code_A, Activite, Phase), by = "Code_A") %>% 
  dplyr::select(colnames(MatAP_IC)) %>% 
  mutate(Type = "Final") %>% 
  rbind(MatAP_IC) %>% 
  mutate(
    Code_A = factor(Code_A, levels = unique(Code_A)),
    Type = factor(Type, levels = c("Final", "IC"))
  ) %>%
  arrange(Code_A, Type) 



write_ods(MatAP_finale, paste0(paramdir, "/",ou_mettre_resultat), sheet = "matrice_activites_pressions")
write_ods(En_tete, paste0(paramdir, "/",ou_mettre_resultat), sheet = "en_tete_a_inserer", append = T)

