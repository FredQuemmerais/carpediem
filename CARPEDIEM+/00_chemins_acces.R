# CARPEDIEM - Script auxiliaire de paramétrage des chemins d'accès
# Ce script auxiliaire sert à indiquer les chemins d'accès (nom de dossier, nom du fichier de paramétrage)

# Date          : 20yy/mm/jj
# Version code  : 1.0
# Version R     : R-4.3.3
# Auteurs       : Alexis Esquerré (Cerema) d'après les travaux de :
# Auteurs       : Alice Vanhoutte-Brunier (OFB), Julien Barrere (OFB)
# Contributeurs : Frederic Quemmerais-Amice (OFB), Guilhem Autret (OFB)
# Correctrice   : Tiphaine Lodier (OFB)
# Contact, Documentation, Citations, Licence, Responsabilité      : Veuillez consulter les scripts principaux.

# MERCI DE RENSEIGNER/VERIFIER CI-DESSOUS LES ELEMENTS DEMANDES

    # Nom du fichier de paramétrage (au format ODS) avec l'extension .ods
fileparam         <- "fichier_parametrage_Norsaic - BSO_weighted.ods"

    # Nom du fichier avec les matrices de sensibilité
filesensi         <- "matrice_sensibilite_CD.ods"

    # Chemin d'accès absolu (p. ex. partant de votre répertoire C:/) vers le dossier Carpediem
userdir           <- "C:/Users/alexis.esquerre/Desktop/srv60 (mais local)/Norsaic/Norsaic_IC_2caseStudies/00_Carpediem+_Norsaic"

    # Dossier d'analyse (certaines versions de CD/certains dossiers ont plusieurs analyses différentes)
moddir            <- paste0(userdir,"")

    # Eléments de l'arborescence - Chaque dossier doit exister 
indir             <- paste0(moddir,"/INPUTS")           # Dossier des données d'entrée
outdir            <- paste0(moddir,"/OUTPUTS")          # Dossier où iront les graphiques produits par l'analyse
paramdir          <- paste0(moddir,"/PARAM")            # Dossier avec le fichier de paramétrage en ODS
sauvrepdir        <- paste0(moddir,"/SAUVREP")          # Dossier dans lequel un RData avec la grille sera conservé
sigdir            <- paste0(indir,"/SIG")               # Dossier SIG (rarement utilisé)

    # IMPORTANT
# Dans toutes les donnees carroyees d'activite (ou de pression), donner le meme nom au champ contenant
# l'ID de la cellule de carroyage (pas la ligne). Renseigner le nom de ce champ ci-dessous
champ_id_data_A   <- "idcell"
