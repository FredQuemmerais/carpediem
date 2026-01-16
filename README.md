# Carpediem +


## Description

Projet          : codes développés dans le cadre du projet CARPEDIEM (2016-2018) <br/>
Version         : 1.1 <br/>
Version R       : R-3.5.1 <br/>
Auteurs         : Alice Vanhoutte-Brunier, Julien Barrere <br/>
Contributeurs   : Frederic Quemmerais-Amice, Guilhem Autret, Alexis Esquerre <br/>
Contact         : cerema... <br/>
Cerema <br/>


## Méthodologie
La méthode d'analyse est publiée dans l'article : <br/>
Quemmerais-Amice Frédéric, Barrere Julien, La Rivière Marie, Contin Gabriel, Bailly Denis, 2020. <br/>
A Methodology and Tool for Mapping the Risk of Cumulative Effects on Benthic Habitats. <br/>
Frontiers in Marine Science. 7:569205 <br/>
**https://doi.org/10.3389/fmars.2020.569205**  <br/>

Merci d'utiliser cette référence pour citer ces développements. <br/>


## Installation et utilisation
**Lire la documentation technique disponible dans le dossier "documentation" :** <br/>
Barrere Julien, Autret Guilhem, Quemmerais-Amice Frédéric, 2022. [Guide d'utilisation des outils d'analyses de données du projet Carpediem: cartographie du risque d'effets cumulés sur les habitats benthiques](https://gitlab.ofb.fr/Frederic.QUEMMERAIS-AMICE/carpediem/-/blob/bc6d65d05dd9c994a3e73c033189cc5c5a8f9924/documentation/Barrere_et_al_2022_guide_utilisation_outils_carpediem_benthos_v7-1_public.pdf). Version 7.1 publique, octobre 2022, 149 pages.

**Téléchargez le dossier "CODES_CARPEDIEM" contenu dans le dossier "application_carpediem" sur votre ordinateur, ne changez pas le nom du dossier créé, conserver l'ensemble des sous dossiers et des fichiers (R et xlsx).** <br/>
<br/>

Le paramétrage de toutes les analyses s'effectue dans le fichier .xlsx contenu dans le dossier "PARAM". <br/>
**Lire l'onglet "a_lire" avant toute opération.** <br/>

**Déroulement général de l'analyse avec le script "analyse_etape1_refc_publication_v1.R"** <br/>
Lire le guide d'utilisation pour obtenir des informations plus détaillées. <br/>
Le script réalise l'analyse par défaut (c'est à dire cumulative) du risque d'effets cumulés entre des pressions anthropiques et des habitats benthiques. <br/>

Les paramétrages de l'analyse sont renseignés dans un fichier Excel contenu dans le dossier "PARAM" du dossier "CODES_CARPEDIEM". <br/>
Dans le code, seuls les paramètres **"fileparam"** et **"userdir"** correspondant respectivement au nom du fichier .xlsx de paramétrage et au chemin d'accès du dossier "CODES_CARPEDIEM" sont a renseigner aux lignes 66 et 69. <br/>
Lecture et import des tables de données sources contenues dans une base de données PostgreSQL (locale et/ou distante). <br/>
Export des résultats dans la base de données, dans de nouvelles tables, dans un schéma à spécifier. <br/>
<br/>
Les différentes étapes du script sont : <br/>

1. LECTURE DE FONCTIONS DECRITES DANS DU CODE SOURCE <br/>
2. LECTURE DES PARAMETRES DU FICHIER EXCEL ET ECRITURE DANS LA LISTE config <br/>
3. IMPORT DES TABLES DE LA BASE DE DONNEES <br/>
4. GESTION DES INDICES ET DIMENSIONS, stockage dans des listes <br/>
5. DONNEES HABITATS ET SENSIBILITE <br/>
6. CARTOGRAPHIE DES ACTIVITES <br/>
7. CARTOGRAPHIE DES PRESSIONS <br/>
8. CARTOGRAPHIE DU RISQUE D'EXPOSITION PRESSION / MULTI-PRESSIONS <br/>
9. CALCUL DU RISQUE D'EFFETS // RISQUE D'EFFETS CONCONMITANTS <br/>
10. REALISATION ET SAUVEGARDE DE GRAPHIQUES REPRESENTATIFS DES RESULTATS <br/>
11. EXPORT DES RESULTATS SOUS FORME DE TABLES DANS LA BASE DE DONNEES <br/>
12. CALCUL D'UN INDICE DE CONFIANCE POUR LES RELATIONS THEORIQUES <br/>
<br/>
<br/>

**Déroulement général de l'analyse avec le script "analyse_etape2_monte_carlo_publication.R":** <br/>
Lire le guide d'utilisation pour obtenir des informations plus détaillées. <br/>
Le script réalise l'analyse du risque d'effets cumulés entre des pressions anthropiques et des habitats benthiques en réalisant des simulations de Monte-Carlo qui permettent à chaque simulation de choisir des valeurs au hasard pour 7 critères intervenants dans le calcul. <br/>
Il est imperatif d'avoir réalisé l'analyse étape 1 (script "analyse_etape1_refc_publication_v1.R") au préalable. <br/>
Les paramétrages de l'analyse doivent être renseignés dans le fichier de paramétrage .xlsx utilisé à l'étape précédente et stocké dans le dossier "PARAM" du dossier "CODES_CARPEDIEM". <br/>
Seul l'onglet "simulation_monte_carlo" est a renseigner. <br/>
Dans le code, seuls les paramètres **"fileparam"** et **"userdir"** correspondant respectivement au nom du fichier .xlsx de paramétrage et au chemin d'accès du dossier "CODES_CARPEDIEM" sont a renseigner aux lignes 63 et 65. <br/>
Lecture et import des tables de données sources contenues dans une base de données PostgreSQL (locale et/ou distante). <br/>
Export des résultats dans la base de données, dans de nouvelles tables, dans un schéma à spécifier. <br/>
<br/>

Les différentes étapes du script sont : <br/>
1. IMPORT DES DONNEES ACTIVITES ET HABITATS <br/>
2. CODAGE DES FONCTIONS ASSOCIEES AUX FACTEURS <br/>
3. REALISATION DES SIMULATIONS <br/>
<br/>
<br/>

**Déroulement général de l'analyse avec le script "analyse_etape3_graphiques_publication.r" :** <br/>
Lire le guide d'utilisation pour obtenir des informations plus détaillées. <br/>
Le script réalise des analyses statistiques et des graphiques à partir des resultats de l'étape 1 et sur des zones d'intérêts définis par l'utilisateur. <br/>
Il est imperatif d'avoir réalisé l'analyse étape 1 (script "analyse_etape1_refc_publication_v1.R") au préalable. <br/>
Les paramétrages de l'analyse doivent être renseignés dans le fichier de paramétrage .xlsx utilisé à l'étape précédente et stocké dans le dossier "PARAM" du dossier "CODES_CARPEDIEM". <br/>
Seul l'onglet "statistiques_par_zones" est a renseigner. <br/>

Dans le code, seuls les paramètres **"fileparam"** et **"userdir"** correspondant respectivement au nom du fichier .xlsx de paramétrage et au chemin d'accès du dossier "CODES_CARPEDIEM" sont a renseigner aux lignes 64 et 66. <br/>
Lecture et import des tables de données sources contenues dans une base de données PostgreSQL (locale et/ou distante). <br/>
Export des graphiques au format image dans le dossier "OUTPUTS/graphs". <br/>
<br/>

Les différentes étapes du script sont : <br/>
1. LECTURE DU FICHIER DE PARAMETRAGE ET IMPORT DES DONNEES <br/>
2. PRODUCTION D'UNE MATRICE ACTIVITE PRESSION REDUITE AUX ACTIVITES ET PRESSIONS PRISES EN COMPTE DANS L'EVALUATION <br/>
3. CREATION DES GRAPHES <br/>

## Licence
Ce programme est un logiciel libre diffusé sous les termes de la licence publique générale GNU (GNU General Public License, GNU GPL) version 3 ou toute version ultérieure. Vous pouvez consulter le guide rapide de la GNU GPL v3 sur https://www.gnu.org/licenses/quick-guide-gplv3.fr.html. Vous pouvez redistribuer et/ou modifier le contenu de ce programme suivant les termes de la GNU GPL version 3 ou ultérieure telle que publiée par la Free Software Foundation. Consultez la GNU General Public License pour plus de details.

## Responsabilité
Ce programme est diffusé dans l'espoir qu'il sera utile. Les auteurs, les contributeurs et l'OFB n'offrent aucune garantie de fonctionnement et de résultat liée à l'utilisation de ce programme. Ils ne peuvent en aucun cas être tenu responsables des interprétations et des conclusions qui pourraient être faites à partir des résultats issus de l'utilisation de ces programmes. <br/>

## Statut du projet
Aucun nouveau développement n'est prévu prochainement.
