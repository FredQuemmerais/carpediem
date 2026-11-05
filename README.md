# Carpediem V2

[![Link to discover EDITO](https://dive.edito.eu/badges/Powered-by-EDITO.svg)](https://dive.edito.eu/)

Outils de cartographie du risque d'effets cumulés des pressions anthropiques sur les habitats benthiques.


## Description

Projet          : Carpediem V2, évolution des codes développés dans le cadre du projet CARPEDIEM (2016-2018) <br/>
Cadre           : partenariat entre le laboratoire BOREA (Sorbonne Université, CNRS UMR 8067) et l'Office Français de la Biodiversité (OFB), convention de recherche n° OFB-24-1838 <br/>
Version R       : R 4.5.1 (voir `renv.lock`) <br/>
Auteurs version 1 : Alice Vanhoutte-Brunier, Julien Barrere <br/>
Contributeurs   : Frédéric Quemmerais-Amice, Guilhem Autret, Alexis Esquerre <br/>
Contacts        : Antonin Gimard (Cerema), Frédéric Quemmerais-Amice (OFB) <br/>


## Méthodologie
La méthode d'analyse est publiée dans l'article : <br/>
Quemmerais-Amice Frédéric, Barrere Julien, La Rivière Marie, Contin Gabriel, Bailly Denis, 2020. <br/>
A Methodology and Tool for Mapping the Risk of Cumulative Effects on Benthic Habitats. <br/>
Frontiers in Marine Science. 7:569205 <br/>
**https://doi.org/10.3389/fmars.2020.569205**  <br/>

Merci d'utiliser cette référence pour citer ces développements. <br/>


## Installation et utilisation
La documentation complète de la V2 (prérequis, formats des données et des matrices, configuration YAML, déroulé des analyses) est dans [`execution/README.md`](execution/README.md). <br/>

En résumé, depuis la racine du projet ouverte dans R (ou `carpediem_v2.Rproj` dans RStudio) :

```r
renv::restore()                   # une seule fois : installe les packages de renv.lock
source("execution/run_app.R")     # interface Shiny
source("execution/run_batch.R")   # mode batch, lit study/config/config.yml
```

Les données et la configuration d'une étude sont placées dans le dossier `study/` (sous-dossiers `inputs/` et `config/`). <br/>
Le passage d'une étude au format V1 (paramétrage xlsx et base PostgreSQL) vers le format V2 est décrit dans [`migration_v1_v2/`](migration_v1_v2/). <br/>
Le guide d'utilisation de la version 1 reste une référence pour la méthode : Barrere Julien, Autret Guilhem, Quemmerais-Amice Frédéric, 2022. Guide d'utilisation des outils d'analyses de données du projet Carpediem : cartographie du risque d'effets cumulés sur les habitats benthiques. Version 7.1 publique, octobre 2022. <br/>


## Utilisation sur EDITO
Carpediem est disponible sur [EDITO](https://dive.edito.eu/) (European Digital Twin of the Ocean), dans le catalogue officiel des processus du Datalab, catégorie « Metrics and indicators ». L'étude Mer du Nord est proposée comme jeu de données par défaut. <br/>
Les fichiers d'intégration (image Docker, chart Helm, procédure de déploiement, organisation des résultats) sont dans le dossier [`edito-docker/`](edito-docker/). <br/>
Exploitation au quotidien (lancer un run, ajouter une étude, gérer les runs, mettre à jour le processus et l'application web) : [`edito-docker/HANDBOOK.md`](edito-docker/HANDBOOK.md). <br/>

Les résultats produits sur EDITO sont à créditer avec la mention : « Généré à l'aide du service EDITO de l'Union européenne » (« Generated using EDITO of the European Union »). <br/>


## Licence
Ce programme est un logiciel libre diffusé sous les termes de la licence publique générale GNU Affero (GNU Affero General Public License, GNU AGPL) version 3. Le texte complet est dans le fichier [`LICENSE`](LICENSE). Vous pouvez redistribuer et/ou modifier le contenu de ce programme suivant les termes de la GNU AGPL version 3 telle que publiée par la Free Software Foundation. <br/>

## Responsabilité
Ce programme est diffusé dans l'espoir qu'il sera utile. Les auteurs, les contributeurs et l'OFB n'offrent aucune garantie de fonctionnement et de résultat liée à l'utilisation de ce programme. Ils ne peuvent en aucun cas être tenus responsables des interprétations et des conclusions qui pourraient être faites à partir des résultats issus de l'utilisation de ces programmes. <br/>
