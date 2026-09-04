---
output:
  html_document: default
  pdf_document: default
---
# Carpediem - Pipeline d'analyse des risques d'effets cumulés

## Introduction

### Contexte

Le projet **Carpediem V2** est conduit en partenariat entre le Laboratoire de Biologie des Organismes et des Écosystèmes Aquatiques (BOREA, Sorbonne Université, CNRS UMR 8067) et l'Office Français de la Biodiversité (OFB), dans le cadre de la convention de recherche n° OFB-24-1838. Ce pipeline s'inscrit dans la première sous-action portant sur l'amélioration des différents modules de l'analyse et constitue un des livrables méthodologiques attendus au T2 2026.

Le projet porte sur la cartographie des risques d'effets cumulés des pressions anthropiques (physiques, chimiques et biologiques) sur les habitats benthiques. L'analyse vise à modéliser le potentiel d'impact résultant de l'exposition des écosystèmes à plusieurs pressions générées par les activités humaines observées, sans préjuger directement de l'état des habitats ni des impacts effectivement réalisés. Il s'agit d'un outil d'aide à la décision pour la gestion intégrée du milieu marin, permettant d'identifier et de hiérarchiser les zones à enjeu.

Carpediem repose sur la spatialisation des intensités d'activités anthropiques, combinées et traduites - via une matrice de lien - en intensités de pressions. Ces données d'exposition sont ensuite croisées avec la cartographie des fonds marins, au regard d'une matrice de sensibilité des habitats benthiques aux pressions, afin de produire un indice spatialisé de risque d'effets cumulés.

### ⚠️ Non-comparabilité des résultats entre analyses

La normalisation des intensités d'activités est réalisée **à l'échelle de l'étude**. Cela implique une **dépendance forte au jeu de données considéré**, et **limite la comparabilité des résultats entre études ou territoires** : les niveaux d'intensité d'activité - et par conséquent de pression - sont relatifs au contexte d'analyse, et non à des référentiels communs.

**Pour permettre une comparabilité entre analyses**, un module optionnel de normalisation d'après des valeurs d'intensité de pressions génériques doit être activé.

### Schéma de structure du projet

```
Carpediem/
│
├── renv/ #bibliothèque locale des packages versionnés
│
├── execution/
│   ├── run_app.R # point d'entrée Shiny
│   └── run_batch.R # point d'entrée batch
│
├── engine/ 
│   ├── functions/ #fonctions helpers
│   └── steps # scripts de calcul
│
└── study/
    ├── inputs/ # fichiers .gpkg (mode flat_files)
    ├── outputs/ # résultats, logs, rapport HTML
    └── config/
       ├── config.yml # configuration de référence
       ├── runs/ # configs horodatées générées par l'app
       ├── matrice_act_press.ods
       └── matrice_sensi_hab_press.ods
```

---

## Prérequis

### Version R

- **R version 4.5.1** (recommandée - voir `renv.lock` pour la version exacte utilisée en développement)
- [renv](https://rstudio.github.io/renv/) pour la gestion des packages (voir installation ci-dessous)

### Installation des librairies (une seule fois)

1. Ouvrir `carpediem_v2.Rproj` dans RStudio : `renv` s'active automatiquement.
2. Dans la console R, lancer **renv::restore()**
3. Confirmer l'installation des packages (répondre `y` si demandé). Cette étape télécharge et installe, dans une bibliothèque isolée propre au projet, les mêmes versions de packages que celles utilisées en développement (listées dans `renv.lock`).

Cette commande n'est à exécuter qu'une seule fois, lors de la première mise en place du projet sur une nouvelle machine. Elle n'est pas nécessaire avant chaque lancement de l'analyse.

### Formatage des données et sources possibles

Le pipeline peut importer les couches spatiales depuis **deux origines**, contrôlées par `config$db$data_source` : <br>

| Valeur | Origine |
|---|---|
| `postgis` *(par défaut)* | Base de données PostgreSQL/PostGIS |
| `flat_files` | Fichiers locaux déposés dans `study/inputs/` |

<br>

En mode `flat_files`, les données doivent être déposés dans `study/inputs/` et nommés d'après le `Table_code` déclaré dans `config$datasets`. <br>
Le format .gpkg est recherché en priorité ; .shp est utilisé en repli si le .gpkg correspondant est absent. <br>
Nommage des fichiers locaux (mode `flat_files`) :

```
study/inputs/
├── grid.gpkg # Table_code = "grid"
├── habitats.gpkg # Table_code déclaré pour Data_type = "E"
└── chalutage.gpkg # Table_code déclaré pour un dataset Data_type = "A"
```
<br>

En mode `postgis`, les données doivent être rangées dans le schéma et la table déclaré dans `config$datasets`. <br>

Types de données attendus (`Data_type`)
<br>

| `Data_type` | Rôle | Cardinalité |
|---|---|---|
| `G` | Grille d'analyse (`Table_code = "grid"`) | **au moins 1 ligne** - grille de base + grille zonale si besoin |
| `E` | Habitats benthiques | **exactement 1 ligne** |
| `A` | Activités anthropiques | **au moins 1 ligne** - plusieurs tables possibles, chacune avec un `Table_code` unique |

Colonnes obligatoires par dataset :

- `Table_code`, `Data_type`, `Source` - toujours requises
- `DB_table` - requise en mode base de données
- `DB_schema` - requise si `Source = postgis` en mode base de données
- `id_col`, `geom_col` - identifiant et colonne géométrie de la table (utilisés pour les jointures spatiales)
- `intensity_cols` - pour les datasets `Data_type = A` : liste des colonnes d'intensité d'activité à exploiter
- `Field` *(optionnel)* - si renseignée, le pipeline vérifie que le champ existe bien dans la table PostgreSQL avant de lancer l'analyse <br>
<br>

#### Vérifications automatiques (`step0_checkup`)

Avant le calcul, le pipeline exécute un contrôle de cohérence qui vérifie notamment : <br>
- l'existence des dossiers clés du projet, <br>
- l'unicité des `Table_code` pour les tables d'activités, <br>
- la présence des colonnes obligatoires selon le mode choisi, <br>
- (en mode base de données, si `Field` est renseignée) l'existence effective du champ dans PostgreSQL. <br>

Tout problème détecté est remonté de façon structurée (chemins d'accès, schémas/tables/champs, données géographiques) sans nécessairement interrompre l'exécution - à consulter dans les logs avant d'analyser les résultats.

### Formatage des matrices (ODS)

Le pipeline s'appuie sur deux fichiers `.ods`, déposés dans `study/config/`<br>
<br>

#### Matrice_act_press.ods

Ce fichier doit contenir 5 onglets obligatoires :

| Onglet | Contenu |
|---|---|
| `CCR_mediane` | Coefficients de lien activités - pressions (scénario médian) - référence |
| `CCR_precaution` | Coefficients de lien, scénario précaution |
| `CCR_binaire` | Coefficients de lien binaires (V1) |
| `typologie_activites` | Référentiel hiérarchique des activités |
| `typologie_pressions` | Référentiel hiérarchique des pressions |

**Structure des onglets `CCR_*`** (identique sur les 3) : <br>
- Colonnes obligatoires : `Code_A` (code activité) et `Type` <br>
- Une ou plusieurs colonnes `Pr_*` (une par pression), contenant les coefficients <br>
- Chaque activité doit avoir deux lignes : une avec `Type = "lien"` (coefficients de contribution relative, valeurs attendues dans [0,1]) et une avec `Type = "ic"` (intervalle de confiance associé) <br>
- Colonnes `Niveau` et `Nom_activite`  <br>
<br>
L'ordre des colonnes n'a pas d'importance. <br>

**Structure de l'onglet `typologie_pressions`** :

| Colonne | Description |
|---|---|
| `Code_P` | Code de la pression |
| `Niveau` | Profondeur hiérarchique (1 = racine, jusqu'à 4 = feuille fine) |
| `Nom_pression` | Libellé |
| `parent` | `Code_P` du parent hiérarchique (vide pour une racine) |
| `mode_calcul` | Mode de calcul de la pression - voir ci-dessous |
| `demonstrateur` | Indicateur (0/1) |

Modes de calcul (`mode_calcul`) : <br>
**1** - pression estimée à partir des activités : Σ(γᵢⱼ · Aᵢ_norm) <br>
**2** - alimentée depuis des données externes *(fonctionnalité à venir)* <br>
**3** - reconstruite récursivement à partir de ses pressions filles (agrégation bottom-up) <br>

`Niveau` et `mode_calcul` sont indépendants : une pression "feuille" (mode 1 ou 2) peut se situer à n'importe quel niveau hiérarchique.<br>
<br>

#### Matrice_sensi_hab_press.ods

Ce fichier peut comporter un ou plusieurs onglets - versions de la matrice de sensibilité (*e.g.* mode d'agrégation des scores de sensibilité `precaution` et `median`), de structure similaire à celle des matrices activités-pressions : <br>
- Colonne `Type` : `sensibilite` / `IC`<br>
- Colonne `Code_H` : code habitat <br>
- Colonnes `Pr_*` : une par pression, valeurs de sensibilité (échelle 0–5) <br>

Une convention importante à connaître pour la préparation des matrices : la valeur **`99`** est interprétée par le pipeline comme un code **"donnée absente" (nodata)**, et non comme une valeur de sensibilité réelle. Un seuil configurable (`config$Calc$th_sensib`, en %) détermine la proportion de surface "nodata" tolérée par maille avant d'exclure la valeur agrégée de sensibilité cumulée.<br>
<br>

#### Tolérance de casse

⚠️ Les noms de colonnes (`Type`, `Code_A`, `Code_P`, `Niveau`, etc.) et les valeurs de la colonne `Type` (`lien`/`ic`) sont **insensibles à la casse** (comparaison via minuscules après suppression des espaces) dans `matrice_act_press.ods`. Cela dit, l'**orthographe exacte** des en-têtes reste requise (pas de correspondance approximative/floue).

---

## Lancement d'une analyse via l'interface Shiny

### Démarrage

```r
source("execution/run_app.R")
```

L'application s'ouvre dans le navigateur. Elle permet de configurer l'intégralité d'une analyse via des onglets dédiés, puis génère un fichier `config.yml` horodaté dans `study/config/runs/` avant de lancer le calcul.

### Déroulé d'une analyse

1. Compléter les onglets de configuration (voir détail ci-dessous), du premier au dernier.
2. Cliquer sur **"Enregistrer et lancer l'analyse"** : la configuration est écrite dans un fichier YAML horodaté, puis le calcul démarre.
3. Suivre la progression dans l'onglet **Logs**.
4. Une fois le calcul terminé, cliquer sur **"Ouvrir le rapport HTML"** pour consulter la synthèse de l'analyse.

### Onglet "Projet & DB"

**Sous-onglet Projet**
- **Région Maritime (SRM)** : Méditerranée, Golfe de Gascogne, Mers Celtiques, ou Manche Mer du Nord. Détermine le filtre géographique appliqué à l'analyse (`project.srm`).

**Sous-onglet Base de données**
- **Source des données** : `Base de données PostgreSQL` ou `Fichiers locaux (dossier inputs)` - détermine le mode d'import (voir section *Prérequis > Formatage des données et sources possibles*). <br>
- Si **PostgreSQL** est sélectionné : <br>
  - Activer PostGIS (case à cocher) <br>
  - Configuration DB : `local` ou `dist1` <br>
  - Hôte, port, nom de la base, utilisateur, mot de passe <br>
  - ⚠️ Le mot de passe saisi est **écrit en clair dans le fichier YAML de configuration** généré <br>
- Si **Fichiers locaux** est sélectionné : aucun champ supplémentaire - un message rappelle de déposer les fichiers `.gpkg` dans `study/inputs/`, nommés d'après le `Table_code` déclaré dans les datasets (ex. `grid.gpkg`, `habbenth.gpkg`, `chalutage.gpkg`).<br>

### Onglet "Calculs"

**Fichiers d'entrée** <br>
- Matrice liens activités-pressions et matrice sensibilité habitats-pressions : sélection parmi les fichiers `.ods` présents dans `study/config/`.

**Paramètres généraux** <br>
- **Agrégation sensibilité** : `Précaution` (par défaut) ou `Médiane` - scénario utilisé pour l'agrégation de la sensibilité habitat (`Calc.agreg_sensib`).<br>
- **Seuil % données manquantes acceptable** : seuil (`Calc.th_sensib`, 0–100 %) au-delà duquel une maille est exclue du calcul de sensibilité cumulée faute de données suffisantes (voir section *Prérequis > Formatage des matrices*, convention `99` = nodata).<br>
- **Corriger les surfaces d'habitat** : active ou non la correction des surfaces d'habitat (`Calc.l_corr_surfhab`). <br>

**Unités de surface des données sources** <br>
- **Unité cible** : unité utilisée pour les calculs internes du moteur (`m2`, `km2` ou `ha`). <br>
- Pour chaque champ de surface (habitat benthique : `E_surfhab`, `E_surfmer` ; grille : `G_surfmesh`, `G_surfmer`), préciser l'unité dans laquelle la donnée est stockée en base. Le moteur effectue la conversion vers l'unité cible. <br>

### Onglet "Datasets"

Deux tableaux éditables (double-clic sur une cellule pour modifier) :

- **Datasets Grille & Habitats (G / E)** : la ligne de grille principale a son champ `enabled` forcé à `TRUE`.
- **Datasets Activités (A)** : une ligne par colonne d'intensité - les champs `Table_code` / `Source` / `DB_*` sont répétés pour chaque dataset.

Boutons **Ajouter** / **Supprimer la sélection** pour gérer les lignes de chaque tableau. Voir la section *Prérequis > Formatage des données et sources possibles* pour le détail des champs attendus (`Table_code`, `Data_type`, `Source`, `DB_schema`, `DB_table`, `id_col`, `geom_col`, `intensity_cols`, etc.).

### Onglet "Monte-Carlo"

**Activation des modules d'analyse** <br>
- **Monte Carlo** : active le module de robustesse stochastique (`step2_monte_carlo.R`) <br>
- **Robustesse déterministe** : teste l'effet marginal de chaque facteur méthodologique via des scénarios fixes (scénarios non paramétrables depuis l'interface - voir `config.yml` directement, section *Lancement via la console R*) <br>
- **Robustesse stochastique** : caractérise la dispersion des résultats Monte Carlo - nécessite que le module Monte Carlo soit activé <br>
- **Sensibilité formelle** : décomposition de variance par méthode de Shapley (`step2d_sensibilite.R`). Ce module est présent dans le fichier de configuration (`analyses.sensibilite_formelle`) mais n'est **pas encore paramétrable depuis l'interface Shiny** - les paramètres de simulation (nombre de permutations `m`, nombre de répétitions `nrep`, etc.) y seront intégrés dans une prochaine version. En l'état, son activation et son paramétrage passent par une édition manuelle du fichier de config (voir section *Lancement via la console R*).<br>

**Simulation Monte Carlo** <br>
- **Nombre de simulations** (`analyses.monte_carlo.nbsimul`) <br>
- **Seuils statistiques** (`thres_stat`) : seuils en % (séparés par virgule) pour identifier les mailles les plus/moins exposées <br>

**Facteurs d'incertitude** - pour chaque facteur, choisir une modalité fixe ou l'option "Aléatoire" (tirée à chaque simulation) :

| Facteur | Rôle | Modalité par défaut |
|---|---|---|
| X1 - Matrice activité-pression | médiane / précaution / binaire | Médiane |
| X2 - Relation activité-pression | linéaire / optimiste / pessimiste / logistique | Linéaire |
| X3 - Variabilité sensibilité | perturbation par IC | Sans IC |
| X4 - Variabilité CCR activité-pression | perturbation par IC | Sans IC |
| X5 - Variabilité activités | perturbation par IQ | Sans IQ |
| X6 - Fonction de distance | application d'une fonction de distance | Sans |
| X7 - Matrice sensibilité | précaution / médiane | Précaution |

**Export vers DB** *(affiché uniquement en mode PostgreSQL)* <br>
- Exporter chaque simulation / les statistiques finales <br>
- Schéma d'export PostgreSQL et configuration DB cible <br>

### Onglet "Outputs"

**Paramètres généraux** <br>
- Écrire les cartes, exporter les détails de l'analyse <br>
- *En mode PostgreSQL* : exporter vers DB, exporter les détails vers DB

**Export cartes détaillées** : cartes des activités (Ai), des pressions (Pj), et des pressions ré-exprimées (REX Pj)

**Step 3 - Graphiques** <br>
- Activer/désactiver <br>
- Mode : `Simplifié` ou `Complet` (ce dernier nécessite de préciser le **champ zone**, ex. `zone`) <br>

**Rapport HTML** : active la génération du rapport de synthèse global consulté à l'issue de l'analyse.

### Onglet "Logs"

Suivi en temps réel du journal d'exécution, avec boutons pour rafraîchir l'affichage ou télécharger le log complet.

### Charger une configuration existante

Le bouton **"Charger config"** (dans l'onglet *Projet & DB*) accepte tout fichier `.yml` conforme au format de `study/config/config.yml`. Les champs de l'interface sont alors pré-remplis avec son contenu, permettant de repartir d'une configuration existante plutôt que de tout ressaisir.

---

## Lancement d'une analyse via la console R

### Démarrage

```r
source("execution/run_batch.R")
```

Ce mode lit par défaut le fichier `study/config/config.yml`. Pour utiliser une autre configuration, **éditer manuellement** `study/config/config.yml` avant de lancer le script (le remplacer par le contenu voulu, ou changer les valeurs directement) <br>

Le mode batch est destiné aux **utilisateurs avertis** : contrairement à l'interface Shiny, il n'y a ni validation interactive, ni pré-remplissage guidé - toute erreur de configuration se manifeste au moment de l'exécution. <br>

### Déroulé du script

Le script exécute successivement, dans cet ordre, les étapes suivantes :

| Étape | Rôle | Condition d'exécution |
|---|---|---|
| **Step 0** | Vérification de l'environnement et import des tables (`step0_checkup`) | Toujours exécutée |
| **Step 1** | Analyse simple avec le modèle de référence (`step1_analyse_simple`) | Toujours exécutée |
| **Step 2** | Monte Carlo stochastique | `analyses.monte_carlo.enabled: true` |
| **Step 2b** | Robustesse déterministe | `analyses.robustesse_deterministe.enabled: true` - indépendante de l'étape 2 |
| **Step 2c** | Robustesse stochastique | `analyses.robustesse_stoch.enabled: true` **ET** résultat de l'étape 2 disponible (sinon avertissement et étape ignorée) |
| **Step 2d** | Sensibilité formelle (méthode Shapley) | `analyses.sensibilite_formelle.enabled: true` |
| **Step 3** | Graphiques | `Step3.enabled: true` |
| **Rapport** | Carte interactive Leaflet + rapport HTML global | Générés si `report.enabled: true` (rapport global) - la carte Leaflet est générée indépendamment dès que le contexte d'analyse (`res1$ctx`) est disponible |

Chaque étape désactivée ou non exécutable affiche un message explicite dans la console (`[RUN]` / `[SKIP]`), ce qui permet de suivre précisément ce qui a été calculé.
<br>

### Remplissage du fichier de config

Le fichier `config.yml` couvre l'ensemble des paramètres exposés dans l'interface Shiny (voir section précédente), **plus deux blocs qui ne sont pour l'instant configurables qu'en édition manuelle du fichier** :
<br>

#### `analyses.robustesse_deterministe.scenarios`

Définit, pour chaque facteur testé, la liste des modalités à évaluer (effet marginal - un facteur varie à la fois, les autres restant à leur valeur de référence). **Le premier élément de chaque liste est le scénario de référence**, et au moins 2 modalités sont nécessaires par facteur :

```yaml
robustesse_deterministe:
  enabled: yes
  scenarios:
    X1_mat_AP: [2, 3, 4]           # médiane (réf), précaution, binaire
    X2_relation_AP: [2, 3, 4, 5]   # linéaire (réf), optimiste, pessimiste, logistique
    X7_mat_sensi: [2, 3]           # précaution (réf), médiane
    X6_distance: [0, 1]            # sans (réf), avec fonction distance
```
<br>

#### `analyses.sensibilite_formelle`

Décompose la variance totale des résultats entre les facteurs d'incertitude, par méthode de Shapley. Non exposé dans l'interface Shiny à ce stade (voir section précédente) :

```yaml
sensibilite_formelle:
  enabled: false
  methode: "shapley"
  m: 10        # nombre de permutations
  nrep: 5      # nombre de répétitions
  facteurs:
    X1_mat_AP: [2, 3, 4]
    X2_relation_AP: [2, 3, 4, 5]
    X3_var_sensi: [0, 1]
    X4_var_ccr_AP: [0, 1]
    X5_var_act: [0, 1]
    X6_distance: [0, 1]
    X7_mat_sensi: [2, 3]
```

⚠️ Ce module peut être coûteux en temps de calcul selon les valeurs de `m` et `nrep` (nombre de simulations engendrées = fonction du nombre de facteurs × `m` × `nrep`) - commencer par de petites valeurs pour estimer le temps d'exécution avant un run complet.

### Différences selon `db.data_source`

Le fichier de config doit être cohérent avec le mode de source de données choisi :

- **`data_source: postgis`** : chaque dataset doit renseigner `DB_schema` et `DB_table`, et les identifiants de connexion doivent être présents dans le bloc `DBConnectlocal` (ou équivalent selon `db.choice`).
- **`data_source: flat_files`** : les champs `DB_schema` / `DB_table` sont omis ; les fichiers `.gpkg` correspondants doivent être présents dans `study/inputs/` (voir section *Prérequis > Formatage des données et sources possibles*).

### Notes sur le format YAML

- Les valeurs booléennes peuvent s'écrire `yes`/`no` ou `true`/`false` indifféremment.
- Une colonne `intensity_cols` à valeur unique peut être écrite comme un scalaire (`intensity_cols: i_rmc`) ou comme une liste à un élément (`intensity_cols: [i_rmc]`) - les deux formats sont acceptés.
