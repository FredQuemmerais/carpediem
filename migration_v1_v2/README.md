# Migration des études Carpediem V1 vers V2

Ce document décrit la procédure générale de passage d'une étude au format Carpediem+
(V1 : fichier de paramétrage ODS, GeoPackages multi-couches, codes d'activités et de
pressions de la V1) vers le format Carpediem V2 (configuration YAML, un GeoPackage par jeu
de données, matrices V2).

Les scripts de ce dossier sont un exemple d'application de cette procédure à l'étude
Mer du Nord (voir la section [Exemple](#exemple--étude-mer-du-nord)). Ils peuvent servir de
point de départ pour une autre étude, après adaptation.

Les scripts ne font partie ni de l'image Docker ni du processus EDITO : ils préparent les
données en amont. Le résultat est un dossier d'étude V2 (`config/` et `inputs/`)
utilisable par `execution/run_batch.R`, par l'interface Shiny ou par le conteneur.

## Vue d'ensemble de la procédure

1. Rassembler les entrées V1 de l'étude.
2. Établir et faire valider la correspondance entre les codes V1 et les référentiels V2
   (classeur de correspondance).
3. Convertir la matrice activités-pressions.
4. Convertir la matrice de sensibilité des habitats.
5. Convertir les données géographiques et produire les fichiers de configuration.
6. Contrôler le résultat et lancer une première analyse.

Les règles de conversion appliquées par défaut sont résumées plus bas. Ce sont des choix
méthodologiques : ils sont à faire valider, pour chaque étude, par les responsables
scientifiques, et à consigner avec l'étude.

## 1. Entrées V1

| Entrée | Contenu attendu |
|---|---|
| Fichier de paramétrage V1 (ODS) | Onglet `matrice_activites_pressions` : lignes `Final` (amplitudes) et `IC` (indices de confiance) par activité, une colonne par pression |
| Matrice de sensibilité V1 (ODS) | Onglets `median` et `precaution` ; colonne `cd_eunis`, sensibilités `Pr_*` de 0 à 5 (99 = sans donnée), confiances `Pr_*_icas` (1, 3 ou 5) |
| Grille (Shapefile) | Une maille par ligne, identifiant unique, colonnes de façade (`srm`), surface de maille et surface en mer |
| Habitats (GeoPackage) | Une ligne par maille et habitat, code EUNIS, surfaces |
| Activités (GeoPackages multi-couches) | Une couche par activité ou groupe d'activités, avec l'identifiant de maille et une colonne d'intensité |

Le fichier de paramétrage V1 peut contenir des identifiants de connexion à une base de
données (onglet `connexion_bd`) : il ne doit pas être diffusé tel quel.

## 2. Classeur de correspondance

Le classeur (xlsx) est l'entrée centrale de la conversion. Il contient :

* un onglet `Pressions` (une ligne par pression V1, à partir de la ligne 3) : code V1 (A),
  libellé (B), code V2 proposé (D), pression à évaluer « Oui »/« Non » (H), décision (I),
  code V2 retenu si la proposition est modifiée (J) ;
* un onglet `Activités` (une ligne par couche de données et activité V1, à partir de la
  ligne 3) : couche sous la forme `fichier :: couche` (A), colonne d'intensité (B), code
  V1 (C), code V2 proposé (F), décision (J), code V2 retenu si modifié (K), traitement si
  la couche porte plusieurs activités (M) ;
* des onglets de référence listant les codes V2 de pressions et d'activités, pour faciliter
  la saisie.

Un modèle vierge est fourni : `ref/modele_correspondances_v1_v2.xlsx`. L'onglet `Pressions`
y est prérempli avec la typologie V1 et une proposition de code V2 pour chaque pression ;
l'onglet `Activités` est à compléter avec les couches de l'étude. Ne pas insérer ni déplacer
de colonnes : les scripts les lisent par position.

Les décisions reconnues sont « Validé » et « Modifié » ; toute autre valeur exclut la
ligne. Les propositions de correspondance sont à préparer à partir des libellés V1 et des
typologies V2, puis à soumettre aux responsables scientifiques de l'étude.

## 3 à 5. Conversion

Avec les scripts de l'exemple, la conversion s'enchaîne ainsi (chemins à adapter) :

```bash
ETUDE=chemin/vers/etude_v1          # entrées V1
V2=chemin/vers/etude_v2             # étude V2 produite
CLASSEUR=chemin/vers/correspondances.xlsx   # rempli à partir du modèle
mkdir -p "$V2/study/config" "$V2/conversion"

python3 migration_v1_v2/build_matrice_act_press.py \
  --param "$ETUDE/PARAM/fichier_parametrage.ods" \
  --workbook "$CLASSEUR" \
  --template migration_v1_v2/ref/matrice_act_press_baie_de_seine.ods \
  --out-dir "$V2/study/config" --report-dir "$V2/conversion"

python3 migration_v1_v2/build_matrice_sensi.py \
  --sensi "$ETUDE/PARAM/matrice_sensibilite.ods" \
  --workbook "$CLASSEUR" \
  --habitats "$ETUDE/INPUTS/habitats.gpkg::couche" \
  --out-dir "$V2/study/config" --report-dir "$V2/conversion"

python3 migration_v1_v2/build_study.py \
  --inputs "$ETUDE/INPUTS" \
  --workbook "$CLASSEUR" \
  --matrices "$V2/study/config" \
  --out-dir "$V2/study" --report-dir "$V2/conversion"
```

Dans l'exemple, `build_study.py` contient en tête quelques constantes propres à l'étude, à
adapter pour une autre étude : système de coordonnées, nom de la colonne d'identifiant de maille, liste
des façades, nom du fichier de grille, fichier et couche d'habitats. Les paramètres
d'analyse écrits dans les fichiers de configuration (seuils, Monte Carlo, robustesse,
zones de l'étape 3) sont également à relire.

Pour publier l'étude (sur EDITO par exemple, où `CARPEDIEM_STUDY` accepte une URL https,
une clé s3 ou un chemin local), en faire une archive reproductible :

```bash
tar --sort=name --mtime="2026-01-01 00:00:00Z" --owner=0 --group=0 --numeric-owner \
  -C "$V2/study" -cf - config inputs | gzip -n > "$V2/study.tar.gz"
```

Sorties :

| | Contenu |
|---|---|
| `study/config/` | `matrice_act_press.ods`, `matrice_sensi_hab_press.ods`, `config.yml` (analyse complète), `config_analyse_simple.yml` (analyse simple) |
| `study/inputs/` | `grid.gpkg`, `habbenth.gpkg`, un GeoPackage par couche d'activité retenue |
| `conversion/` | `conversion_activites.csv`, `conversion_pressions.csv`, journaux de conversion |

La conversion est reproductible : relancer les scripts redonne les mêmes fichiers.

## Règles de conversion par défaut

Matrice activités-pressions :

* CCR = amplitude V1 / 1000 ; amplitude vide = 0.
* IC = IC V1 / 5 ; IC vide = 0.
* Lien binaire = 1 si l'amplitude est non nulle et l'IC V1 vaut 3 ou plus.
* Un seul code V2 par couche de données (celui de la première ligne de la couche dans le
  classeur) ; plusieurs activités V1 rattachées au même code V2 sont fusionnées avec
  l'amplitude maximale (et l'IC correspondant).
* Activités parentes : maximum des enfants, IC de l'enfant portant ce maximum.
* Activité V2 sans correspondance : 0.
* Scénario précaution = copie du médian (la matrice V1 n'a qu'un scénario).
* Typologie des pressions : `demonstrateur` = 1 pour les pressions évaluées et leurs
  ancêtres.

Matrice de sensibilité :

* Colonnes des pressions évaluées uniquement, sous leur code V2 ; 99 conservé
  (« sans donnée »).
* IC 1, 3 ou 5 repris ; IC à 99 ou vides laissés vides.
* Onglet binaire : 5 si la sensibilité médiane est non nulle, 0 si nulle, 99 conservé ;
  IC du médian.

Données et configuration :

* Grille convertie en GeoPackage ; couche d'habitats avec une colonne `hab_iq` vide
  (pas d'indice de qualité des habitats en V1).
* Un GeoPackage par couche d'activité retenue, reprojeté si besoin, colonne d'intensité
  renommée de façon unique (`i_<couche>`), ce qu'exige `act_col_mapping`.
* Couches sans activité V1 correspondante ou exclues dans le classeur : ignorées (listées
  dans le journal).
* `config.yml` : Monte Carlo et robustesse déterministe activés ;
  `config_analyse_simple.yml` : désactivés ; analyse de sensibilité formelle (Shapley)
  désactivée dans les deux ; graine aléatoire fixée.

## Contrôles

* Relire les journaux et rapports du dossier `conversion/` (couches ignorées, codes
  d'habitats absents de la matrice de sensibilité, valeurs vides).
* `matrice_act_press.ods` doit être lue sans erreur par `step1_init()` du moteur V2.
* Lancer d'abord `config_analyse_simple.yml`, puis l'analyse complète.

## Exemple : étude Mer du Nord

Les fichiers de ce dossier appliquent la procédure à l'étude Mer du Nord. Les valeurs propres
à cette étude (noms de fichiers et de couches, colonne d'identifiant, façades, paramètres
d'analyse) y sont écrites en dur, et les numéros de décision cités dans les commentaires
(D1, D17...) renvoient au registre des choix de cette étude.

| Fichier | Rôle |
|---|---|
| `convertir_mer_du_nord.sh` | Enchaîne les trois scripts ci-dessous avec les chemins de l'étude Mer du Nord, puis produit l'archive de l'étude ; le classeur de correspondance rempli (non versionné) est indiqué par `CARPEDIEM_WORKBOOK` |
| `build_matrice_act_press.py` | Matrice activités-pressions V2 (`matrice_act_press.ods`) à partir de la matrice V1 et du classeur de correspondance |
| `build_matrice_sensi.py` | Matrice de sensibilité V2 (`matrice_sensi_hab_press.ods`) à partir de la matrice de sensibilité V1 |
| `build_study.py` | Données d'entrée (grille, habitats, activités) et fichiers de configuration (`config.yml`, `config_analyse_simple.yml`) |
| `ref/modele_correspondances_v1_v2.xlsx` | Modèle vierge du classeur de correspondance (procédure générale) |
| `ref/matrice_act_press_baie_de_seine.ods` | Matrice V2 de référence (exemple Baie de Seine) : source des typologies V2 d'activités et de pressions |

Prérequis : Python 3 avec pandas, numpy, odfpy, openpyxl, geopandas, pyogrio et PyYAML.
