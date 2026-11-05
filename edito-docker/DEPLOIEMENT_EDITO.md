# Déploiement de Carpediem sur EDITO

Mode opératoire pour publier une version du processus Carpediem sur EDITO : image Docker,
étude par défaut, test dans le Process Playground, publication dans le catalogue officiel
« Metrics and indicators ». Vue d'ensemble de l'exploitation : [`HANDBOOK.md`](HANDBOOK.md).

Registre actuel des images : `docker.io/easyglobalmarket/carpediem` (compte EGM). A remplacer
par un dépôt Docker Hub public de Cerema ou de l'OFB : utiliser alors son nom partout
ci-dessous.

## 1. Construire et pousser l'image

Depuis la racine du dépôt, sur la branche à publier :

```bash
docker login                       # compte du dépôt Docker Hub
docker buildx build --platform linux/amd64 \
  -f edito-docker/Dockerfile \
  --build-arg CARPEDIEM_VERSION=<version> \
  --build-arg CARPEDIEM_COMMIT=$(git rev-parse --short HEAD) \
  -t easyglobalmarket/carpediem:<version> \
  --push .
```

* L'étiquette est la version du processus (`x.y.z`). Le commit du code est inscrit dans le
  `run.json` de chaque run. Une étiquette publiée n'est jamais réécrite : toute modification
  de l'image (code ou `edito-docker/`) donne une nouvelle version.
* `linux/amd64` est obligatoire (nœuds EDITO). Sur un Mac Apple Silicon, la construction
  passe par l'émulation et prend plus de temps.
* Le dépôt Docker Hub doit être **public** (exigence EDITO).
* Taille attendue : environ 0,75 Go compressé.

## 2. Publier l'étude Mer du Nord (archive publique)

L'image ne contient pas d'étude. L'étude par défaut du processus est l'archive publique de
l'étude Mer du Nord, lisible par tous les utilisateurs d'EDITO. Même procédure pour toute
autre étude à rendre publique.

1. Produire l'archive (`config/` et `inputs/`). Pour Mer du Nord :
   `migration_v1_v2/convertir_mer_du_nord.sh` écrit `study.tar.gz` (environ 7,4 Mo).
2. La déposer dans le bucket du compte EDITO, par exemple sous
   `carpediem/mer_du_nord/study.tar.gz`, avec l'explorateur de fichiers du Datalab
   (« Mes fichiers »).
3. La rendre publique dans l'explorateur de fichiers (icône en forme d'œil, visibilité
   publique).
4. Vérifier, sans identifiants, depuis n'importe quel poste :
   ```bash
   curl -sI https://minio.dive.edito.eu/oidc-<compte>/carpediem/mer_du_nord/study.tar.gz | head -1
   ```
   Réponse attendue : `200`.

Rendre une étude publique revient à rediffuser ses données : vérifier les droits de
rediffusion avant. Ne jamais publier un fichier de paramétrage V1 (il peut contenir des
identifiants de base de données).

## 3. Déclarer le processus (add-your-process)

Process Catalog > Contribution > add-your-process, puis remplir :

| Section du formulaire | Champ | Valeur |
|---|---|---|
| 1 | Friendly name | `CumulativeImpactTool-CarpeDiem` |
| 2. Configuration | Container image is already built | `true` |
| | Image tag | `docker.io/easyglobalmarket/carpediem:<version>` |
| 3. Metadata | Process name | `carpediem` |
| | Process version | `<version>` (toujours supérieure à la version déjà déclarée) |
| | Process description | `description` de `helm/metrics-and-indicators/carpediem/Chart.yaml` |
| | Icon URL | `icon` du même `Chart.yaml` |
| | Homepage URL | `https://github.com/FredQuemmerais/carpediem/tree/carpediem_v2` |
| | Keywords | un mot-clé par champ : cumulative, impacts, marine, spatial, planning, biodiversity, activities, habitats, pressures, uncertainty |
| 4. Git | | rien (l'image est déjà construite) |
| 5. Copernicus Marine | | non coché |
| 6. Other environment variables | | voir ci-dessous |
| 7. Resources for add-your-process | | valeurs par défaut |
| 8. Resources for your process | CPU | minimum 1000m ; maximum 8000m si le champ existe |
| | Mémoire (si le champ existe) | minimum 2 Gi ; maximum 8 Gi |

Variables d'environnement (section 6), à saisir en anglais, descriptions comprises :

| Name | Description | DefaultValue |
|---|---|---|
| `CARPEDIEM_STUDY` | Study to run: a .tar.gz archive (https:// URL or s3://bucket/key) or a folder (s3://bucket/prefix) containing config/ and inputs/. ${AWS_BUCKET_NAME} is replaced by your bucket. Default: North Sea example | `https://minio.dive.edito.eu/oidc-antoningimard/carpediem/mer_du_nord/study.tar.gz` |
| `CARPEDIEM_CONFIG` | YAML configuration, relative to the study. config/config_analyse_simple.yml: simple analysis (about 1 min); config/config.yml: with Monte Carlo and robustness analyses (about 20 min with 6 CPUs) | `config/config_analyse_simple.yml` |
| `CARPEDIEM_OUTPUT` | Where the runs are stored: each run writes its own folder <run id>/ there (run.json, run.log, study/, outputs/). ${AWS_BUCKET_NAME} is replaced by your bucket | `s3://${AWS_BUCKET_NAME}/carpediem/runs` |
| `CARPEDIEM_RUN_DESCRIPTION` | Short description of this run, shown in the list of runs (e.g. "Mer du Nord, without pelagic fishing") | (vide) |
| `CARPEDIEM_WORKERS` | Number of parallel workers for the Monte Carlo analysis; empty: number of CPUs minus one | (vide) |

Ressources : environ 0,8 Go de mémoire, plus environ 0,7 Go par worker supplémentaire. Avec
8 vCPU (7 workers), prévoir environ 6 Go, d'où le maximum à 8 Gi.

Enregistrer (Save), puis lancer (Launch). Enregistrer de nouveau juste avant le lancement
pour que toute la configuration soit conservée (les processus du Playground peuvent être
supprimés lors des maintenances).

## 4. Tester dans le Process Playground

Le processus apparaît sous 5 minutes dans Process catalog, catalogue « Playground »
(identifiant `process-playground`), en cherchant « carpediem ». Les événements du job
add-your-process qui suivent `Job completed` (`MountVolume.SetUp failed`…) sont des messages
de nettoyage de Kubernetes, sans effet.

1. **Analyse simple** : lancer avec les valeurs par défaut, en choisissant le bucket de
   sortie dans la configuration S3.
   * Durée attendue : 1 à 2 minutes.
   * Journal : lignes `[carpediem] … Run <run id> started`, `Downloading study…`,
     `Running pipeline…`, `End: exit code 0, duration …`, `Writing run folder to …`.
   * Dans le bucket : `oidc-<compte>/carpediem/runs/<run id>/` avec `run.json`, `run.log`,
     `study/` et `outputs/` (`report.html`, `carte_interactive.html`,
     `resultats_analyse_*.gpkg`, `maps/`, `graphs/`). `run.json` apparaît dès le début du run
     (statut `running`).
2. **Run complet** : relancer avec `CARPEDIEM_CONFIG=config/config.yml`, 6 CPU et 8 Gi.
   Durée attendue : environ 20 minutes.

Tester aussi depuis un autre compte EDITO si possible (accès public à l'étude par défaut).

## 5. Publication dans le catalogue officiel

Chart à publier : `edito-docker/helm/metrics-and-indicators/carpediem/`. Par rapport au chart
généré par add-your-process, il ajoute les descriptions des paramètres, les responsables, un
`README.md`, aucune relance automatique d'un run en échec (`backoffLimit: 0`) et des
ressources réduites pour le conteneur `copy-output`.

Etapes (compte GitLab autorisé par le support EDITO, actuellement celui d'Antonin Gimard) :

1. Tester la version dans le Playground (étapes 1 à 4).
2. Mettre à jour le chart : `version` et `appVersion` dans `Chart.yaml`, étiquette de l'image
   dans `templates/job.yaml` ; `values.yaml`, `values.schema.json` et `README.md` si les
   paramètres ou les valeurs par défaut changent. Vérifier avec `helm lint` et
   `helm template`.
3. Cloner `https://gitlab.mercator-ocean.fr/pub/edito-infra/process-helm-charts`, créer une
   branche.
4. Copier `edito-docker/helm/metrics-and-indicators/carpediem/` dans le dossier
   `metrics-and-indicators/` du dépôt.
5. Optionnel, avant de pousser : `pre-commit run --all-files` (même contrôle que la CI :
   espaces en fin de ligne, fin de fichier, format JSON).
6. Commit, push, merge request vers `main`, en mentionnant `@pub/edito-infra/codeowners`.
   La CI vérifie le format et que la version n'existe pas encore dans le catalogue ; à la
   fusion, le chart est publié dans « Metrics and indicators ».
7. Reporter la modification du chart dans le dépôt Carpediem.

Autre voie, plus rapide mais sans les apports du chart ci-dessus : demander au support
EDITO de copier la version du Playground dans le catalogue officiel
(`make move-chart-from-playground-to-official-catalog TARGET_CATALOG=metrics-and-indicators
CHART_NAME=carpediem CHART_VERSION=<version>`).

## En cas d'erreur

* `Onyxia API Error: Put /api/my-lab/app Response:500` au lancement : erreur dans le
  formulaire (souvent un chemin). Ouvrir un notebook avec le rôle « edit » pour voir
  l'erreur exacte.
* Le run s'arrête au téléchargement : vérifier `CARPEDIEM_STUDY` (nom du bucket, préfixe).
* `Pipeline exit code` différent de 0 : le `run.log` est tout de même copié dans le bucket.
* Les sorties ne doivent pas être écrites dans `/output` : le chart copie seulement
  `/output-data` vers `s3://oidc-<compte>/<nom du run>/`. Laisser `CARPEDIEM_OUTPUT` à sa
  valeur par défaut.

## Accès aux runs depuis l'extérieur d'EDITO (développement)

Pour lire `carpediem/runs/` depuis un poste de développement, utiliser les identifiants S3
temporaires du compte EDITO : dans le Datalab, menu du compte (« My account » / « Mon
compte »), onglet de connexion au stockage (« Connect to storage »). La page donne
`AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN`, `AWS_S3_ENDPOINT`
(`minio.dive.edito.eu`), `AWS_DEFAULT_REGION`, la date d'expiration, et des exemples de
configuration (variables d'environnement, rclone, MinIO client, Python). Ces identifiants
expirent : les régénérer sur la même page. Ne pas les enregistrer dans un dépôt de code.

## Historique des versions

| Version | Date | Contenu |
|---|---|---|
| 2.0.0 à 2.0.3 | octobre 2026 | Versions de mise au point dans le Process Playground (droits du script d'entrée, dossier de sortie, journal horodaté avec CPU et mémoire) |
| 2.0.4 | 22 octobre 2026 | Un dossier par run (`run.json`, `run.log`, `study/`, `outputs/`), variable `CARPEDIEM_RUN_DESCRIPTION`, licence dans l'image. Run complet Mer du Nord sur EDITO : 17 min 55 s (6 CPU, 5 workers, 8 Gi) |
| 2.1.0 | 22 octobre 2026 | Plus d'étude dans l'image ; `CARPEDIEM_STUDY` obligatoire, archive `.tar.gz` acceptée ; étude par défaut : archive publique Mer du Nord ; configuration par défaut : analyse simple. Publiée dans le catalogue officiel (merge request !50 de `process-helm-charts`) |
