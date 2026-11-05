# Carpediem on EDITO: handbook

Day-to-day operations of the Carpediem process and web application on EDITO.
EDITO Datalab: https://datalab.dive.edito.eu/. EDITO support: chat widget, bottom right of
the Datalab.

| Item | Where |
|---|---|
| Process (official catalogue) | Process catalog > Metrics and indicators > `carpediem` |
| Process image | `docker.io/easyglobalmarket/carpediem:<version>` |
| Process chart | [`helm/metrics-and-indicators/carpediem/`](helm/metrics-and-indicators/carpediem/), published from GitLab `pub/edito-infra/process-helm-charts` |
| Default study | `https://minio.dive.edito.eu/oidc-antoningimard/carpediem/mer_du_nord/study.tar.gz` (public) |
| Runs | `s3://oidc-<user>/carpediem/runs/<run id>/` (your bucket) |
| Web application | Service Playground (add-your-service), image `docker.io/easyglobalmarket/carpediem-web-app` |

## 1. Prerequisites

* EDITO account. Antonin Gimard's account also has access to add-your-process,
  add-your-service and GitLab (merge requests to the official catalogues).
* For any image update: a public Docker Hub repository you can push to. The images are
  currently pushed to EGM's account (`easyglobalmarket`): move them to a Cerema or OFB
  repository and use its name everywhere below.
* For new studies or configurations: R with the project set up, see
  [`../execution/README.md`](../execution/README.md) (Prérequis).

## 2. Launch a run

1. Process catalog > `carpediem` > Launch.
2. Set the parameters (table in the [chart README](helm/metrics-and-indicators/carpediem/README.md#running-the-process)):
   * `CARPEDIEM_STUDY`: study to analyse (default: Mer du Nord).
   * `CARPEDIEM_CONFIG`: `config/config_analyse_simple.yml` (about 1 min) or
     `config/config.yml` (Monte Carlo and robustness, about 20 min).
   * `CARPEDIEM_RUN_DESCRIPTION`: always fill it in; it names the run and is shown in the web
     application.
3. Resources: 2 Gi for a simple analysis; 6 CPU and 8 Gi for `config/config.yml`.
4. Follow the run: My services / process monitoring, log lines starting with `[carpediem]`.
5. Errors: [`DEPLOIEMENT_EDITO.md`, « En cas d'erreur »](DEPLOIEMENT_EDITO.md#en-cas-derreur).

## 3. Add a study or a configuration

Study layout: [chart README, « Running your own study »](helm/metrics-and-indicators/carpediem/README.md#running-your-own-study).

New configuration for an existing study:

1. Copy an existing YAML file of the study's `config/` (options:
   [`../execution/README.md`](../execution/README.md), « Remplissage du fichier de config »).
2. Test it locally (`source("execution/run_batch.R")`) or launch it directly on EDITO.
3. Upload it to the study's `config/` folder in your bucket (Datalab > My files).
4. Launch with `CARPEDIEM_CONFIG=config/<file>.yml`.

New study:

1. V1 study: convert it with [`../migration_v1_v2/`](../migration_v1_v2/README.md).
   New study: start from the Mer du Nord archive (default `CARPEDIEM_STUDY` URL).
2. Upload it to your bucket (My files), as a folder (`config/` + `inputs/`) or as a
   `.tar.gz` archive of these two folders.
3. Launch with `CARPEDIEM_STUDY=s3://${AWS_BUCKET_NAME}/<path>` (folder or archive).

Public study (readable by all EDITO users, e.g. a new default study):
[`DEPLOIEMENT_EDITO.md` § 2](DEPLOIEMENT_EDITO.md#2-publier-létude-mer-du-nord-archive-publique).
Check the redistribution rights of the data first. Never publish a V1 parameter file
(it can contain database passwords).

## 4. Manage the runs

Layout, run id and `run.json`: [`RUNS_LAYOUT.md`](RUNS_LAYOUT.md).

* Browse: Datalab > My files > `carpediem/runs/`.
* Status: `status` in `run.json` (`running`, `succeeded`, `failed`). An old `running` run
  was killed: delete it.
* Diagnose a failed run: its `run.log`.
* Delete a run: delete its folder (runs are independent).
* Download: the run folder, or `outputs/` only (`report.html`, `carte_interactive.html`,
  GeoPackages in EPSG:3035).
* Access from outside EDITO (scripts, development): temporary S3 credentials,
  [`DEPLOIEMENT_EDITO.md`, « Accès aux runs depuis l'extérieur d'EDITO »](DEPLOIEMENT_EDITO.md#accès-aux-runs-depuis-lextérieur-dedito-développement).
  Never store them in a code repository.
* Results published or shared: credit « Generated using EDITO of the European Union »
  ([root README](../README.md#utilisation-sur-edito)).

## 5. Use the runs in the web application

1. Launch the web application from the Service Playground (My services if already running).
2. It reads the runs of the bucket of the account that launched it (`carpediem/runs/`).
3. Data no longer accessible after a while: the S3 credentials have expired; restart the
   service.
4. Do not share the service URL: anyone with it can browse your runs (no login yet).

## 6. Update the process

Any change to the code or to `edito-docker/` gives a new version (`x.y.z`, greater than the
published one). A published image tag or chart version is never reused.

1. Merge the code change on GitHub (`FredQuemmerais/carpediem`).
2. Build and push the image `carpediem:<version>`:
   [`DEPLOIEMENT_EDITO.md` § 1](DEPLOIEMENT_EDITO.md#1-construire-et-pousser-limage).
3. Test it in the Process Playground (add-your-process, same version):
   [§ 3 and § 4](DEPLOIEMENT_EDITO.md#3-déclarer-le-processus-add-your-process).
4. Update the chart and publish it in the official catalogue (merge request on
   `process-helm-charts`):
   [§ 5](DEPLOIEMENT_EDITO.md#5-publication-dans-le-catalogue-officiel).

Change of default study or configuration only: step 4 (`values.yaml`), no new image.

## 7. Update the web application

1. Build and push a new image with a fixed version tag (not `latest`).
2. add-your-service (Datalab > Contribution): same service, new image tag, new version.
3. Check it in the Service Playground, then delete the old service instance.

Before publication in the official service catalogue (merge request on GitLab
`pub/edito-infra/service-helm-charts`, category « Data visualization », same steps as § 6.4):
access protection, handling of expired credentials, configurable runs location.
