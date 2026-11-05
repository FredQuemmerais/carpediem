# Carpediem runs: storage layout and run.json

Contract between the Carpediem container (EDITO process) and the applications that read its
results (the web application in particular). Applies from image version 2.0.4.
Last update: 2026-10-22.

## Principle

Each run of the container writes **one folder**, named after the run id, under a common
root. The folder holds a description of the run (`run.json`), its log, a copy of the study
it used, and its results. There is no database: listing the runs is listing the folders.

Root on EDITO (form default of `CARPEDIEM_OUTPUT`):

```
s3://<user bucket>/carpediem/runs/            e.g. s3://oidc-antoningimard/carpediem/runs/
```

## Layout of a run folder

```
<root>/<run id>/
├── run.json      description of the run (see below)
├── run.log       full log (container messages and pipeline output)
├── study/        copy of the study folder used by the run, as it was at launch
│   ├── config/   YAML configurations and the two ODS matrices
│   └── inputs/   one GeoPackage per input dataset (grid, habitats, activities)
└── outputs/      results of the pipeline
    ├── resultats_analyse_<date>.gpkg   indices per grid cell (layer grille_indices)
    ├── details_analyse_<date>.gpkg     activities, pressures, sensitivities, effects per cell
    ├── monte_carlo_<date>.gpkg         Monte Carlo statistics (if enabled)
    ├── maps/                           REFC, REXC and confidence index maps (GPKG, GeoJSON)
    ├── graphs/                         JPEG graphs and their CSV data
    ├── robustesse_det/<factor>/        deterministic robustness (if enabled)
    ├── report.html, carte_interactive.html
    └── stats_recap_<date>.json, run_info.json
```

* `study/` is a copy, not a reference: the run stays readable and reproducible even if the
  study folder is changed or deleted later.
* `outputs/` only exists if the pipeline produced results; a failed run may have only
  `run.json`, `run.log` and `study/`.
* File names in `outputs/` contain timestamps: find files through `run.json` (`files`, field
  `kind`), not by building their names.
* All GeoPackages are in **EPSG:3035** (ETRS89-LAEA), the CRS of the study grid. Grid cells
  are identified by `idmesh` (input datasets: `id_3nq_3` for Mer du Nord).

## Run id

```
<UTC start time, YYYYMMDDTHHMMSSZ>-<suffix>       e.g. 20261022T085333Z-mer-du-nord-analyse-simple-de-reference
```

* Suffix: short form of `CARPEDIEM_RUN_DESCRIPTION` (the free text the user enters at
  launch): accents removed, lowercase, any other character replaced by a dash, at most 40
  characters, cut between two words.
* Without description: short form of the configuration name (`config_analyse_simple.yml`
  gives `analyse-simple`, `config.yml` gives `config`).
* Sorting the ids alphabetically sorts the runs by start time.
* Show `description` (full text) to the users, not the id.

## Lifecycle and listing

1. Start: once the study is fetched and the configuration found, `run.json` is written with
   `status: "running"` (no file list yet).
2. End: the whole folder is written, with `run.json` completed (`status`, `ended_at`,
   `duration_seconds`, `summary`, `files`).

| `status` | Meaning |
|---|---|
| `running` | Run in progress (or interrupted without being able to write its end, e.g. pod killed) |
| `succeeded` | Pipeline finished with exit code 0 |
| `failed` | Error (`exit_code` not 0); see `run.log` |

A `running` run that is old (for example more than a few hours) has most probably been
killed. If the study or the configuration cannot be found, the run is `failed` and its
`run.json` has no parameters.

To list the runs: S3 `ListObjectsV2` on the prefix `carpediem/runs/` with delimiter `/`
(one common prefix per run), then read `<prefix>run.json` of each run.

## run.json

Example: `edito-docker/examples/run.json` (real run, simple analysis).

| Field | Type | Content |
|---|---|---|
| `schema_version` | string | `"1.0"`; changed if a field is renamed or removed |
| `id` | string | run id (folder name) |
| `description` | string | text entered by the user (`CARPEDIEM_RUN_DESCRIPTION`), may be empty |
| `status` | string | `running`, `succeeded`, `failed` |
| `exit_code` | number or null | exit code of the container (null while running) |
| `started_at`, `ended_at` | string or null | UTC, ISO 8601 (`2026-10-22T08:53:33Z`) |
| `duration_seconds` | number or null | |
| `software.carpediem_version` | string | version of the EDITO process / image (e.g. `2.0.4`) |
| `software.code_commit` | string | commit of the Carpediem code (branch `v2-engineering`) |
| `study.source` | string | where the study was read (`s3://…` or local path) |
| `study.config` | string | configuration used, relative to the study folder |
| `parameters` | object or null | from the configuration: `srm` (sub-regions), `agreg_sensib`, `th_sensib`, `seed`, `analyses` (booleans `simple_analysis`, `monte_carlo`, `deterministic_robustness`, `stochastic_robustness`, `shapley_sensitivity`, and `monte_carlo_simulations`) |
| `resources` | object | `cpu_limit`, `memory_limit_gib`, `memory_peak_gib`, `workers` (Monte Carlo workers; null if not used); null when unknown |
| `summary` | object or null | indicators of the run (content of `stats_recap_<date>.json`): `n_mailles`, `n_activites`, `n_pressions`, `srm`, averages and maxima of IMA, IPC, REXC, REFC, IC |
| `files` | array | every file of the folder except `run.json` (empty while running) |

Each element of `files`:

| Field | Content |
|---|---|
| `path` | path relative to the run folder |
| `kind` | role of the file (table below) |
| `format` | file extension (`gpkg`, `html`, `jpeg`, `csv`, `json`, `yml`, `ods`, `log`…) |
| `size` | size in bytes |
| `layers` | GeoPackages only: layer names |
| `active_config` | `true` on the configuration file used by the run |
| `dataset`, `activity_codes` | `input_activity` only: dataset name and V2 activity code(s) it feeds |
| `factor` | robustness files only: uncertainty factor (`X1_mat_AP`, `X2_relation_AP`…) |

Values of `kind`:

| `kind` | File | Main content |
|---|---|---|
| `log` | `run.log` | |
| `config` | `study/config/*.yml` | configuration files (the one used has `active_config`) |
| `matrix_activities_pressures` | `study/config/matrice_act_press.ods` | |
| `matrix_sensitivity` | `study/config/matrice_sensi_hab_press.ods` | |
| `input_grid` | `study/inputs/grid.gpkg` | analysis grid |
| `input_habitats` | `study/inputs/habbenth.gpkg` | habitats per cell (several rows per cell) |
| `input_activity` | `study/inputs/<dataset>.gpkg` | intensity of one activity per cell |
| `result_indices` | `outputs/resultats_analyse_<date>.gpkg` | layer `grille_indices`: `idmesh`, `srm`, `surfmer`, `IMA_option1/2`, `IPC_option1/2`, `REXC_E1`, `REFC_E1`, `REXC_E1_norm`, `IC`, `IQ_A_mean`, `IC_AP_mean`, `IQ_H_mean`, `IC_HP_mean` |
| `result_details` | `outputs/details_analyse_<date>.gpkg` | layers `activites` (one column per activity code), `pressions_norm`, `sensibilites`, `effets` (one column per pressure) |
| `result_monte_carlo` | `outputs/monte_carlo_<date>.gpkg` | Monte Carlo statistics per cell |
| `map_refc`, `map_rexc`, `map_confidence` | `outputs/maps/*.gpkg` | one indicator per cell (subset of `result_indices`) |
| `map_geojson` | `outputs/maps/*.geojson` | same maps, for the pipeline's own HTML map |
| `robustness`, `robustness_map_html`, `robustness_geojson` | `outputs/robustesse_det/<factor>/…` | deterministic robustness per factor |
| `report_html` | `outputs/report.html` | pipeline report |
| `map_html` | `outputs/carte_interactive.html` | pipeline interactive map (needs internet: Leaflet CDN, OSM tiles) |
| `graph`, `graph_data` | `outputs/graphs/*` | graphs (JPEG) and their data (CSV) |
| `summary` | `outputs/stats_recap_<date>.json` | same content as `summary` in `run.json` |
| `pipeline_info` | `outputs/run_info.json` | |
| `other` | anything else | |

New kinds may be added without changing `schema_version`: applications should ignore kinds
they do not know.

## Access for applications

* On EDITO (web application deployed as a service): the service receives the user's S3
  credentials (`AWS_*` variables) and reads the user's bucket.
* During development, outside EDITO: temporary S3 credentials of the EDITO account (see
  `edito-docker/DEPLOIEMENT_EDITO.md`).
