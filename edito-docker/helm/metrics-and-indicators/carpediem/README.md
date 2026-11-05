# Carpediem: risk of cumulative effects on benthic habitats

Carpediem maps the risk of cumulative effects (RCE) of human activities on benthic
habitats. It crosses a map of benthic habitats and their sensitivity to pressures with the
distribution and intensity of human activities and the pressures they generate. It produces
cumulative effect indices per grid cell, a confidence index and, through Monte Carlo
simulations and robustness analyses, a measure of the variability of the results.

The method was developed by Quemmerais-Amice et al. (2020) during the Carpediem project,
led in France to support the Marine Strategy Framework Directive, and builds on Halpern et
al. (2008). It has been applied to French waters and to cross-border case studies in Europe.

## Contact

* Antonin Gimard, Cerema: antonin.gimard@cerema.fr
* Frédéric Quemmerais-Amice, OFB: frederic.quemmerais-amice@ofb.gouv.fr

Source code: https://github.com/FredQuemmerais/carpediem (branch `carpediem_v2`), licence
AGPL-3.0. Integration in EDITO: EGM, for Cerema.

## Running the process

With the default values, the process runs a simple analysis of the North Sea example study
(Norsaic EUGNS: 14,521 grid cells, 19 activities, 24 pressures) in about one minute, and
writes the results to your own storage, in `carpediem/runs/<run id>/`.

| Parameter | Default | Description |
|---|---|---|
| `CARPEDIEM_STUDY` | North Sea example (`.tar.gz` archive) | Study to run: a `.tar.gz` archive (`https://` URL or `s3://bucket/key`) or a folder (`s3://bucket/prefix`), containing `config/` and `inputs/` |
| `CARPEDIEM_CONFIG` | `config/config_analyse_simple.yml` | Configuration, relative to the study. `config/config.yml` adds the Monte Carlo (250 simulations) and robustness analyses |
| `CARPEDIEM_OUTPUT` | `s3://${AWS_BUCKET_NAME}/carpediem/runs` | Where the runs are stored; `${AWS_BUCKET_NAME}` is your bucket |
| `CARPEDIEM_RUN_DESCRIPTION` | (empty) | Short description of the run, shown in the list of runs |
| `CARPEDIEM_WORKERS` | (empty: CPUs minus one) | Number of parallel workers for the Monte Carlo analysis |

Indicative run times for the North Sea study: simple analysis about 1 minute; Monte Carlo
and robustness analyses about 20 minutes with 6 CPUs. Memory: 2 GiB are enough for the
simple analysis; allow up to 8 GiB with the Monte Carlo analysis on 6 CPUs.

## Running your own study

A study is a folder with the following layout, uploaded to your EDITO storage (as a folder
or as a `.tar.gz` archive):

```
config/
  config.yml                    configuration (YAML): sub-regions, datasets, analyses
  matrice_act_press.ods         activity-pressure matrix (relative contribution coefficients)
  matrice_sensi_hab_press.ods   habitat sensitivity matrix
inputs/
  grid.gpkg                     analysis grid
  habbenth.gpkg                 benthic habitats per grid cell (EUNIS codes)
  <activity>.gpkg               one GeoPackage per activity dataset (intensity per cell)
```

The files of the North Sea example are a good starting point: download the default
archive, adapt the configuration and the data, upload the new study and set
`CARPEDIEM_STUDY` to its location.

## Results

Each run writes one folder, `<CARPEDIEM_OUTPUT>/<run id>/`, where the run id is the UTC
start time followed by a short form of the description (e.g.
`20261022T085333Z-north-sea-reference`):

* `run.json`: description of the run (status, dates, parameters, resources, summary
  indicators, list of the files with their role). It is written at the start with the status
  `running` and completed at the end.
* `run.log`: full log.
* `study/`: copy of the study used by the run.
* `outputs/`: results. Main files: `resultats_analyse_<date>.gpkg` (indices per grid cell:
  REFC, REXC, IMA, IPC, confidence index), `details_analyse_<date>.gpkg` (activities,
  pressures, sensitivities and effects per cell), `monte_carlo_<date>.gpkg`,
  `robustesse_det/`, `report.html` (report) and `carte_interactive.html` (map). GeoPackages
  are in EPSG:3035.

## References

* Quemmerais-Amice F., Barrere J., La Rivière M., Contin G., Bailly D. (2020). A methodology
  and tool for mapping the risk of cumulative effects on benthic habitats. Frontiers in
  Marine Science, 7, 569205.
* Halpern B. S. et al. (2008). A global map of human impact on marine ecosystems. Science,
  319, 948-952.
