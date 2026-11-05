# Carpediem V2 container (EDITO process)

This folder contains what is needed to run the Carpediem V2 batch pipeline
(`execution/run_batch.R`) in a container, as expected by the EDITO process catalogue:
non interactive, configured through environment variables, inputs and outputs on
local folders or on S3 storage (EDITO MinIO).

Files:

* `../Dockerfile` (with `../Dockerfile.dockerignore`): image based on `rocker/r-ver:4.5.1` (R version of `renv.lock`), R
  packages restored with renv at their locked versions, `rclone` for S3 transfers.
* `entrypoint.sh`: fetches the study, runs the pipeline, publishes the outputs.

## Build

From the repository root (branch `v2-engineering`, based on `carpediem_v2`):

```bash
docker build -f edito-docker/Dockerfile -t carpediem:dev .
```

For EDITO the image must be `linux/amd64` and pushed to a public registry, for instance:

```bash
docker buildx build --platform linux/amd64 -f edito-docker/Dockerfile \
  --build-arg CARPEDIEM_VERSION=<version> --build-arg CARPEDIEM_COMMIT=$(git rev-parse --short HEAD) \
  -t <registry>/<account>/carpediem:<version> --push .
```

`CARPEDIEM_VERSION` and `CARPEDIEM_COMMIT` are recorded in the `run.json` of each run.

On an Apple Silicon Mac, a local `linux/arm64` build works too (plain `docker build`),
but is slower to build since fewer binary R packages are available for arm64.

## Run

The image contains no study: `CARPEDIEM_STUDY` is required. A study has the layout
`config/` (YAML configuration and the two ODS matrices) and `inputs/` (GeoPackages), as a
folder or as a `.tar.gz` archive of that folder.

With a study folder (results in `./out`):

```bash
docker run --rm \
  -v /path/to/my_study:/study:ro \
  -v "$PWD/out:/output" \
  -e CARPEDIEM_STUDY=/study \
  -e CARPEDIEM_CONFIG=config/config.yml \
  carpediem:dev
```

On EDITO, set `CARPEDIEM_OUTPUT` to `s3://${AWS_BUCKET_NAME}/carpediem/runs` (form
default, see `../DEPLOIEMENT_EDITO.md`): the container writes the run folder there itself.
Left empty, it writes to `/output-data`, the shared volume of the processes created by
add-your-process, which their `copy-output` container uploads to
`s3://oidc-<user>/<process run name>/`. Outside EDITO:

```bash
docker run --rm \
  -e CARPEDIEM_STUDY=https://minio.dive.edito.eu/oidc-<user>/carpediem/mer_du_nord/study.tar.gz \
  -e CARPEDIEM_CONFIG=config/config_analyse_simple.yml \
  -e CARPEDIEM_OUTPUT=s3://oidc-<user>/carpediem/runs \
  -e CARPEDIEM_RUN_DESCRIPTION="North Sea, test" \
  -e AWS_ACCESS_KEY_ID=... -e AWS_SECRET_ACCESS_KEY=... -e AWS_SESSION_TOKEN=... \
  -e AWS_S3_ENDPOINT=minio.dive.edito.eu \
  carpediem:dev
```

## Environment variables

| Variable | Default | Description |
|---|---|---|
| `CARPEDIEM_STUDY` | (required) | Study: a folder (local path or `s3://bucket/prefix`) or a `.tar.gz` / `.tgz` archive of it (local path, `s3://bucket/key` or `https://` URL, e.g. a public file on EDITO). Must contain `config/` and `inputs/` (an archive may also contain a single top folder holding them) |
| `CARPEDIEM_CONFIG` | `config/config.yml` | YAML config, relative to the study folder. Use `db.data_source: flat_files` to read the GeoPackages of `inputs/` |
| `CARPEDIEM_OUTPUT` | `/output-data` if that folder exists, else `/output` | Root under which the run folder `<run id>/` is written: local path or `s3://bucket/prefix`. On EDITO (add-your-process), `/output-data` is the shared volume that the `copy-output` container uploads to `s3://oidc-<user>/<process run name>/` |
| `CARPEDIEM_RUN_DESCRIPTION` | (empty) | Free text describing the run, stored in `run.json`; its short form is the suffix of the run id (without it, the configuration name) |
| `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN` | | S3 credentials, only needed for `s3://` locations |
| `${AWS_BUCKET_NAME}` in `CARPEDIEM_STUDY` / `CARPEDIEM_OUTPUT` | | Replaced by the value of `AWS_BUCKET_NAME` (the user's bucket, injected by EDITO), e.g. `s3://${AWS_BUCKET_NAME}/carpediem/outputs` |
| `AWS_S3_ENDPOINT` | `minio.dive.edito.eu` | S3 endpoint (with or without `https://`). If unset, `S3_ENDPOINT` (full URL, also injected by EDITO) is used |
| `CARPEDIEM_WORKERS` | CPU limit of the container minus one | Number of parallel workers for Monte Carlo and Shapley |

Outputs: one folder per run, `<CARPEDIEM_OUTPUT>/<run id>/`, with `run.json` (description of
the run), `run.log`, `study/` (copy of the study used) and `outputs/` (results). Layout,
run id and `run.json` are specified in `../RUNS_LAYOUT.md`. `run.json` is written with status
`running` at the start, and the whole folder at the end, also when the run fails. The
container exit code is the pipeline's (2 if the study or the configuration is missing, 3 if
the run folder could not be written).

## Notes and known issues

* The image needs the branch `v2-engineering`: its `renv.lock` is consistent (the
  original one fails to restore because of `dbplyr` / `dplyr`), and the parallel steps
  size their workers from the container CPU limits (`parallelly::availableCores()`).
* Worker count can be forced with `CARPEDIEM_WORKERS` (or `execution.workers` in the
  YAML); Monte Carlo and Shapley runs are reproducible when a `seed` is set.
* The S3 variable names follow the Onyxia convention used by the EDITO datalab; EDITO
  support confirmed on 2026-10-19 that processes receive the same `AWS_*` variables (plus
  `S3_ENDPOINT` and `AWS_BUCKET_NAME`).
