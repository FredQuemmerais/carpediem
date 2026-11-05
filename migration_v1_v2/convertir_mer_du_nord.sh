#!/usr/bin/env bash
# Conversion de l'étude Mer du Nord (format Carpediem+ officiel / V1) au format Carpediem V2.
# A lancer depuis n'importe où ; les chemins sont relatifs à la racine du projet.
#
# Entrées (non modifiées) :
#   Donnees Mer du Nord/INPUTS/   données d'origine (grille Shapefile, GeoPackages multi-couches)
#   Donnees Mer du Nord/PARAM/    fichier de paramétrage officiel et matrice de sensibilité
#   classeur de correspondances validé (non versionné ; chemin modifiable avec
#   CARPEDIEM_WORKBOOK, format : ref/modele_correspondances_v1_v2.xlsx)
# Sorties :
#   Donnees Mer du Nord/V2/study/       étude V2 prête pour le conteneur (config/ + inputs/)
#   Donnees Mer du Nord/V2/study.tar.gz archive de cette étude (à déposer sur EDITO)
#   Donnees Mer du Nord/V2/conversion/  rapports de conversion (CSV, journaux)
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SRC="${ROOT}/Donnees Mer du Nord"
V2="${SRC}/V2"
WORKBOOK="${CARPEDIEM_WORKBOOK:-${ROOT}/documentation/Correspondances_Mer_du_Nord_V2_réponse_client.xlsx}"
HERE="${ROOT}/migration_v1_v2"

mkdir -p "${V2}/study/config" "${V2}/conversion"

python3 "${HERE}/build_matrice_act_press.py" \
  --param "${SRC}/PARAM/fichier_parametrage_Norsaic - EUGNS.ods" \
  --workbook "${WORKBOOK}" \
  --template "${HERE}/ref/matrice_act_press_baie_de_seine.ods" \
  --out-dir "${V2}/study/config" --report-dir "${V2}/conversion"

python3 "${HERE}/build_matrice_sensi.py" \
  --sensi "${SRC}/PARAM/matrice_sensibilite_CD.ods" \
  --workbook "${WORKBOOK}" \
  --habitats "${SRC}/INPUTS/habEunis_carroyes.gpkg::hab_eunis" \
  --out-dir "${V2}/study/config" --report-dir "${V2}/conversion"

python3 "${HERE}/build_study.py" \
  --inputs "${SRC}/INPUTS" \
  --workbook "${WORKBOOK}" \
  --matrices "${V2}/study/config" \
  --out-dir "${V2}/study" --report-dir "${V2}/conversion"

# Archive of the study (config/ + inputs/), for EDITO: CARPEDIEM_STUDY can point to it
# (public https:// URL, s3:// key or local path). Reproducible: fixed order, dates and owner.
tar --sort=name --mtime="2026-01-01 00:00:00Z" --owner=0 --group=0 --numeric-owner \
  -C "${V2}/study" -cf - config inputs | gzip -n > "${V2}/study.tar.gz"

echo "Etude V2 : ${V2}/study (archive : ${V2}/study.tar.gz)"
