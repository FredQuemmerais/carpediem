#!/usr/bin/env python3
"""Build the Carpediem V2 study folder for Mer du Nord: input GeoPackages and config.yml.

Rules: see migration_v1_v2/README.md; decision numbers in comments (D16...) refer to the
study's decision register (not versioned).

Inputs
  --inputs    official INPUTS folder (grid shapefile, multi-layer GeoPackages)
  --workbook  correspondence workbook (sheet Activités), see ref/modele_correspondances_v1_v2.xlsx
  --matrices  folder with matrice_act_press.ods and matrice_sensi_hab_press.ods (outputs of
              build_matrice_act_press.py and build_matrice_sensi.py); copied into config/
              unless it is already the config/ folder of the study
Outputs
  --out-dir     study folder: config/ (the two matrices, config.yml for the complete run,
                config_analyse_simple.yml without Monte Carlo and robustness), inputs/ (one
                GeoPackage per dataset, layer named after the Table_code)
  --report-dir  conversion_study_log.txt (default: --out-dir)
"""
import argparse
import copy
import os
import shutil
import sys
import tempfile

import geopandas as gpd
import openpyxl
import pyogrio
import yaml

CRS = 3035
ID = "id_3nq_3"
SRM = ["MCE", "MDN", "EEZ_BE", "BELTS_SUND", "SKGR", "KTG"]          # D28
GRID_SHP = "Carroyage_Gimel_3nq_EUNS_EPSG3035.shp"
HAB = ("habEunis_carroyes.gpkg", "hab_eunis")                          # layer used in January


def retained_layers(workbook):
    """layer (file::layer) -> (V2 code, intensity column). One code per layer (D17, D19);
    layers without official activity (pipelines, D20) and excluded layers (D21) are skipped."""
    wb = openpyxl.load_workbook(workbook)
    out, skipped = {}, []
    for r in wb["Activités"].iter_rows(min_row=3, values_only=True):
        if not r[0]:
            continue
        layer, col, official, decision = r[0].replace(" ", ""), r[1], r[2], (r[9] or "").strip()
        code = r[10] or r[5]
        if decision not in ("Validé", "Modifié"):
            skipped.append(f"{layer}: excluded ({decision})")
            continue
        if not official:
            skipped.append(f"{layer}: no official activity, not used (D20)")
            continue
        out.setdefault(layer, (code, col))
    return out, sorted(set(skipped))


def write(gdf, path, layer):
    """Write a one-layer GeoPackage. Written in a local temporary folder first, then moved:
    SQLite cannot always create a database directly on a shared or network folder."""
    tmp = os.path.join(tempfile.gettempdir(), f"carpediem_{os.getpid()}_{layer}.gpkg")
    for f in (tmp, path):
        if os.path.exists(f):
            os.remove(f)
    pyogrio.write_dataframe(gdf, tmp, layer=layer, driver="GPKG")
    shutil.move(tmp, path)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--inputs", required=True)
    ap.add_argument("--workbook", required=True)
    ap.add_argument("--matrices", required=True)
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--report-dir")
    a = ap.parse_args()
    a.report_dir = a.report_dir or a.out_dir
    os.makedirs(a.report_dir, exist_ok=True)
    inp = os.path.join(a.out_dir, "inputs")
    cfg_dir = os.path.join(a.out_dir, "config")
    os.makedirs(inp, exist_ok=True)
    os.makedirs(cfg_dir, exist_ok=True)
    log = []

    # ---- Grid (D39) ------------------------------------------------------------
    g = gpd.read_file(os.path.join(a.inputs, GRID_SHP))
    if g.crs is None or g.crs.to_epsg() != CRS:
        sys.exit(f"grid: unexpected CRS {g.crs}")
    if g[ID].duplicated().any() or not g.is_valid.all():
        sys.exit("grid: duplicated ids or invalid geometries")
    g = g[[ID, "srm", "sf_cell", "sf_mer", "geometry"]].rename_geometry("geom")
    write(g, os.path.join(inp, "grid.gpkg"), "grid")
    log.append(f"grid: {len(g)} cells, valid geometries, SRM {g.srm.value_counts().to_dict()}, "
               f"cells with sf_mer <= 0 or empty (dropped by the V2): {int((g.sf_mer.fillna(0) <= 0).sum())}")

    # ---- Habitats (D40) --------------------------------------------------------
    h = gpd.read_file(os.path.join(a.inputs, HAB[0]), layer=HAB[1])
    h = h[[ID, "SRM", "sf_cell", "sf_mer", "sf_hab", "cod_eunis", "geometry"]].rename_geometry("geom")
    h["hab_iq"] = None                                                     # D25: no habitat quality index
    h["hab_iq"] = h["hab_iq"].astype("float64")
    if not h.is_valid.all():
        sys.exit("habitats: invalid geometries")
    write(h, os.path.join(inp, "habbenth.gpkg"), "habbenth")
    log.append(f"habbenth: {len(h)} rows (cell x habitat), {h[ID].nunique()} cells, "
               f"{int(h.cod_eunis.isna().sum())} rows without EUNIS code, hab_iq empty")

    # ---- Activities (D16 to D21, D41) -------------------------------------------
    layers, skipped = retained_layers(a.workbook)
    log += [f"skipped: {s}" for s in skipped]
    datasets, mapping = [], {}
    for layer, (code, col) in layers.items():
        base, lname = layer.split("::")                                    # e.g. peche::static_gear
        fname = base + ".gpkg"
        d = gpd.read_file(os.path.join(a.inputs, fname), layer=lname)
        # Table_code (e.g. peche_static_gear) and a unique intensity column name (D41)
        tc = base if lname == "intensity" else f"{base}_{lname}"
        icol = f"i_{tc}"
        if d[ID].duplicated().any():
            sys.exit(f"{layer}: duplicated cell ids")
        reproj = d.crs.to_epsg() != CRS
        d = d.to_crs(CRS) if reproj else d
        d = d[[ID, col, "geometry"]].rename(columns={col: icol}).rename_geometry("geom")
        write(d, os.path.join(inp, f"{tc}.gpkg"), tc)
        datasets.append(dict(Table_code=tc, enabled=True, Source="gpkg", id_col=ID, geom_col="geom",
                             Data_type="A", intensity_cols=icol))
        mapping[icol] = code
        log.append(f"{tc}: {len(d)} cells, column {col} -> {icol} -> {code}"
                   f"{', reprojected from EPSG:4326' if reproj else ''}"
                   f", empty values: {int(d[icol].isna().sum())}")

    # ---- Matrices ---------------------------------------------------------------
    for f in ("matrice_act_press.ods", "matrice_sensi_hab_press.ods"):
        src, dst = os.path.join(a.matrices, f), os.path.join(cfg_dir, f)
        if not os.path.exists(src):
            sys.exit(f"missing matrix: {src}")
        if os.path.abspath(src) != os.path.abspath(dst):
            shutil.copy(src, dst)

    # ---- config.yml (D25 to D32, D42 to D44) ------------------------------------
    cfg = {
        "project": {"srm": ", ".join(SRM)},
        "db": {"data_source": "flat_files"},
        "inputs": {"ods": {"act_press": "config/matrice_act_press.ods",
                           "sensi_hab_press": "config/matrice_sensi_hab_press.ods"}},
        "Calc": {"agreg_sensib": 2, "th_sensib": 100.0, "l_corr_surfhab": False},
        "Outputs": {"write_maps": True, "verbose_maps": False, "export_details": True, "export_db": False,
                    "export_db_details": False, "export_maps_Ai": True, "export_maps_Pj": True,
                    "export_maps_REX_Pj": True},
        "datasets": [
            dict(Table_code="grid", enabled=True, Data_type="G", Source="gpkg", id_col=ID, geom_col="geom",
                 srm_col="srm", surfmesh_col="sf_cell", surfmer_col="sf_mer"),
            dict(Table_code="habbenth", enabled=True, Data_type="E", Source="gpkg", id_col=ID, geom_col="geom",
                 surfmesh_col="sf_cell", surfmer_col="sf_mer", surfhab_col="sf_hab", habcode_col="cod_eunis"),
        ] + datasets,
        "act_col_mapping": mapping,
        "units": {"areas": {"target": "m2", "allowed": ["m2", "km2", "ha"],
                            "HabBenth_z": {"E_surfhab": "km2", "E_surfmer": "km2"},
                            "Grid_z": {"G_surfmesh": "km2", "G_surfmer": "km2"}}},
        "execution": {"seed": 42},
        "analyses": {
            "monte_carlo": {
                "enabled": True, "nbsimul": 250, "thres_stat": [10.0, 25.0],
                "factors": {"X1_mat_AP": 1, "X2_relation_AP": 1, "X3_var_sensi": 1, "X4_var_ccr_AP": 1,
                            "X5_var_act": 0, "X6_typo_act": 1, "X7_mat_sensi": 1, "X8_var_hab": 0,
                            "X9_norm_form": 1, "X10_agreg_press": 1},
                "export_simul": False, "export_result": False,
            },
            "robustesse_deterministe": {"enabled": True, "scenarios": {
                "X1_mat_AP": [2, 3, 4], "X2_relation_AP": [2, 3, 4, 5], "X7_mat_sensi": [2, 3, 4],
                "X6_typo_act": [2, 3, 4], "X9_norm_form": [2, 3, 4], "X10_agreg_press": [2, 3, 4]}},
            "robustesse_stoch": {"enabled": False},
            "sensibilite_formelle": {"enabled": False, "methode": "shapley", "m": 100, "nrep": 10, "facteurs": {
                "X1_mat_AP": [2, 3, 4], "X2_relation_AP": [2, 3, 4, 5], "X7_mat_sensi": [2, 3, 4],
                "X6_typo_act": [2, 3, 4], "X3_var_sensi": [0, 1], "X4_var_ccr_AP": [0, 1],
                "X9_norm_form": [2, 3, 4], "X10_agreg_press": [2, 3, 4]}},
        },
        "Step3": {"enabled": True, "mode": "simplifie", "zone_field": "srm"},
        "report": {"enabled": True, "output_file": "report.html", "maps_iframe_file": "carte_interactive.html",
                   "include_plots": True, "include_monte_carlo": True, "include_files_links": True},
    }
    def dump(conf, name, title):
        header = (f"# Carpediem V2, study Mer du Nord (Norsaic EUGNS), flat files: {title}\n"
                  "# Generated by migration_v1_v2/build_study.py; decisions in\n"
                  "# the study's decision register\n")
        with open(os.path.join(cfg_dir, name), "w", encoding="utf-8") as f:
            f.write(header)
            yaml.safe_dump(conf, f, sort_keys=False, allow_unicode=True, default_flow_style=False, width=100)

    dump(cfg, "config.yml", "complete run (Monte Carlo and deterministic robustness, D43)")
    simple = copy.deepcopy(cfg)
    simple["analyses"]["monte_carlo"]["enabled"] = False
    simple["analyses"]["robustesse_deterministe"]["enabled"] = False
    dump(simple, "config_analyse_simple.yml", "simple analysis only (no Monte Carlo, no robustness)")
    log.append("config: config.yml (complete run), config_analyse_simple.yml (simple analysis)")

    with open(os.path.join(a.report_dir, "conversion_study_log.txt"), "w", encoding="utf-8") as f:
        f.write("\n".join(log) + "\n")
    print("\n".join(log))


if __name__ == "__main__":
    main()
