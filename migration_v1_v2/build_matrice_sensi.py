#!/usr/bin/env python3
"""Build the Carpediem V2 habitat sensitivity matrix (matrice_sensi_hab_press.ods) for the
Mer du Nord study, from the official sensitivity matrix (matrice_sensibilite_CD.ods).

Rules: see migration_v1_v2/README.md; decision numbers in comments (D22...) refer to the
study's decision register (not versioned).

Inputs
  --sensi     official sensitivity matrix (sheets median, precaution; columns cd_eunis,
              Pr_* sensitivity 0..5 or 99 = no data, Pr_*_icas confidence 1/3/5)
  --workbook  correspondence workbook (sheet Pressions), see ref/modele_correspondances_v1_v2.xlsx
  --habitats  optional habitat GeoPackage::layer, to report habitat codes missing from the matrix
Outputs
  in --out-dir (the config/ folder of the V2 study):
    matrice_sensi_hab_press.ods   sheets median, precaution, binaire; rows Type = sensibilite / IC
  in --report-dir (default: --out-dir):
    conversion_sensi_log.txt      counts and checks
"""
import argparse
import os
import sqlite3
import sys

import numpy as np
import openpyxl
import pandas as pd

NODATA = 99


def evaluated_pressures(path):
    wb = openpyxl.load_workbook(path)
    out = {}
    for r in wb["Pressions"].iter_rows(min_row=3, values_only=True):
        if r[0] and (r[7] or "").strip() == "Oui":
            v2 = r[9] or r[3]
            if v2 in out:
                sys.exit(f"Two evaluated official pressures for {v2}")
            out[v2] = r[0]
    return out  # V2 code -> official code


def convert_sheet(df, p_map, log, label):
    v2_codes = list(p_map)
    sens = pd.DataFrame({"Type": "sensibilite", "Code_H": df["cd_eunis"].astype(str).str.strip()})
    ic = pd.DataFrame({"Type": "IC", "Code_H": sens["Code_H"]})
    n_empty_s = n_ic_cleared = 0
    for v2 in v2_codes:
        off = p_map[v2]
        s = pd.to_numeric(df[off], errors="coerce")
        n_empty_s += int(s.isna().sum())
        sens[v2] = s.fillna(NODATA)                                        # D35
        c = pd.to_numeric(df.get(off + "_icas"), errors="coerce") if off + "_icas" in df else pd.Series(np.nan, index=df.index)
        bad = c.isna() | (c == NODATA) | (c < 0) | (c > 5)
        n_ic_cleared += int(bad.sum())
        ic[v2] = c.where(~bad)                                             # D36 (empty = no information)
    log.append(f"[{label}] empty sensitivity cells set to {NODATA}: {n_empty_s}; "
               f"IC cells left empty (99, empty or out of 0..5): {n_ic_cleared}")
    return sens, ic


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--sensi", required=True)
    ap.add_argument("--workbook", required=True)
    ap.add_argument("--habitats")
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--report-dir")
    a = ap.parse_args()
    a.report_dir = a.report_dir or a.out_dir
    os.makedirs(a.out_dir, exist_ok=True)
    os.makedirs(a.report_dir, exist_ok=True)
    log = []

    p_map = evaluated_pressures(a.workbook)
    log.append(f"Evaluated pressures: {len(p_map)} (other V2 pressures absent: no data, D22)")
    src = {s: pd.read_excel(a.sensi, engine="odf", sheet_name=s) for s in ("median", "precaution")}
    for s, df in src.items():
        missing = [o for o in p_map.values() if o not in df.columns]
        if missing:
            sys.exit(f"[{s}] official pressures missing from the sensitivity matrix: {missing}")
        if df["cd_eunis"].duplicated().any():
            sys.exit(f"[{s}] duplicated habitat codes")
    if not src["median"]["cd_eunis"].equals(src["precaution"]["cd_eunis"]):
        sys.exit("median and precaution sheets do not list the same habitats")
    log.append(f"Habitats: {len(src['median'])}")

    sheets = {}
    for s in ("median", "precaution"):
        sens, ic = convert_sheet(src[s], p_map, log, s)
        sheets[s] = (sens, ic)

    # D23: binary = 5 if the median sensitivity is not zero, 0 if zero; no data (99) kept (D37)
    med_s, med_ic = sheets["median"]
    bin_s = med_s.copy()
    for v2 in p_map:
        x = med_s[v2]
        bin_s[v2] = np.where(x == NODATA, NODATA, np.where(x > 0, 5, 0))
    sheets["binaire"] = (bin_s, med_ic.copy())                            # IC of the median (D37)

    out = os.path.join(a.out_dir, "matrice_sensi_hab_press.ods")
    with pd.ExcelWriter(out, engine="odf") as w:
        for s in ("median", "precaution", "binaire"):
            sens, ic = sheets[s]
            pd.concat([sens, ic], ignore_index=True).to_excel(w, sheet_name=s, index=False)

    # Value distribution (median) for the report
    vals = med_s[list(p_map)].to_numpy().ravel()
    log.append("Median sensitivity values: " +
               ", ".join(f"{k}: {v}" for k, v in sorted(pd.Series(vals).value_counts().items())))

    # Habitat codes of the study absent from the matrix (no sensitivity, treated as no data)
    if a.habitats:
        gpkg, layer = a.habitats.split("::")
        con = sqlite3.connect(gpkg)
        h = pd.read_sql(f'select cod_eunis, count(*) n, sum(sf_hab) s from "{layer}" group by cod_eunis', con)
        miss = h[~h.cod_eunis.isin(med_s.Code_H)]
        log.append(f"Habitat layer {layer}: {len(h)} codes, {len(miss)} absent from the matrix "
                   f"({int(miss.n.sum())} of {int(h.n.sum())} rows, "
                   f"{miss.s.sum():.1f} of {h.s.sum():.1f} habitat surface units)")
        for _, r in miss.iterrows():
            log.append(f"  absent: {r.cod_eunis if r.cod_eunis else '(empty code)'}: {int(r.n)} rows, surface {r.s:.2f}")

    with open(os.path.join(a.report_dir, "conversion_sensi_log.txt"), "w", encoding="utf-8") as f:
        f.write("\n".join(log) + "\n")
    print("\n".join(log))
    print(f"Written: {out}")


if __name__ == "__main__":
    main()
