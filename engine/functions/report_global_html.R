# engine/functions/report_global_html.R
# version du 11.08.2026

write_global_report_html <- function(config, res1, res2 = NULL,
                                     res2b = NULL, res2c = NULL,
                                     res2d = NULL, res3 = NULL) {
  
  # ---- Guards ----
  stopifnot(
    "res1 doit être une liste"         = is.list(res1),
    "res1$ctx manquant"                = !is.null(res1$ctx),
    "res1$ctx$outputsdir manquant"     = !is.null(res1$ctx$outputsdir),
    "res1$ctx$paths manquant"          = !is.null(res1$ctx$paths)
  )
  
  run_dir <- res1$ctx$outputsdir
  if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE,
                                       showWarnings = FALSE)
  
  # ---- Helpers ----
  esc <- function(x) {
    x <- as.character(x)
    x <- gsub("&",  "&amp;",  x, fixed = TRUE)
    x <- gsub("<",  "&lt;",   x, fixed = TRUE)
    x <- gsub(">",  "&gt;",   x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x
  }
  
  fmt_num <- function(x, digits = 2) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) return("NA")
    suppressWarnings({
      x <- as.numeric(x)
      if (all(is.na(x))) return("NA")
      format(round(x, digits), nsmall = digits, trim = TRUE)
    })
  }
  
  fmt_pct <- function(x, digits = 1) paste0(fmt_num(x, digits), " %")
  
  # Chemin relatif à run_dir si possible, sinon URI file:// absolu
  rel_or_file_uri <- function(path_abs) {
    p <- normalizePath(as.character(path_abs), winslash = "/", mustWork = FALSE)
    rd <- normalizePath(run_dir, winslash = "/", mustWork = FALSE)
    if (startsWith(p, rd)) {
      sub(paste0("^", rd, "/?"), "", p)
    } else {
      paste0("file:///", p)
    }
  }
  
  file_link <- function(path_abs, label = NULL) {
    if (is.null(path_abs) || !nzchar(as.character(path_abs))) return("")
    p   <- as.character(path_abs)
    lbl <- if (!is.null(label)) esc(label) else esc(basename(p))
    if (!file.exists(p) && !dir.exists(p)) {
      return(paste0("<div class='file missing'>", lbl,
                    ": <span class='muted'>absent</span> (", esc(p), ")</div>"))
    }
    href <- esc(rel_or_file_uri(p))
    paste0("<div class='file'><a href='", href,
           "' target='_blank' rel='noopener'>", lbl, "</a></div>")
  }
  
  kpi_card <- function(label, value) {
    paste0("<div class='kpi'><div class='label'>", esc(label),
           "</div><div class='value'>", esc(value), "</div></div>")
  }
  
  # ---- Meta ----
  ts          <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  run_dir_disp <- gsub("\\\\", "/", run_dir)
  
  # ---- Synthèse Step1 ----
  stats1    <- res1$stats
  n_mailles <- tryCatch(nrow(res1$grid_final), error = function(e) NA_integer_)
  n_act     <- if (!is.null(res1$activites))    max(0, ncol(res1$activites) - 1)    else NA_integer_
  n_pr      <- if (!is.null(res1$effets))        max(0, ncol(res1$effets) - 1)       else
    if (!is.null(res1$pressions_norm)) max(0, ncol(res1$pressions_norm) - 1) else NA_integer_
  
  ima_moy  <- stats1$ima_opt1_moy %||% NA_real_
  ipc_moy  <- stats1$ipc_opt1_moy %||% NA_real_
  refc_moy <- stats1$refc_moy     %||% NA_real_
  ic_moy   <- stats1$ic_moy       %||% NA_real_
  rexc_moy <- stats1$rexc_moy     %||% NA_real_
  
  # ---- Visuels ----
  leaflet_html <- file.path(run_dir, "carte_interactive.html")
  has_leaflet  <- file.exists(leaflet_html)
  
  graphs_dir <- file.path(run_dir, "graphs")
  graph_imgs <- character(0)
  if (dir.exists(graphs_dir)) {
    whitelist  <- c("Histogramme_REFC.jpeg", "Bilan_activites.jpeg",
                    "Bilan_pressions_N3.jpeg",  "Radar_IC.jpeg")
    graph_imgs <- whitelist[whitelist %in% list.files(graphs_dir)]
  }
  
  # ---- Fichiers Step1 ----
  gpkg_principal <- res1$ctx$paths$gpkg_principal
  gpkg_details   <- res1$ctx$paths$gpkg_details
  stats_json     <- res1$ctx$paths$stats_json
  
  # ---- Labels facteurs ----
  factor_labels <- list(
    X1_mat_AP = list(
      title = "Matrice activité-pression",
      map   = c("1" = "Aléa", "2" = "Médiane", "3" = "Précaution", "4" = "Binaire")
    ),
    X2_relation_AP = list(
      title = "Forme relation A-P",
      map   = c("1" = "Aléa", "2" = "Linéaire", "3" = "Optimiste",
                "4" = "Pessimiste", "5" = "Logistique")
    ),
    X3_var_sensi = list(
      title = "Incertitude sensibilités",
      map   = c("0" = "Sans IC", "1" = "Avec IC")
    ),
    X4_var_ccr_AP = list(
      title = "Incertitude CCR",
      map   = c("0" = "Sans IC", "1" = "Avec IC")
    ),
    X5_var_act = list(
      title = "Qualité données activités",
      map   = c("0" = "Sans IQ", "1" = "Avec IQ")
    ),
    X6_typo_act = list(
      title = "Granularité typologie activité",
      map   = c("1" = "Aléa", "2" = "Natif", "3" = "Agrégé N2", "4" = "Agrégé N1")
    ),
    X7_mat_sensi = list(
      title = "Matrice sensibilité",
      map   = c("1" = "Aléa", "2" = "Précaution", "3" = "Médiane", "4" = "Binaire")
    ),
    X8_var_hab = list(
      title = "Perturbation surfaces habitats",
      map   = c("0" = "Sans", "1" = "Avec")
    ),
    X9_norm_form = list(
      title = "Forme de normalisation",
      map   = c("1" = "Aléa", "2" = "Log", "3" = "Linéaire", "4" = "Sigmoïde")
    ),
    X10_agreg_press = list(
      title = "Agrégation des pressions",
      map   = c("1" = "Aléa", "2" = "Additif", "3" = "Synergique", "4" = "Antagoniste")
    )
  )
  
  fmt_factor <- function(name, factors) {
    v    <- if (!is.null(factors[[name]])) as.character(factors[[name]]) else NA_character_
    meta <- factor_labels[[name]]
    title <- if (!is.null(meta$title)) meta$title else name
    desc  <- if (!is.na(v) && !is.null(meta$map[[v]])) meta$map[[v]] else "NA"
    paste0("<tr><td>", esc(name), "</td><td>", esc(title),
           "</td><td>", esc(v), "</td><td>", esc(desc), "</td></tr>")
  }
  
  factor_order <- c("X1_mat_AP", "X2_relation_AP", "X3_var_sensi", "X4_var_ccr_AP",
                    "X5_var_act", "X6_typo_act", "X7_mat_sensi", "X8_var_hab",
                    "X9_norm_form", "X10_agreg_press")
  
  # ---- Section : Cartes interactives ----
  cartes_section <- paste0("
<div class='card'>
  <h2>Représentations cartographiques associées</h2>
  <div class='muted' style='margin-bottom:12px;font-size:13px;'>
    Leaflet interactif : navigable et zoomable.
  </div>",
                           if (has_leaflet) "<iframe src='carte_interactive.html'></iframe>"
                           else "<div class='muted'>Carte interactive non générée.</div>",
                           "
</div>")
  
  # ---- Section : Graphiques ----
  graphs_html <- if (length(graph_imgs) > 0) {
    paste0(vapply(graph_imgs, function(f)
      paste0("<div class='imgcard'><div class='muted'>", esc(f),
             "</div><img src='graphs/", esc(f), "' alt='", esc(f), "'></div>"),
      character(1)), collapse = "")
  } else "<div class='muted'>Aucun graphique disponible.</div>"
  
  graphs_section <- paste0("
<div class='card'>
  <h2>Graphiques</h2>",
                           if (dir.exists(graphs_dir)) file_link(graphs_dir, "Accès au dossier graphiques complet") else "",
                           "
  <div class='imggrid' style='margin-top:12px;'>", graphs_html, "</div>
</div>")
  
  # ---- Section : Synthèse Monte Carlo ----
  has_mc <- !is.null(res2) && is.list(res2)
  
  mc_section <- if (has_mc) {
    nsim       <- res2$nsim
    thres_stat <- res2$thres_stat
    factors    <- res2$factors
    
    mc_refc_mean    <- mean(res2$stats_simul$REFC_moy, na.rm = TRUE)
    mc_refc_sd_mean <- mean(res2$stats_simul$sd_REFC,  na.rm = TRUE)
    mc_refc_cv_mean <- mean(res2$stats_simul$cv_REFC,  na.rm = TRUE)
    
    factor_rows <- paste0(vapply(factor_order, fmt_factor, character(1), factors = factors),
                          collapse = "")
    
    paste0("
<div class='card'>
  <h2>Synthèse Monte Carlo</h2>
  <div class='muted' style='margin-bottom:12px;font-size:13px;'>
    Dispersion des REFC simulés suivant les facteurs d'incertitude ci-dessous.
    Si seuls des modalités fixées de facteurs déterministes sont activés, 
    toutes les simulations sont identiques (écart-type et coefficient de variation inter-simulations nuls).
  </div>
  <div class='kpi-grid'>",
           kpi_card("Nombre de simulations", esc(nsim)),
           kpi_card("REFC moyen des simulations", fmt_num(mc_refc_mean, 3)),
           kpi_card("Écart-type moyen inter-simulations", fmt_num(mc_refc_sd_mean, 3)),
           kpi_card("CV moyen inter-simulations", fmt_num(mc_refc_cv_mean, 3)),
           "
  </div>
  <div class='param-line' style='margin-top:14px;'>
  </div>
  <h3>Facteurs d'incertitude et modalités appliquées</h3>
  <table>
    <thead><tr><th>Facteur</th><th>Signification</th><th>Valeur</th><th>Modalité</th></tr></thead>
    <tbody>", factor_rows, "</tbody>
  </table>
</div>")
  } else {
    "<div class='card'><h2>Synthèse Monte Carlo</h2>
     <div class='muted'>Non exécuté pour cette analyse.</div></div>"
  }
  
  # ---- Section : Robustesse déterministe ----
  has_rob_det <- !is.null(res2b) && is.list(res2b) && length(res2b) > 0
  
  rob_det_section <- if (has_rob_det) {
    
    facteur_rows <- vapply(names(res2b), function(fact_name) {
      comp_df       <- res2b[[fact_name]]
      sp_list       <- attr(comp_df, "spearman")
      stable_list   <- attr(comp_df, "pct_rang_stable")
      ecart_list    <- attr(comp_df, "ecart_moyen")
      stab25_list   <- attr(comp_df, "stab_top25")
      ref_nm        <- attr(comp_df, "ref_scenario") %||% "ref"
      seuil         <- attr(comp_df, "seuil_stable_pct") %||% 5
      
      if (is.null(sp_list) || length(sp_list) == 0) return("")
      
      rows <- vapply(names(sp_list), function(nm) {
        sp    <- sp_list[[nm]]
        pstab <- stable_list[[nm]]
        eca   <- ecart_list[[nm]]
        s25   <- stab25_list[[nm]]
        
        p25 <- if (!is.null(s25)) s25$pct_retenu else NA_real_
        
        paste0(
          "<tr>",
          "<td>", esc(fact_name), "</td>",
          "<td>", esc(ref_nm), "</td>",
          "<td>", esc(nm), "</td>",
          "<td>", fmt_num(sp, 3), "</td>",
          "<td>", fmt_pct(pstab), "</td>",
          "<td>", fmt_num(eca, 3), "</td>",
          "<td>", fmt_pct(p25), "</td>",
          "</tr>"
        )
      }, character(1))
      paste(rows, collapse = "")
    }, character(1))
    
    paste0("
<div class='card'>
  <h2>Analyse de Robustesse Déterministe</h2>
  <div class='muted' style='margin-bottom:12px;font-size:13px;'>
    Effet de chaque choix méthodologique sur les résultats.
    Un Spearman proche de 1, un taux élevé de mailles stables en rang et
    une stabilité top 25% élevée indiquent que les conclusions sont
    robustes au choix du scénario.
  </div>
  <table>
    <thead>
      <tr>
        <th>Facteur</th>
        <th>Scénario référence</th>
        <th>Scénario comparé</th>
        <th>Spearman</th>
        <th>Mailles stables (rang, \u00b15%)</th>
        <th>Écart absolu moyen</th>
        <th>Stabilité top 25%</th>
      </tr>
    </thead>
    <tbody>", paste(facteur_rows, collapse = ""), "</tbody>
  </table>
  <div class='muted' style='margin-top:10px;font-size:12px;'>
    Spearman : corrélation de rang des REFC entre scénarios.
    Mailles stables (rang, ±5%) : part des mailles dont le rang change de
    moins de 5% de l'effectif total entre le scénario de référence et le
    scénario comparé — indicateur de robustesse du classement spatial.
    Écart absolu moyen : moyenne, sur toutes les mailles, de l'écart absolu
    entre REFC du scénario de référence et REFC du scénario comparé.
    Stabilité top 25% : part des mailles classées dans le top 25% du
    scénario de référence qui restent dans le top 25% du scénario comparé
    — traduit la fiabilité de la priorisation opérationnelle.
    GeoPackages détaillés dans outputs/robustesse_det/.
  </div>
</div>")
  } else ""
  
  # ---- Section : Robustesse stochastique ----
  has_rob_stoch <- !is.null(res2c) && is.list(res2c) && !is.null(res2c$synthese)
  
  rob_stoch_carte_html <- NULL
  if (has_rob_stoch && !is.null(res2c$geojson_path) &&
      file.exists(res2c$geojson_path)) {
    tryCatch({
      rob_stoch_carte_html <- write_leaflet_robustesse_html(
        geojson_path = res2c$geojson_path,
        outputsdir   = dirname(res2c$geojson_path) |> dirname()
      )
    }, error = function(e)
      warning("report_global_html: carte robustesse stochastique échouée : ",
              e$message))
  }
  
  rob_stoch_iframe <- if (!is.null(rob_stoch_carte_html) &&
                          file.exists(rob_stoch_carte_html)) {
    paste0("<iframe src='", esc(rel_or_file_uri(rob_stoch_carte_html)),
           "' style='width:100%;height:500px;",
           "border:1px solid #e5e7eb;border-radius:10px;margin-top:12px;'></iframe>")
  } else ""
  
  rob_stoch_section <- if (has_rob_stoch) {
    s <- res2c$synthese
    paste0("
<div class='card'>
  <h2>Analyse de Robustesse Stochastique</h2>
  <div class='muted' style='margin-bottom:12px;font-size:13px;'>
    Stabilité des résultats face aux facteurs d'incertitude stochastiques.
    Un Spearman proche de 1, un delta de rang faible et une stabilité top
    élevée indiquent que le classement des zones à risque est robuste aux
    perturbations.
  </div>
  <div class='kpi-grid'>",
           kpi_card("Spearman df/simulations", fmt_num(s$spearman_ref_vs_moy, 3)),
           kpi_card(paste0("Mailles stables (rang, \u00b1", esc(s$seuil_stable_pct %||% 5), "%)"),
                    fmt_pct(s$pct_rang_stable)),
           kpi_card("Écart abs moyen df/simulations", fmt_num(s$ecart_moy_ref_moyen, 3)),
           kpi_card("Stabilité top 25%", fmt_pct(s$pct_top25_stable)),
           "
  </div>
  <div class='muted' style='margin-top:10px;font-size:12px;'>
    Spearman : corrélation de rang entre REFC par défaut et moyenne des
    simulations. Delta de rang : écart, maille par maille, entre son rang de
    risque en référence (défaut) et son rang moyen dans les simulations,
    rapporté à l'effectif total de mailles (en %) — indicateur non-agrégé de
    la robustesse du classement spatial, visualisé sur la carte ci-dessous.
    Écart moyen df/simulations : moyenne, sur toutes les mailles, de l'écart
    absolu entre REFC de référence et REFC moyen simulé (indique l'ampleur
    du décalage indépendamment du rang). Stabilité top N % : part des
    mailles du top N % (défaut) restant dans le top N % de la distribution
    simulée moyenne.
  </div>",
           rob_stoch_iframe,
           "
</div>")
  } else ""
  
  # ---- Section : Sensibilité de Shapley ----
  has_shapley <- !is.null(res2d) && is.list(res2d) && !is.null(res2d$results)
  
  shapley_section <- if (has_shapley) {
    
    results_df <- res2d$results[order(res2d$results$rang), ]
    
    shapley_rows <- vapply(seq_len(nrow(results_df)), function(i) {
      r <- results_df[i, ]
      title <- factor_labels[[r$facteur]]$title %||% r$facteur
      paste0(
        "<tr>",
        "<td>", esc(r$rang), "</td>",
        "<td>", esc(r$facteur), "</td>",
        "<td>", esc(title), "</td>",
        "<td>", fmt_num(r$shapley, 4), "</td>",
        "<td>", fmt_pct(r$shapley_norm * 100, 1), "</td>",
        "</tr>"
      )
    }, character(1))
    
    barplot_html <- if (!is.null(res2d$png_path) && file.exists(res2d$png_path)) {
      paste0("<div class='imgcard' style='margin-top:14px;'><img src='",
             esc(rel_or_file_uri(res2d$png_path)),
             "' alt='Valeurs de Shapley par facteur'></div>")
    } else {
      "<div class='muted' style='margin-top:14px;'>Graphique non disponible.</div>"
    }
    
    paste0("
<div class='card'>
  <h2>Analyse de Sensibilité de Shapley</h2>
  <div class='muted' style='margin-bottom:12px;font-size:13px;'>
    Contribution relative de chaque facteur d'incertitude à la variabilité
    du classement spatial du risque (corrélation de Spearman du REFC simulé
    par rapport au REFC de référence). Plus la valeur de Shapley est élevée,
    plus le facteur pèse dans l'incertitude globale des résultats.
  </div>
  <div class='kpi-grid'>",
           kpi_card("Facteurs testés", esc(res2d$k %||% nrow(results_df))),
           kpi_card("Permutations (m)", esc(res2d$m)),
           kpi_card("Répétitions stochastiques (nrep)", esc(res2d$nrep)),
           "
  </div>",
           barplot_html,
           "
  <h3>Classement des facteurs par contribution</h3>
  <table>
    <thead>
      <tr><th>Rang</th><th>Facteur</th><th>Signification</th>
          <th>Shapley</th><th>Contribution normalisée</th></tr>
    </thead>
    <tbody>", paste(shapley_rows, collapse = ""), "</tbody>
  </table>
  <div class='muted' style='margin-top:10px;font-size:12px;'>
    Contribution normalisée : part de la valeur de Shapley absolue de chaque
    facteur dans la somme de tous les facteurs testés (somme = 100 %).
    Détails complets : outputs/sensibilite_shapley/.
  </div>
</div>")
  } else {
    "<div class='card'><h2>Analyse de Sensibilité de Shapley</h2>
     <div class='muted'>Non exécutée pour cette analyse.</div></div>"
  }
  
  # ---- CSS ----
  css <- "
:root{--accent:#96d1de;--bg:#f6f8fb;--card:#ffffff;--text:#0f172a;
      --muted:#64748b;--border:#e5e7eb;}
*{box-sizing:border-box;}
body{font-family:Arial,sans-serif;margin:0;color:var(--text);background:var(--bg);}
.container{max-width:1200px;margin:0 auto;padding:24px 18px 60px;}
.header{background:var(--card);border-bottom:1px solid var(--border);padding:18px 0;}
.header-inner{max-width:1200px;margin:0 auto;padding:0 18px;}
.title{font-size:34px;font-weight:800;margin:0 0 8px;letter-spacing:-.02em;}
.meta{color:var(--muted);display:flex;flex-wrap:wrap;gap:10px 18px;margin-top:6px;}
.badge{display:inline-block;background:rgba(150,209,222,.25);color:#0b3a44;
       border:1px solid rgba(150,209,222,.55);padding:3px 10px;border-radius:999px;
       font-size:12px;font-weight:700;}
.path{font-family:monospace;font-size:12px;color:var(--muted);word-break:break-all;}
.card{background:var(--card);border:1px solid var(--border);border-radius:14px;
      padding:20px 22px;margin:24px 0;box-shadow:0 6px 18px rgba(15,23,42,.04);}
h2{margin:0 0 14px;font-size:21px;letter-spacing:-.01em;
   padding-bottom:10px;border-bottom:2px solid rgba(150,209,222,.5);}
h3{margin:18px 0 10px;font-size:15px;color:#0b3a44;}
.muted{color:var(--muted);}
.kpi-grid{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:12px;}
@media(max-width:1100px){.kpi-grid{grid-template-columns:repeat(2,minmax(0,1fr));}}
@media(max-width:560px){.kpi-grid{grid-template-columns:1fr;}}
.kpi{border:1px solid var(--border);border-radius:14px;padding:12px 14px;
     background:linear-gradient(180deg,rgba(150,209,222,.12),rgba(150,209,222,.04));}
.kpi .label{font-size:12px;color:var(--muted);margin-bottom:6px;}
.kpi .value{font-size:26px;font-weight:800;}
.param-line{display:flex;align-items:center;gap:8px;margin:8px 0;font-size:14px;}
.param-line b{min-width:160px;}
.imggrid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px;}
@media(max-width:1100px){.imggrid{grid-template-columns:1fr;}}
.imgcard{border:1px solid var(--border);border-radius:14px;padding:10px;background:var(--card);}
.imgcard img{max-width:100%;height:auto;display:block;border-radius:10px;}
table{border-collapse:collapse;width:100%;margin-top:6px;}
th,td{border:1px solid var(--border);padding:10px;text-align:left;font-size:13px;}
th{background:#f1f5f9;}
tr:nth-child(even) td{background:#fbfdff;}
iframe{width:100%;height:650px;border:1px solid var(--border);border-radius:14px;margin-top:8px;}
.file div,.file a{font-size:13px;margin:4px 0;}
.file.missing span.muted{font-style:italic;}
"
  
  # ---- Assemblage HTML ----
  html <- paste0(
    "<!doctype html>
<html lang='fr'>
<head>
<meta charset='utf-8'>
<meta name='viewport' content='width=device-width,initial-scale=1'>
<title>Rapport CARPEDIEM</title>
<style>", css, "</style>
</head>
<body>

<div class='header'>
  <div class='header-inner'>
    <div class='title'>Rapport d'analyse CARPEDIEM</div>
    <div class='meta'>
      <span class='badge'>Date</span> <span>", esc(ts), "</span>
      <span class='badge'>Résultats</span>
      <span class='path'>", esc(run_dir_disp), "</span>
    </div>
  </div>
</div>

<div class='container'>

<div class='card'>
  <h2>Analyse de risque par défaut</h2>
  <div class='kpi-grid'>",
    kpi_card("REFC moyen (risque d'effet)", fmt_num(refc_moy, 3)),
    kpi_card("REXC moyen (risque d'exposition)", fmt_num(rexc_moy, 3)),
    kpi_card("IC données moyen", fmt_num(ic_moy, 3)),
    kpi_card("Nb total Mailles", esc(n_mailles)),
    kpi_card("Nb total Activités", esc(n_act)),
    kpi_card("Nb total Pressions", esc(n_pr)),
    kpi_card("Nb moyen Activités par maille", fmt_num(ima_moy)),
    kpi_card("Nb moyen Pressions par maille", fmt_num(ipc_moy)),
    "
  </div>
</div>

", cartes_section, "
", graphs_section, "
", mc_section, "
", rob_det_section, "
", rob_stoch_section, "
", shapley_section, "

</div>
</body>
</html>")
  
  # ---- Écriture ----
  out_name <- trimws(as.character(
    config$report$output_file %||% "report.html"))
  out_html <- file.path(run_dir, out_name)
  writeLines(html, out_html, useBytes = TRUE)
  message("Rapport HTML généré : ", out_html)
  
  # ---- run_info.json ----
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    tryCatch(jsonlite::write_json(
      list(run_dir    = run_dir,
           report_html = out_html,
           timestamp   = ts),
      file.path(run_dir, "run_info.json"),
      auto_unbox = TRUE, pretty = TRUE),
      error = function(e)
        warning("Impossible d'écrire run_info.json : ", e$message))
  }
  
  invisible(out_html)
}
